/*
 * $Id$
 *
 * Copyright (c) 2000-2010 by Rodney Kinney, Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;

import VASSAL.build.GameModule;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.launch.Launcher;
import VASSAL.preferences.Prefs;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.image.svg.SVGImageUtils;
import VASSAL.tools.io.FileUtils;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.nio.file.FileSystems;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.Paths;
import VASSAL.tools.nio.file.zipfs.ZipFileSystem;

/**
 * An ArchiveWriter is a writeable DataArchive. New files may be added
 * with the {@link #addFile} and {@link #addImage} methods.
 */
public class ArchiveWriter extends DataArchive {
  private boolean isTempArchive = false;

  /**
   * Create a new writeable archive.
   * 
   * @param zipName the name of the archive. If null, the user will be
   * prompted for a filename when saving. If not null, new entries will
   * be added to the named archive. If the file exists and is not a zip
   * archive, it will be overwritten.
   */
  public ArchiveWriter(String zipName) {
    isTempArchive = zipName == null;

    String archiveName = zipName;
    try {
      if (isTempArchive) {
// FIXME: This creates an odd situation, in which we need the file not to
// exist, and to continue not existing, but we have no guarantee that it
// won't be created in /tmp!
        final File f = File.createTempFile("tmp", ".zip");
        f.delete();
        archiveName = f.getAbsolutePath();
      }

      final URI uri = URIUtils.toURI("zip", new File(archiveName));
      archive = FileSystems.newFileSystem(uri, zipOpts);
    }
    catch (IOException e) {
// FIXME: Setting archive to null is wrong. This ctor needs to throw instead.
      archive = null;
      WriteErrorDialog.error(e, archiveName);
    }
  }
 
  /**
   * Add an image file to the archive. The file will be copied into an
   * "images" directory in the archive. Storing another image with the
   * same name will overwrite the previous image.
   * 
   * @param path the full path of the image file on the user's filesystem
   * @param name the name under which to store the image in the archive
   */
  public void addImage(String path, String name) {
    // check SVG for external references and pull them in
    if (name.toLowerCase().endsWith(".svg")) {
      for (String s : SVGImageUtils.getExternalReferences(path)) {
        final File f = new File(s);

        try {
          addFile(imageDir + f.getName(),
                  SVGImageUtils.relativizeExternalReferences(s));
        }
        catch (IOException e) {
          ReadErrorDialog.error(e, f);
        }
      }
    }
    // otherwise just add what we were given
    else {
      addFile(path, imageDir + name);
    }

    Op.load(name).update();
    localImages = null;
  }

  public void addImage(String name, byte[] contents) {
    addFile(imageDir + name, contents);
    localImages = null;
  } 
  
  public void addSound(String path, String fileName) {
    addFile(path, soundDir + fileName);
  }

  public void removeImage(String name) {
    try {    
      final Path file = archive.getPath(imageDir + name);
      file.deleteIfExists();
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, ((ZipFileSystem) archive).getFileSystemPath().toString());
    }

    localImages = null;
  }
 
  /**
   * Copy a file from the user's filesystem to the archive.
   * 
   * @param path the full path of the file on the user's filesystem
   * @param fileName the name under which to store the file in the archive
   */
  public void addFile(String path, String fileName) {

    try {
      final Path src = Paths.get(path);
      final Path dst = archive.getPath(fileName);

      src.copyTo(dst);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, ((ZipFileSystem) archive).getFileSystemPath().toString());
    }
  }

  /**
   * Copy an <code>InputStream</code> into the archive
   * 
   * @param fileName the name under which to store the contents of the stream
   * @param in the stream to copy
   */
  public void addFile(String fileName, InputStream in) {
    OutputStream out = null;
    try {
      final Path dst = archive.getPath(fileName);
      out = dst.newOutputStream();
      IOUtils.copy(in, out);
      out.close();
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, ((ZipFileSystem) archive).getFileSystemPath().toString());
    }
    finally {
      IOUtils.closeQuietly(out);
    }
  }

  public void addFile(String fileName, byte[] content) {
    addFile(fileName, new ByteArrayInputStream(content));
  }

  public void save() throws IOException {
    save(false);
  }

  public void save(boolean notifyModuleManager) throws IOException {
    if (isTempArchive) saveAs(notifyModuleManager);
    else write((ZipFileSystem) archive, notifyModuleManager);
  }

  public void saveAs() throws IOException {
    saveAs(false);
  }

  protected void write(ZipFileSystem fs, boolean notifyModuleManager)
                                                           throws IOException {
    fs.flush();
    
    // FIXME: use a listener here?
    if (notifyModuleManager) {
      Launcher.getInstance().sendSaveCmd(new File(fs.getFileSystemPath().toString()));
    }
  }

  public void saveAs(boolean notifyModuleManager) throws IOException {
    final FileChooser fc = FileChooser.createFileChooser(
      GameModule.getGameModule().getFrame(),
      (DirectoryConfigurer) Prefs.getGlobalPrefs()
                                 .getOption(Prefs.MODULES_DIR_KEY));
    if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return;
    final String filename = fc.getSelectedFile().getPath();

    final Path zold = ((ZipFileSystem) archive).getFileSystemPath();
    final Path znew = Paths.get(filename);

    if (!zold.isSameFile(znew)) {
      final URI uri = URIUtils.toURI("zip", new File(filename)); 
      final ZipFileSystem tmpArchive =
        (ZipFileSystem) FileSystems.newFileSystem(uri, zipOpts);

      FileUtils.copy(archive.getPath("/"), tmpArchive.getPath("/"));

      ((ZipFileSystem) archive).revert();
      archive.close();

      if (isTempArchive) isTempArchive = false;        

      archive = tmpArchive;
    } 

    write((ZipFileSystem) archive, notifyModuleManager);
  }

  @Deprecated
  public void write() throws IOException {
    write(false);
  }

  @Deprecated  
  public void write(boolean notifyModuleManager) throws IOException {
    save(notifyModuleManager);
  }
}
