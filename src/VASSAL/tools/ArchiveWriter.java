/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
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

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.zip.ZipFile;

import VASSAL.build.GameModule;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.launch.Launcher;
import VASSAL.preferences.Prefs;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.image.svg.SVGImageUtils;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.ZipArchive;

/**
 * An ArchiveWriter is a writeable DataArchive. New files may be added
 * with the {@link #addFile} and {@link #addImage} methods.
 */
public class ArchiveWriter extends DataArchive {
  private String archiveName;
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
    archiveName = zipName;

    if (archiveName == null) {
      isTempArchive = true;
      try {
        archiveName = File.createTempFile("tmp", ".zip").getPath();
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, archiveName);
      }
    }

    final File f = new File(archiveName);
    try {
      if (f.exists()) {
        try {
          archive = new ZipArchive(archiveName);
        }
        catch (IOException e1) {
          // the file is not a valid ZIP archive, truncate it
          archive = new ZipArchive(archiveName, true);
        }
      }
      else {
        archive = new ZipArchive(archiveName);
      }
    }
    catch (IOException e) {
      archive = null;
      WriteErrorDialog.error(e, archiveName);
    }
  }

  public ArchiveWriter(FileArchive archive) {
    archiveName = archive.getName();
    this.archive = archive;
  }

  @Deprecated
  public ArchiveWriter(ZipFile archive) {
    archiveName = archive.getName();
    try {
      this.archive = new ZipArchive(archiveName);
    }
    catch (IOException e) {
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
      List<String> exrefs = null;
      try {
        exrefs = SVGImageUtils.getExternalReferences(path);
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, name);
        return;
      }

      for (String s : exrefs) {
        final File f = new File(s);

        byte[] buf = null;
        try {
          buf = SVGImageUtils.relativizeExternalReferences(s);
        }
        catch (IOException e) {
          ReadErrorDialog.error(e, f);
          continue;
        }

        addFile(imageDir + f.getName(), buf);
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

  @Deprecated
  public boolean isImageAdded(String name) {
    try {
      return archive.contains(imageDir + name);
    }
    catch (IOException e) {
      return false;
    }
  }

  public void removeImage(String name) {
    try {
      archive.remove(imageDir + name);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, archive.getName());
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
      archive.add(fileName, path);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, archive.getName());
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
      out = archive.getOutputStream(fileName);
      IOUtils.copy(in, out);
      out.close();
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, archive.getName());
    }
    finally {
      IOUtils.closeQuietly(out);
    }
  }

  public void addFile(String fileName, byte[] content) {
    try {
      archive.add(fileName, content);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, archive.getName());
    }
  }

  public void save() throws IOException {
    save(false);
  }

  public void save(boolean notifyModuleManager) throws IOException {
    if (isTempArchive) saveAs(notifyModuleManager);
    else write(archive, notifyModuleManager);
  }

  public void saveAs() throws IOException {
    saveAs(false);
  }

  protected void write(FileArchive fa, boolean notifyModuleManager)
                                                           throws IOException {
    fa.flush();

    // FIXME: use a listener here?
    if (notifyModuleManager) {
      Launcher.getInstance().sendSaveCmd(fa.getFile());
    }
  }

  public void saveAs(boolean notifyModuleManager) throws IOException {
    final FileChooser fc = FileChooser.createFileChooser(
      GameModule.getGameModule().getFrame(),
      (DirectoryConfigurer) Prefs.getGlobalPrefs()
                                 .getOption(Prefs.MODULES_DIR_KEY));
    if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return;
    final String filename = fc.getSelectedFile().getPath();

    if (filename != archive.getName()) {
      // Copy the current state to the new archive.
      final FileArchive tmp = archive;

      archive = new ZipArchive(tmp, filename);
      archiveName = filename;
      archive.flush();

      tmp.revert();
      tmp.close();

      write(archive, notifyModuleManager);

      if (isTempArchive) {
        tmp.getFile().delete();
        isTempArchive = false;
      }
    }
    else {
      write(archive, notifyModuleManager);
    }
  }

  /**
   * If the ArchiveWriter was initialized with non-null file name, then
   * write the contents of the archive to the named archive. If it was
   * initialized with a null name, prompt the user to select a new file
   * into which to write archive.
   */
  @Deprecated
  public void write() throws IOException {
    write(false);
  }

  @Deprecated
  public void write(boolean notifyModuleManager) throws IOException {
    save(notifyModuleManager);
  }
}
