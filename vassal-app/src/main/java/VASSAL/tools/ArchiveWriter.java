/*
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
import java.nio.file.Files;
import java.util.List;
import java.util.zip.ZipFile;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.image.svg.SVGImageUtils;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.ZipArchive;
import org.apache.commons.lang3.StringUtils;

import javax.swing.JOptionPane;

/**
 * An ArchiveWriter is a writeable DataArchive. New files may be added
 * with the {@link #addFile} and {@link #addImage} methods. {@link #save()} and {@link #saveAs()} will
 * cause the archive to be written out, with FileChooser invoked if appropriate.
 */
public class ArchiveWriter extends DataArchive {
  private String archiveName;
  private String defaultExtension;
  private boolean isTempArchive = false;

  /**
   * Create a new writeable archive.
   *
   * @param zipName the name of the archive. If null, the user will be
   * prompted for a filename when saving. If not null, new entries will
   * be added to the named archive. If the file exists and is not a zip
   * archive, it will be overwritten.
   * @param defaultExtension the default file extension for the archive.
   * If non-null, and the user needs to be prompted for a filename, this will
   * be the default file extension added automatically.
   */
  public ArchiveWriter(String zipName, String defaultExtension) {
    archiveName = zipName;
    this.defaultExtension = defaultExtension;

    if (archiveName == null) {
      isTempArchive = true;

      if (this.defaultExtension == null) {
        this.defaultExtension = ".zip";  //NON-NLS
      }

      try {
        archiveName = Files.createTempFile(Info.getTempDir().toPath(), "tmp_", this.defaultExtension).toAbsolutePath().toString();  //NON-NLS
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

  public ArchiveWriter(FileArchive archive, String defaultExtension) {
    archiveName = archive.getName();
    this.defaultExtension = defaultExtension;
    this.archive = archive;
  }

  /**
   * Create a new writeable archive.
   *
   * @param zipName the name of the archive. If null, the user will be
   * prompted for a filename when saving. If not null, new entries will
   * be added to the named archive. If the file exists and is not a zip
   * archive, it will be overwritten.
   */
  public ArchiveWriter(String zipName) {
    this(zipName, null);
  }

  public ArchiveWriter(FileArchive archive) {
    this(archive, null);
  }

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public ArchiveWriter(ZipFile archive) {
    ProblemDialog.showDeprecated("2020-08-06");
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
    if (name.toLowerCase().endsWith(".svg")) { //$NON-NLS-1$//
      final List<String> exrefs;
      try {
        exrefs = SVGImageUtils.getExternalReferences(path);
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, name);
        return;
      }

      for (final String s : exrefs) {
        final File f = new File(s);

        final byte[] buf;
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

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public boolean isImageAdded(String name) {
    ProblemDialog.showDeprecated("2020-08-06");
    try {
      return archive.contains(imageDir + name);
    }
    catch (IOException e) {
      return false;
    }
  }

  public void removeImage(String name) {
    removeFile(imageDir + name);
    localImages = null;
  }

  /**
   * Removes a file in the archive
   * @param name file in the archive to be removed
   */
  public void removeFile(String name) {
    try {
      archive.remove(name);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, archive.getName());
    }
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
    try (OutputStream out = archive.getOutputStream(fileName)) {
      in.transferTo(out);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, archive.getName());
    }
  }

  /**
   * Copy am array of <code>bytes</code> into the archive
   *
   * @param fileName the name under which to store the contents of the stream
   * @param content array of bytes to copy
   */
  public void addFile(String fileName, byte[] content) {
    try {
      archive.add(fileName, content);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, archive.getName());
    }
  }

  /**
   * Saves the archive, prompting for a name only if none has ever been provided.
   * @throws IOException IOException
   */
  public boolean save() throws IOException {
    return save(false);
  }

  /**
   * Saves the archive, prompting for a name only if none has ever been provided.
   * @param notifyModuleManager If true, notifies Module Manager that the save has occurred
   * @throws IOException IOException
   */
  public boolean save(boolean notifyModuleManager) throws IOException {
    if (isTempArchive) {
      return saveAs(notifyModuleManager);
    }
    else {
      write(archive, notifyModuleManager);
    }
    return true;
  }

  /**
   * Saves the archive, always prompting for a new filename.
   * @throws IOException IOException
   */
  public boolean saveAs() throws IOException {
    return saveAs(false);
  }

  /**
   * Writes the file archive.
   * @param fa File archive
   * @param notifyModuleManager if true, notifies the module manager that a file has been saved.
   * @throws IOException IOException
   */
  protected void write(FileArchive fa, boolean notifyModuleManager)
                                                           throws IOException {
    fa.flush();
  }

  /**
   * Saves the archive, always prompting for a new filename. If a defaultExtension has been
   * provided, it will be added to the filename unless the user specifies a different one explicitly.
   * @param notifyModuleManager If true, notifies Module Manager that the save has occurred
   * @return true if operation proceeded, false if it was cancelled by user at file chooser or confirmation dialog
   * @throws IOException IOException
   */
  public boolean saveAs(boolean notifyModuleManager) throws IOException {
    final FileChooser fc = FileChooser.createFileChooser(
      GameModule.getGameModule().getPlayerWindow(),
      (DirectoryConfigurer) Prefs.getGlobalPrefs()
                                 .getOption(Prefs.MODULES_DIR_KEY));
    if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return false;
    String filename = fc.getSelectedFile().getPath();

    if (!StringUtils.isEmpty(defaultExtension) && (filename.lastIndexOf('.') < 0)) {
      filename = filename + defaultExtension;
      if (new File(filename).exists() && JOptionPane.NO_OPTION == JOptionPane.showConfirmDialog(GameModule.getGameModule().getPlayerWindow(), Resources.getString("Editor.ArchiveWriter.overwrite", filename), Resources.getString("Editor.ArchiveWriter.file_exists"), JOptionPane.YES_NO_OPTION)) {
        return false;
      }
    }

    if (!filename.equals(archive.getName())) {
      // Copy the current state to the new archive.
      final FileArchive tmp = archive;

      archiveName = filename; // Set archive name before we open the new one, so if we get an exception we can complain using specified new filename
      archive = new ZipArchive(tmp, filename);
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

    return true;
  }

  /**
   * If the ArchiveWriter was initialized with non-null file name, then
   * write the contents of the archive to the named archive. If it was
   * initialized with a null name, prompt the user to select a new file
   * into which to write archive.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void write() throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    write(false);
  }

  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void write(boolean notifyModuleManager) throws IOException {
    ProblemDialog.showDeprecated("2020-08-06");
    save(notifyModuleManager);
  }
}
