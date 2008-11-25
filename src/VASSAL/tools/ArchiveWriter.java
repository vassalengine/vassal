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

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.SortedSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import VASSAL.build.GameModule;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.launch.Launcher;
import VASSAL.preferences.Prefs;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.image.SVGImageUtils;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.io.IOUtils;

/**
 * An ArchiveWriter is a writeable DataArchive. New files may be added
 * with the {@link #addFile} and {@link #addImage} methods.
 */
public class ArchiveWriter extends DataArchive {
  private final Map<String,Object> files =
    new ConcurrentHashMap<String,Object>();
  
  // List of existing images removed by changing the name of Game Piece Images
  private final ArrayList<String> removedImages = new ArrayList<String>();
  
  private String archiveName;
  private boolean closeWhenNotInUse;

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
    closeWhenNotInUse = false;

    if (archiveName != null) {
      final File f = new File(archiveName);
      if (f.exists()) {
        try {
          archive = new ZipFile(f);
        }
        catch (IOException e) {
// FIXME: This isn't quite right, e.g., in the case where the user intends
// to overwrite the given file, but it isn't a zip file. How to distinguish
// between that situation and a bona fide I/O error?
          ReadErrorDialog.error(e, archiveName);
        }
      }
    }
  }
  
  public ArchiveWriter(String zipName, boolean keepClosed) {
    this(zipName);
    closeWhenNotInUse();
  }
  
  public ArchiveWriter(ZipFile archive) {
    this.archive = archive;
    archiveName = archive.getName();
  }

  /**
   * Close the archive, but keep it available for reuse. Used for Preferences
   * files which must be shared between processes.
   */
  public void closeWhenNotInUse() {
    closeWhenNotInUse = true;

    if (archive != null) {
      try {
        archive.close();
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, archive.getName());
      }

      archive = null;
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
        final String n = f.getName();
// FIXME: this isn't right---n might not be a displaying SVG image
//        ImageCache.remove(new SourceOp(n));         
        files.put(imageDir + n, f.getPath());
      }
    }
    // otherwise just add what we were given
    else {
//      ImageCache.remove(new SourceOp(name));
      files.put(imageDir + name, path);
    }

    Op.load(name).update();
    localImages = null;
  }

  public void addImage(String name, byte[] contents) {
    files.put(imageDir + name, contents);
    removedImages.remove(imageDir + name);
    localImages = null;
  } 
  
  public void addSound(String path, String fileName) {
    addFile(path, soundDir + fileName);
  }

  public boolean isImageAdded(String name) {
    return files.containsKey(imageDir + name);
  }

  public void removeImage(String name) {
//    ImageCache.remove(new SourceOp(name));
    final String path = imageDir + name;
    
    files.remove(path);
    if (!removedImages.contains(path)) {
      removedImages.add(path);
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
    files.put(fileName, path);
  }

  /**
   * Copy an <code>InputStream</code> into the archive
   * 
   * @param fileName the name under which to store the contents of the stream
   * @param in the stream to copy
   */
  public void addFile(String fileName, InputStream in) {
    try {
      files.put(fileName, IOUtils.getBytes(in));
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, fileName);
    }
  }

  public void addFile(String fileName, byte[] content) {
    files.put(fileName, content);
  }

  private void openIfClosed() throws IOException {
    if (closeWhenNotInUse && archive == null && archiveName != null) {
      archive = new ZipFile(archiveName);
    }
  }

  /**
   * Overrides {@link DataArchive#getInputStream(String)} to return streams
   * that have been added to the archive but not yet written to disk.
   */
  @Override
  public InputStream getInputStream(String fileName)
                                    throws IOException, FileNotFoundException {
    final Object o = files.get(fileName);
    if (o instanceof String) {
      return new FileInputStream((String) o);
    }
    else if (o instanceof byte[]) {
      return new ByteArrayInputStream((byte[]) o);
    }

    openIfClosed();
    if (archive != null) return super.getInputStream(fileName);

    throw new FileNotFoundException(fileName + " not found");
  }
 
  @Override
  public URL getURL(String fileName) throws IOException, FileNotFoundException {
    final Object o = files.get(fileName);
    if (o instanceof String) {
      return new URL("file", null, (String) o);
    }

    openIfClosed();
    if (archive != null) return super.getURL(fileName);

    throw new FileNotFoundException(fileName + " not found");
  }

  public void saveAs() throws IOException {
    saveAs(false);
  }
  
  public void saveAs (boolean notifyModuleManager) throws IOException {
    archiveName = null;
    write(notifyModuleManager);
  }

  /**
   * If the ArchiveWriter was initialized with non-null file name, then
   * write the contents of the archive to the named archive. If it was
   * initialized with a null name, prompt the user to select a new file
   * into which to write archive.
   */
  public void write() throws IOException {
    write(false);
  }
  
  public void write(boolean notifyModuleManager) throws IOException {
    if (archiveName == null) {
      final FileChooser fc = FileChooser.createFileChooser(
        GameModule.getGameModule().getFrame(),
        (DirectoryConfigurer) Prefs.getGlobalPrefs()
                                   .getOption(Prefs.MODULES_DIR_KEY));
      if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return;
      archiveName = fc.getSelectedFile().getPath();
    }
    
    String temp = new File(archiveName).getParent();
    temp = temp == null ? "temp" : temp + File.separator + "temp";
    int n = 1;
    while ((new File(temp + n + ".zip")).exists()) {
      n++;
    }
    temp += n + ".zip";

    ZipOutputStream out = null;
    try {
      out = new ZipOutputStream(
              new BufferedOutputStream(
                new FileOutputStream(temp)));
      out.setLevel(9);

      try {
        openIfClosed();
      }
      catch (IOException e) {
        // Not an error, either the archive doesn't exist yet,
        // or it isn't a ZIP file and we're going to overwrite it.
        // In the case that this is a bona fide I/O error, we'll
        // surely hit it again when we try to write later, so it
        // can be ignored here.
      }

      final byte[] buf = new byte[8192];

      if (archive != null) {
        // Copy old unmodified entries into temp file
        ZipInputStream in = null;
        try {
          in = new ZipInputStream(
                new BufferedInputStream(
                  new FileInputStream(archive.getName())));

          ZipEntry entry = null;

          while ((entry = in.getNextEntry()) != null) {
            // skip modified or new entries
            final String name = entry.getName();

            // Skip entries that have been added this editing session
            if (files.containsKey(name)) continue;

// FIXME: modify this so that removed images are in files, but with a
// null value? (Simpler.)

            // Skip images removed via Game Piece Image name changes
            if (removedImages.contains(name)) continue;

            if (entry.getMethod() == ZipEntry.DEFLATED) {
              // we can't reuse entries for compressed files
              // because there's no way to reset the fields
              // to acceptable values
              entry = new ZipEntry(name);
            }

            // write out unmodified entries 
            out.putNextEntry(entry);
            IOUtils.copy(in, out, buf);
          }
          in.close();
        }
        finally {
          IOUtils.closeQuietly(in);
        }
      }

      // Write new entries into temp file
      for (String name : files.keySet()) {
        final Object o = files.get(name);

        final ZipEntry entry = new ZipEntry(name);

        final int method;
        if (name.startsWith(imageDir) || name.startsWith(soundDir)) {
          method = ZipEntry.STORED;
        }
        else {
          method = ZipEntry.DEFLATED;
        }

        entry.setMethod(method);

        if (o instanceof String && !name.toLowerCase().endsWith(".svg")) {
          FileInputStream in = null;
          try {
            // o is a filename 
            in = new FileInputStream((String) o);

            if (method == ZipEntry.STORED) {
              // find the checksum
              final CRC32 checksum = new CRC32();
          
              int count = 0;
              n = 0;
              try {
                while ((n = in.read(buf)) > 0) {
                  checksum.update(buf, 0, n);
                  count += n;
                }
                in.close();
              }
              finally {
                IOUtils.closeQuietly(in);
              }

              entry.setSize(count);
              entry.setCrc(checksum.getValue());

              // reset the stream
              in = new FileInputStream((String) o);
            }

            out.putNextEntry(entry);
            IOUtils.copy(in, out, buf);
            in.close();
          }
          finally {
            IOUtils.closeQuietly(in);
          }
        }
        else {
          byte[] contents;

          if (o instanceof String) {
            contents = SVGImageUtils.relativizeExternalReferences((String) o);
          }
          else if (o instanceof byte[]) {
            contents = (byte[]) o;
          }
          else {
            // unrecognized type
            try {
              throw new IllegalStateException(
                "Entry '" + name + "' is of an unrecognized type.");
            }
            catch (IllegalStateException e) {
              // Note: we catch here and continue becuase the user will
              // appreciate being able to save whatever he can.
              ErrorDialog.bug(e);
            }

            continue;
          }

          if (method == ZipEntry.STORED) {
            entry.setSize(contents.length);
            final CRC32 checksum = new CRC32();
            checksum.update(contents);
            entry.setCrc(checksum.getValue());
          }
      
          out.putNextEntry(entry);
          out.write(contents);
        }
      }

      out.close();
    }
    finally {
      IOUtils.closeQuietly(out);
    }

    if (archive != null) {
      archive.close();
      archive = null;
    }

    final File original = new File(archiveName);
    if (original.exists() && !original.delete()) {
      throw new IOException("Unable to overwrite " + archiveName +
                            "\nData stored in " + temp);
    }

    final File f = new File(temp);
    if (!f.renameTo(original)) {
      throw new IOException("Unable to write to " + archiveName +
                            "\nData stored in " + temp);
    }

// FIXME: use a listener here?
    if (notifyModuleManager) {
      Launcher.getInstance().sendSaveCmd(original);
    }
    
    if (!closeWhenNotInUse) {
      archive = new ZipFile(archiveName);
    }
  }

  public SortedSet<String> getImageNameSet() {
    final SortedSet<String> s = super.getImageNameSet();

    for (String name : files.keySet()) {
      if (name.startsWith(imageDir)) {
        s.add(name.substring(imageDir.length()));
      }
    }

    return s;
  }

  /**
   * Ensure the specified Zip archive exists. Create it and the specified
   * entry if it does not.
   * 
   * @param archiveName Archive file
   * @param entryName Entry Name
   */ 
  public static void ensureExists(File archiveFile, String entryName)
      throws IOException {

    if (archiveFile.exists()) return;

    ZipOutputStream out = null;
    try {
      out = new ZipOutputStream(
              new BufferedOutputStream(new FileOutputStream(archiveFile)));
      out.putNextEntry(new ZipEntry(entryName));
      out.close();
    }
    finally {
      IOUtils.closeQuietly(out);
    }
  }

  /** @deprecated Use {@link getImageNameSet()} instead. */
  @Deprecated
  protected SortedSet<String> setOfImageNames() {
    return getImageNameSet();
  }

  /** @deprecated Use {@link #setofImageNames()} instead. */
  @Deprecated
  @SuppressWarnings("unchecked")
  protected void listImageNames(Collection v) {
    v.addAll(setOfImageNames());
  }

  /**
   * Overrides {@link DataArchive#getInputStream(String)} to return streams
   * that have been added to the archive but not yet written to disk.
   *
   * @deprecated Use {@link #getInputStream(String)} instead.
   */
  @Deprecated
  @Override
  public InputStream getFileStream(String fileName) throws IOException {
    return getInputStream(fileName);
  }
}
