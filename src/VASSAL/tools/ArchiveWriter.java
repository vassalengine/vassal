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
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.SortedSet;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import VASSAL.build.GameModule;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.launch.Launcher;
import VASSAL.preferences.Prefs;
import VASSAL.tools.IOUtils;
import VASSAL.tools.filechooser.FileChooser;

/**
 * An ArchiveWriter is a writeable DataArchive. New files may be added
 * with the {@link #addFile} and {@link #addImage} methods.
 */
public class ArchiveWriter extends DataArchive {
  private final Map<String,Object> images = new HashMap<String,Object>();
  private final Map<String,Object> sounds = new HashMap<String,Object>();
  private final Map<String,Object> files = new HashMap<String,Object>();
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
      try {
        archive = new ZipFile(archiveName);
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, archiveName);
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
        images.put(imageDir + n, f.getPath());
      }
    }
    // otherwise just add what we were given
    else {
//      ImageCache.remove(new SourceOp(name));
      images.put(IMAGE_DIR + name, path);
    }

    localImages = null;
  }

  public void addImage(String name, byte[] contents) {
//    ImageCache.remove(new SourceOp(name));
    images.put(imageDir + name, contents);
    localImages = null;
  } 
  
  public void addSound(String file, String name) {
    sounds.put(SOUNDS_DIR + name, file);
  }

  public boolean isImageAdded(String name) {
    return images.containsKey(name);
  }

  public void removeImage(String name) {
//    ImageCache.remove(new SourceOp(name));
    images.remove(imageDir + name);
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

  /**
   * Overrides {@link DataArchive#getFileStream} to return streams that have
   * been added to the archive but not yet written to disk.
   */
  @Override
  public InputStream getFileStream(String fileName) throws IOException {
    if (closeWhenNotInUse && archive == null && archiveName != null) {
      archive = new ZipFile(archiveName);
    }

    InputStream in = getAddedStream(images, fileName);
    if (in != null) return in;
    in = getAddedStream(files, fileName);
    if (in != null) return in;
    in = getAddedStream(sounds, fileName);
    if (in != null) return in;

    if (archive != null) return super.getFileStream(fileName);

    throw new FileNotFoundException(fileName + " not found");
  }

  private InputStream getAddedStream(Map<String,Object> table, String fileName)
                                                           throws IOException {
    final Object file = table.get(fileName);
    if (file instanceof String) {
      return new FileInputStream((String) file);
    }
    else if (file instanceof byte[]) {
      return new ByteArrayInputStream((byte[]) file);
    }
    else {
      return null;
    }
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
      
      if (closeWhenNotInUse && archive == null && archiveName != null) {
        try {
          archive = new ZipFile(archiveName);
        }
        catch (IOException e) {
          // Not an error, either the archive doesn't exist yet,
          // or it isn't a ZIP file and we're going to overwrite it.
          // In the case that this is a bona fide I/O error, we'll
          // surely hit it again when we try to write later, so it
          // can be ignored here.
        }
      }

      if (archive != null) {
        // Copy old unmodified entries into temp file
        final byte[] buf = new byte[8192];

        ZipInputStream in = null;
        try {
          in = new ZipInputStream(
                new BufferedInputStream(
                  new FileInputStream(archive.getName())));

          ZipEntry entry = null;
          while ((entry = in.getNextEntry()) != null) {
            // skip modified or new entries
            if (images.containsKey(entry.getName()) || 
                sounds.containsKey(entry.getName()) ||
                files.containsKey(entry.getName())) continue;

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
      writeEntries(images, ZipEntry.STORED,   out);
      writeEntries(sounds, ZipEntry.STORED,   out);
      writeEntries(files,  ZipEntry.DEFLATED, out);

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

    if (notifyModuleManager) {
      Launcher.getInstance().sendSaveCmd(original);
    }
    
    if (!closeWhenNotInUse) {
      archive = new ZipFile(archiveName);
    }
  }

  private void writeEntries(Map<String,Object> h, int method,
                            ZipOutputStream out) throws IOException {
    for (String name : h.keySet()) {
      final Object o = h.get(name);

      final ZipEntry entry = new ZipEntry(name);
      entry.setMethod(method);

      if (o instanceof String && !name.toLowerCase().endsWith(".svg")) {
        FileInputStream in = null;
        try {
          // o is a filename 
          in = new FileInputStream((String) o);

          final byte[] buf = new byte[8192];

          if (method == ZipEntry.STORED) {
            // find the checksum
            final CRC32 checksum = new CRC32();
            
            int count = 0;
            int n = 0;
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
  }

  public SortedSet<String> getImageNameSet() {
    final SortedSet<String> s = super.getImageNameSet();
    for (String name : images.keySet()) {
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
    final SortedSet<String> s = super.setOfImageNames();
    for (String name : images.keySet()) {
      if (name.startsWith(imageDir)) {
        s.add(name.substring(imageDir.length()));
      }
    }
    return s;
  }

  /** @deprecated Use {@link #setofImageNames()} instead. */
  @Deprecated
  @SuppressWarnings("unchecked")
  protected void listImageNames(Collection v) {
    v.addAll(setOfImageNames());
  }
}
