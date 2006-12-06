/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;
import VASSAL.build.module.Documentation;
import VASSAL.tools.FileChooser;

/**
 * An ArchiveWriter is a writeable DataArchive. New files may be added with the
 * {@link #addFile} and {@link #addImage} methods.
 */
public class ArchiveWriter extends DataArchive {
  private Hashtable images = new Hashtable();
  private Hashtable sounds = new Hashtable();
  private Hashtable files = new Hashtable();
  private String archiveName;

  /**
   * Create a new writeable archive.
   * 
   * @param zipName
   *          the name of the archive. If null, the user will be prompted for a
   *          filename when saving. If not null, new entries will be added to
   *          the named archive. If the file exists and is not a zip archive, it
   *          will be overwritten.
   */
  public ArchiveWriter(String zipName) {
    archiveName = zipName;
    if (archiveName == null) {
      archive = null;
    }
    else {
      try {
        archive = new ZipFile(archiveName);
      }
      catch (IOException ex) {
        archive = null;
      }
    }
  }

  public ArchiveWriter(ZipFile archive) {
    this.archive = archive;
    archiveName = archive.getName();
  }

  /**
   * Add an image file to the archive. The file will be copied into an "images"
   * directory in the archive. Storing another image with the same name will
   * overwrite the previous image.
   * 
   * @param path
   *          the full path of the image file on the user's filesystem
   * @param name
   *          the name under which to store the image in the archive
   */
  public void addImage(String path, String name) {
    unCacheImage(name);
    images.put(IMAGE_DIR + name, path);
    imageNames = null;
  }

  public void addImage(String name, byte[] contents) {
    unCacheImage(name);
    images.put(IMAGE_DIR + name, contents);
    imageNames = null;
  }
  
  public void addSound(String file, String name) {
    sounds.put(SOUNDS_DIR+name,file);
  }

  public boolean isImageAdded(String name) {
    return images.containsKey(name);
  }

  public void removeImage(String name) {
    unCacheImage(name);
    images.remove(IMAGE_DIR + name);
  }

  /**
   * Copy a file from the user's filesystem to the archive.
   * 
   * @param path
   *          the full path of the file on the user's filesystem
   * @param name
   *          the name under which to store the file in the archive
   */
  public void addFile(String path, String name) {
    files.put(name, path);
  }

  /**
   * Copy an InputStream into the archive
   * 
   * @param name
   *          the name under which to store the contents of the stream
   * @param stream
   *          the stream to copy
   */
  public void addFile(String name, InputStream stream) {
    try {
      files.put(name, getBytes(stream));
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  public void addFile(String name, byte[] content) {
    files.put(name, content);
  }

  /**
   * Overrides {@link DataArchive#getFileStream} to return streams that have
   * been added to the archive but not yet written to disk.
   */
  public InputStream getFileStream(String name) throws IOException {
    InputStream stream;
    stream = getAddedStream(images, name);
    if (stream == null) {
      stream = getAddedStream(files, name);
    }
    if (stream == null) {
      stream = getAddedStream(sounds, name);
    }
    if (stream == null) {
      if (archive != null) {
        stream = super.getFileStream(name);
      }
      else {
        throw new IOException(name + " not found");
      }
    }
    return stream;
  }

  private InputStream getAddedStream(Hashtable table, String name) throws IOException {
    InputStream stream = null;
    Object file = table.get(name);
    if (file instanceof String) {
      stream = new FileInputStream((String) file);
    }
    else if (file instanceof byte[]) {
      stream = new ByteArrayInputStream((byte[]) file);
    }
    return stream;
  }

  public void saveAs() throws IOException {
    archiveName = null;
    write();
  }

  /**
   * If the ArchiveWriter was initialized with non-null file name, then write
   * the contents of the archive to the named archive. If it was initialized
   * with a null name, prompt the user to select a new file into which to write
   * archive
   */
  public void write() throws IOException {
    if (archiveName == null) {
      FileChooser fc = FileChooser.createFileChooser(null);
      fc.setCurrentDirectory(Documentation.getDocumentationBaseDir());
      if (fc.showSaveDialog() != FileChooser.APPROVE_OPTION) return;
      archiveName = fc.getSelectedFile().getPath();
    }

    String temp = (new File(archiveName)).getParent();
    temp = temp == null ? "temp" : temp + File.separator + "temp";
    int n = 1;
    while ((new File(temp + n + ".zip")).exists()) {
      n++;
    }
    temp = temp + n + ".zip";

    int count;
    byte[] buffer = new byte[1024];

    ZipOutputStream out = new ZipOutputStream(new FileOutputStream(temp));
    if (archive != null) {
      /* Copy old non-overwritten entries into temp file */
      ZipEntry entry = null;
      ZipInputStream zis = new ZipInputStream(new FileInputStream(archive.getName()));

      while ((entry = zis.getNextEntry()) != null) {
        if (!images.containsKey(entry.getName()) && !sounds.containsKey(entry.getName()) && !files.containsKey(entry.getName())) {
          /* System.err.println("Copying "+entry.getName()); */
          ByteArrayOutputStream outStream = new ByteArrayOutputStream();
          while ((count = zis.read(buffer, 0, 1024)) >= 0) {
            outStream.write(buffer, 0, count);
          }
          byte[] contents = outStream.toByteArray();
          ZipEntry newEntry = new ZipEntry(entry.getName());
          newEntry.setMethod(entry.getMethod());
          if (newEntry.getMethod() == ZipEntry.STORED) {
            newEntry.setSize(contents.length);
            CRC32 checksum = new CRC32();
            checksum.update(contents);
            newEntry.setCrc(checksum.getValue());
          }
          out.putNextEntry(newEntry);
          out.write(contents, 0, contents.length);
        }
      }
      zis.close();
      archive.close();
    }
    /* Write new entries into temp file */
    writeEntries(images, ZipEntry.STORED, out);
    writeEntries(sounds, ZipEntry.STORED, out);
    writeEntries(files, ZipEntry.DEFLATED, out);
    out.close();

    File original = new File(archiveName);
    if (original.exists()) {
      if (!original.delete()) {
        throw new IOException("Unable to overwrite " + archiveName + "\nData stored in " + temp);
      }
    }
    File f = new File(temp);
    if (!f.renameTo(original)) {
      throw new IOException("Unable to write to " + archiveName + "\nData stored in " + temp);
    }
    archive = new ZipFile(archiveName);
  }

  private static void writeEntries(Hashtable h, int method, ZipOutputStream out) throws IOException {
    byte[] contents;
    ZipEntry entry;

    for (Enumeration e = h.keys(); e.hasMoreElements();) {
      String name = (String) e.nextElement();
      Object o = h.get(name);
      if (o instanceof String) {
        String path = (String) o;
        contents = getBytes(new FileInputStream(path));
      }
      else if (o instanceof byte[]) {
        contents = (byte[]) o;
      }
      else {
        System.err.println("Could not write entry " + name + " = " + o);
        break;
      }
      entry = new ZipEntry(name);
      entry.setMethod(method);
      if (method == ZipEntry.STORED) {
        entry.setSize(contents.length);
        CRC32 checksum = new CRC32();
        checksum.update(contents);
        entry.setCrc(checksum.getValue());
      }
      out.putNextEntry(entry);
      out.write(contents);
    }
  }

  protected void listImageNames(Collection v) {
    super.listImageNames(v);
    for (Enumeration e = images.keys(); e.hasMoreElements();) {
      String name = (String) e.nextElement();
      if (name.startsWith(IMAGE_DIR)) {
        v.add(name.substring(IMAGE_DIR.length()));
      }
    }
  }

}
