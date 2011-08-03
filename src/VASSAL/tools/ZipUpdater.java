/*
 * $Id$
 *
 * Copyright (c) 2003 by Rodney Kinney
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

import static VASSAL.tools.IterableEnumeration.iterate;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Enumeration;
import java.util.Properties;
import java.util.jar.JarOutputStream;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.io.IOUtils;

/**
 * Automatically builds a .jar file that will update a Zip archive.
 * Usage:  java VASSAL.tools.ZipUpdater <oldArchiveName> <newArchiveName>
 * will create a file named update<oldArchiveName>.jar Executing this jar (by double-clicking or
 * typing "java -jar update<oldArchiveName>.jar") will update the old archive so that its contents are identical to
 * the new archive.
 * User: rkinney
 * Date: Oct 23, 2003
 */
public class ZipUpdater implements Runnable {
  private static final Logger logger =
    LoggerFactory.getLogger(ZipUpdater.class);

  public static final String CHECKSUM_RESOURCE = "checksums";
  public static final String TARGET_ARCHIVE = "target";
  public static final String UPDATED_ARCHIVE_NAME = "finalName";
  public static final String ENTRIES_DIR = "entries/";
  private File oldFile;
  private ZipFile oldZipFile;
  private Properties checkSums;

  public ZipUpdater(File input) throws IOException {
    this.oldFile = input;
    if (!oldFile.exists()) {
      throw new IOException("Could not find file " + input.getPath());
    }
  }

  private long getCrc(ZipFile file, ZipEntry entry) throws IOException {
    long crc = -1;
    if (entry != null) {
      crc = entry.getCrc();
      if (crc < 0) {
        CRC32 checksum = new CRC32();

        final InputStream in = file.getInputStream(entry);
        try {
          final byte[] buffer = new byte[1024];
          int count;
          while ((count = in.read(buffer)) >= 0) {
            checksum.update(buffer, 0, count);
          }
          in.close();
          crc = checksum.getValue();
        }
        finally {
          IOUtils.closeQuietly(in);
        }
      }
    }
    return crc;
  }

  private long copyEntry(ZipOutputStream output, ZipEntry newEntry) throws IOException {
    return writeEntry(oldZipFile.getInputStream(new ZipEntry(newEntry.getName())), output, newEntry);
  }

  private long replaceEntry(ZipOutputStream output, ZipEntry newEntry) throws IOException {
    final InputStream newContents =
      getClass().getResourceAsStream("/" + ENTRIES_DIR + newEntry.getName());
    if (newContents == null) {
      throw new IOException("This updater was created with an original that differs from the file you're trying to update.\nLocal entry does not match original:  "+newEntry.getName());
    }

    BufferedInputStream in = null;
    try {
      in = new BufferedInputStream(newContents);
      long cs = writeEntry(newContents, output, newEntry);
      in.close();
      return cs;
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }


  private long writeEntry(InputStream zis, ZipOutputStream output, ZipEntry newEntry) throws IOException {
    // FIXME: is there a better way to do this, so that the whole input
    // stream isn't in memory at once?
    final byte[] contents = IOUtils.toByteArray(zis);
    final CRC32 checksum = new CRC32();
    checksum.update(contents);
    if (newEntry.getMethod() == ZipEntry.STORED) {
      newEntry.setSize(contents.length);
      newEntry.setCrc(checksum.getValue());
    }
    output.putNextEntry(newEntry);
    output.write(contents, 0, contents.length);
    return checksum.getValue();
  }

  public void write(File destination) throws IOException {
    checkSums = new Properties();

    final InputStream rin =
      ZipUpdater.class.getResourceAsStream("/" + CHECKSUM_RESOURCE);
    if (rin == null)
      throw new IOException("Resource not found: " + CHECKSUM_RESOURCE);

    BufferedInputStream in = null;
    try {
      in = new BufferedInputStream(rin);
      checkSums.load(in);
      in.close();
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    final File tempFile = File.createTempFile("VSL", ".zip");

    oldZipFile = new ZipFile(oldFile.getPath());
    try {
      final ZipOutputStream output =
        new ZipOutputStream(new FileOutputStream(tempFile));
      try {
// FIXME: reinstate when we move to 1.6+.
//        for (String entryName : checkSums.stringPropertyNames()) {
        for (Enumeration<Object> e = checkSums.keys(); e.hasMoreElements();) {
          final String entryName = (String) e.nextElement();
          long targetSum;
          try {
            targetSum =
              Long.parseLong(checkSums.getProperty(entryName, "<none>"));
          }
          // FIXME: review error message
          catch (NumberFormatException invalid) {
            throw new IOException("Invalid checksum " + checkSums.getProperty(entryName, "<none>") + " for entry " + entryName);
          }
          final ZipEntry entry = oldZipFile.getEntry(entryName);
          final ZipEntry newEntry = new ZipEntry(entryName);
          newEntry.setMethod(entry != null ? entry.getMethod() : ZipEntry.DEFLATED);
          if (targetSum == getCrc(oldZipFile, entry)) {
            if (targetSum != copyEntry(output, newEntry)) {
              throw new IOException("Checksum mismatch for entry " + entry.getName());
            }
          }
          else {
            if (targetSum != replaceEntry(output, newEntry)) {
              throw new IOException("Checksum mismatch for entry " + entry.getName());
            }
          }
        }
      }
      finally {
        try {
          output.close();
        }
        // FIXME: review error message
        catch (IOException e) {
          logger.error("", e);
        }
      }
    }
    finally {
      oldZipFile.close();
    }

    if (destination.getName().equals(oldFile.getName())) {
      String updatedName = destination.getName();
      int index = updatedName.lastIndexOf('.');
      String backup = index < 0 || index == updatedName.length() - 1
          ? updatedName + "Backup" : updatedName.substring(0, index) + "Backup" + updatedName.substring(index);
      if (!oldFile.renameTo(new File(backup))) {
        throw new IOException("Unable to create backup file " + backup + ".\nUpdated file is in " + tempFile.getPath());
      }
    }
    if (!tempFile.renameTo(destination)) {
      throw new IOException("Unable to write to file " + destination.getPath()+ ".\nUpdated file is in " + tempFile.getPath());
    }
  }

  public void createUpdater(File newFile) throws IOException {
    String inputArchiveName = oldFile.getName();
    int index = inputArchiveName.indexOf('.');
    String jarName;
    if (index >= 0) {
      jarName = "update" + inputArchiveName.substring(0, index) + ".jar";
    }
    else {
      jarName = "update" + inputArchiveName;
    }
    createUpdater(newFile, new File(jarName));
  }

  public void createUpdater(File newFile, File updaterFile) throws IOException {
    if (!updaterFile.getName().endsWith(".jar")) {
      final String newName = updaterFile.getName().replace('.','_')+".jar";
      updaterFile = new File(updaterFile.getParentFile(),newName);
    }
    checkSums = new Properties();

    try {
      oldZipFile = new ZipFile(oldFile);
      final String inputArchiveName = oldFile.getName();

      final ZipFile goal = new ZipFile(newFile);
      try {
        JarOutputStream out = null;
        try {
          out = new JarOutputStream(
                  new BufferedOutputStream(new FileOutputStream(updaterFile)));
          for (ZipEntry entry : iterate(goal.entries())) {
            final long goalCrc = getCrc(goal, entry);
            final long inputCrc =
              getCrc(oldZipFile, oldZipFile.getEntry(entry.getName()));
            if (goalCrc != inputCrc) {
              final ZipEntry outputEntry =
                new ZipEntry(ENTRIES_DIR + entry.getName());
              outputEntry.setMethod(entry.getMethod());

              InputStream gis = null;
              try {
                gis = new BufferedInputStream(goal.getInputStream(entry));
                writeEntry(gis, out, outputEntry);
                gis.close();
              }
              finally {
                IOUtils.closeQuietly(gis);
              }
            }
            checkSums.put(entry.getName(), goalCrc + "");
          }

          final ZipEntry manifestEntry = new ZipEntry("META-INF/MANIFEST.MF");
          manifestEntry.setMethod(ZipEntry.DEFLATED);
          final StringBuilder buffer = new StringBuilder();
          buffer.append("Manifest-Version: 1.0\n")
                .append("Main-Class: VASSAL.tools.ZipUpdater\n");
          writeEntry(
            new ByteArrayInputStream(buffer.toString().getBytes("UTF-8")),
            out,
            manifestEntry);

          final ZipEntry nameEntry = new ZipEntry(TARGET_ARCHIVE);
          nameEntry.setMethod(ZipEntry.DEFLATED);
          writeEntry(
            new ByteArrayInputStream(inputArchiveName.getBytes("UTF-8")),
            out,
            nameEntry);

          final ZipEntry updatedEntry = new ZipEntry(UPDATED_ARCHIVE_NAME);
          updatedEntry.setMethod(ZipEntry.DEFLATED);
          writeEntry(
            new ByteArrayInputStream(newFile.getName().getBytes("UTF-8")),
            out,
            updatedEntry);

          final ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
          checkSums.store(byteOut, null);
          final ZipEntry sumEntry = new ZipEntry(CHECKSUM_RESOURCE);
          sumEntry.setMethod(ZipEntry.DEFLATED);
          writeEntry(
            new ByteArrayInputStream(byteOut.toByteArray()),
            out,
            sumEntry);

          final String className =
            getClass().getName().replace('.', '/') + ".class";
          final ZipEntry classEntry = new ZipEntry(className);
          classEntry.setMethod(ZipEntry.DEFLATED);

          final InputStream is =
            getClass().getResourceAsStream("/" + className);
          if (is == null)
            throw new IOException("Resource not found: " + className);

          BufferedInputStream in = null;
          try {
            in = new BufferedInputStream(is);
            writeEntry(is, out, classEntry);
            in.close();
          }
          finally {
            IOUtils.closeQuietly(in);
          }

          out.close();
        }
        finally {
          IOUtils.closeQuietly(out);
        }

        goal.close();
      }
      finally {
        IOUtils.closeQuietly(goal);
      }

      oldZipFile.close();
    }
    finally {
      IOUtils.closeQuietly(oldZipFile);
    }
  }

  private String fileName;
  private Exception error;

  private ZipUpdater(String fileName, Exception error) {
    this.fileName = fileName;
    this.error = error;
  }

  public void run() {
    JOptionPane.showMessageDialog(null, "Unable to update " + fileName + ".\n" + error.getMessage(), "Update failed", JOptionPane.ERROR_MESSAGE);
    System.exit(0);
  }

  public static void main(String[] args) {
    String oldArchiveName = "<unknown>";
    try {
      if (args.length > 1) {
        oldArchiveName = args[0];
        String goal = args[1];
        ZipUpdater updater = new ZipUpdater(new File(oldArchiveName));
        updater.createUpdater(new File(goal));
      }
      else {
        BufferedReader r = null;
        try {
          r = new BufferedReader(new InputStreamReader(
                ZipUpdater.class.getResourceAsStream("/" + TARGET_ARCHIVE)));
          oldArchiveName = r.readLine();
          r.close();
        }
        finally {
          if (r != null) {
            try {
              r.close();
            }
            catch (IOException e) {
              e.printStackTrace();
            }
          }
        }

        try {
          r = new BufferedReader(new InputStreamReader(
            ZipUpdater.class.getResourceAsStream("/" + UPDATED_ARCHIVE_NAME)));
          final String newArchiveName = r.readLine();
          final ZipUpdater updater = new ZipUpdater(new File(oldArchiveName));
          updater.write(new File(newArchiveName));
          r.close();
        }
        finally {
          if (r != null) {
            try {
              r.close();
            }
            catch (IOException e) {
              e.printStackTrace();
            }
          }
        }
      }
    }
    catch (final IOException e) {
      e.printStackTrace();
      try {
        SwingUtilities.invokeAndWait(new ZipUpdater(oldArchiveName,e));
      }
      catch (Exception e1) {
        e1.printStackTrace();
      }
    }
  }
}
