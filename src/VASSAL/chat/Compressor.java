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
package VASSAL.chat;

import java.awt.Frame;
import java.awt.TextField;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import VASSAL.tools.io.IOUtils;

public abstract class Compressor {
  public static byte[] compress(byte[] in) throws IOException {
    final ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
    final ZipOutputStream zipOut = new ZipOutputStream(byteOut);
    try {
      zipOut.putNextEntry(new ZipEntry("Dummy")); //$NON-NLS-1$
      zipOut.write(in);
    }
    finally {
      try {
        zipOut.close();
      }
      // FIXME: review error message
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    return byteOut.toByteArray();
  }

  public static byte[] decompress(byte[] in) throws IOException {
    final ZipInputStream zipIn =
      new ZipInputStream(new ByteArrayInputStream(in));
    try {
      zipIn.getNextEntry();
      return IOUtils.toByteArray(zipIn);
    }
    finally {
      try {
        zipIn.close();
      }
      // FIXME: review error message
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  public static void main(String args[]) throws Exception {
    if (args.length == 0) {
      final Frame f = new Frame();
      final TextField tf = new TextField(60);
      f.add(tf);
      f.pack();
      f.setVisible(true);
      tf.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          try {
            final String s = evt.getActionCommand();
            System.err.println("Input (" + s.length() + ") = " + s); //$NON-NLS-1$ //$NON-NLS-2$
            final String comp = new String(compress(s.getBytes()));
            System.err.println("Compressed (" + comp.length() + ") = " + comp); //$NON-NLS-1$ //$NON-NLS-2$
            final String decomp = new String(decompress(comp.getBytes()));
            System.err.println("Decompressed (" + decomp.length() + ") = " + decomp); //$NON-NLS-1$ //$NON-NLS-2$
          }
          // FIXME: review error message
          catch (IOException ex) {
            ex.printStackTrace();
          }
        }
      });
    }
    else {
      final ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
      final FileInputStream file = new FileInputStream(args[0]);
      try {
        IOUtils.copy(file, byteOut);
      }
      finally {
        try {
          file.close();
        }
        // FIXME: review error message
        catch (IOException e) {
          e.printStackTrace();
        }
      }

      final byte[] contents = byteOut.toByteArray();
      if (contents[0] == 'P' && contents[1] == 'K') {
        final byte[] uncompressed = Compressor.decompress(contents);
        final FileOutputStream out =
          new FileOutputStream(args[0] + ".uncompressed"); //$NON-NLS-1$
        try {
          out.write(uncompressed);
        }
        finally {
          try {
            out.close();
          }
          // FIXME: review error message
          catch (IOException e) {
            e.printStackTrace();
          }
        }

        final byte[] recompressed = Compressor.compress(uncompressed);
        if (!Arrays.equals(recompressed,contents)) {
// FIXME: don't throw unchecked exception
          throw new RuntimeException("Compression failed"); //$NON-NLS-1$
        }
      }
      else {
        final byte[] compressed = Compressor.compress(contents);
        final FileOutputStream out =
          new FileOutputStream(args[0] + ".compressed"); //$NON-NLS-1$
        try {
          out.write(compressed);
        }
        finally {
          try {
            out.close();
          }
          // FIXME: review error message
          catch (IOException e) {
            e.printStackTrace();
          }
        }

        if (!Arrays.equals(Compressor.decompress(compressed),contents)) {
// FIXME: don't throw unchecked exception
          throw new RuntimeException("Compression failed"); //$NON-NLS-1$
        }
      }
    }
  }
}
