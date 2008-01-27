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

public abstract class Compressor {
  public static byte[] compress(byte[] in) throws IOException {
    ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
    ZipOutputStream zipOut = new ZipOutputStream(byteOut);
    zipOut.putNextEntry(new ZipEntry("Dummy")); //$NON-NLS-1$
    zipOut.write(in);
    zipOut.close();
    return byteOut.toByteArray();
  }

  public static byte[] decompress(byte[] in) throws IOException {
    ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
    ZipInputStream zipIn = new ZipInputStream(new ByteArrayInputStream(in));
    zipIn.getNextEntry();
    byte[] buffer = new byte[4096];
    int count = 0;
    while ((count = zipIn.read(buffer)) > 0) {
      byteOut.write(buffer, 0, count);
    }
    return byteOut.toByteArray();
  }

  public static void main(String args[]) throws Exception {
    if (args.length == 0) {
      Frame f = new Frame();
      TextField tf = new TextField(60);
      f.add(tf);
      f.pack();
      f.setVisible(true);
      tf.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          try {
            String s = evt.getActionCommand();
            System.err.println("Input (" + s.length() + ") = " + s); //$NON-NLS-1$ //$NON-NLS-2$
            String comp = new String(compress(s.getBytes()));
            System.err.println("Compressed (" + comp.length() + ") = " + comp); //$NON-NLS-1$ //$NON-NLS-2$
            String decomp = new String(decompress(comp.getBytes()));
            System.err.println("Decompressed (" + decomp.length() + ") = " + decomp); //$NON-NLS-1$ //$NON-NLS-2$
          }
          catch (IOException ex) {
            ex.printStackTrace();
          }
        }
      });
    }
    else {
      ByteArrayOutputStream byteOut = new ByteArrayOutputStream();
      FileInputStream file = new FileInputStream(args[0]);
      byte[] b = new byte[4096];
      int len = 0;
      while ((len = file.read(b)) > 0) {
        byteOut.write(b, 0, len);
      }
      file.close();
      byte[] contents = byteOut.toByteArray();
      if (contents[0] == 'P' && contents[1] == 'K') {
        byte[] uncompressed = Compressor.decompress(contents);
        FileOutputStream out = new FileOutputStream(args[0] + ".uncompressed"); //$NON-NLS-1$
        out.write(uncompressed);
        out.close();
        byte[] recompressed = Compressor.compress(uncompressed);
        if (!Arrays.equals(recompressed,contents)) {
          throw new RuntimeException("Compression failed"); //$NON-NLS-1$
        }
      }
      else {
        byte[] compressed = Compressor.compress(contents);
        FileOutputStream out = new FileOutputStream(args[0] + ".compressed"); //$NON-NLS-1$
        out.write(compressed);
        out.close();
        if (!Arrays.equals(Compressor.decompress(compressed),contents)) {
          throw new RuntimeException("Compression failed"); //$NON-NLS-1$
        }
      }
    }
  }
}
