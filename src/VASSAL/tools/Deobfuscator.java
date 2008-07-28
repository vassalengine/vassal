/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available 
 * at http://www.opensource.org.
 */
package VASSAL.tools;

import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

/**
 * Converts an file created with {@link Obfuscator} back into plain text.
 * Additionally, plain text will be passed through unchanged.
 */
public class Deobfuscator {
  private String plain;

  public Deobfuscator(InputStream in) throws IOException {
    String s = null;
    try {
      s = IOUtils.toString(in, "UTF-8");
      in.close();
    }
    catch (UnsupportedEncodingException e) {
      // should never happen
      ErrorDialog.bug(e);
      throw e;
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    int offset = Obfuscator.HEADER.length();
    if (s.startsWith(Obfuscator.HEADER) && s.length() > offset+1) {
      byte key = (byte) Integer.parseInt(s.substring(offset, offset + 2), 16);
      offset += 2;
      byte[] bytes = new byte[(s.length() - offset) / 2];
      for (int i = 0; i < bytes.length; ++i) {
        bytes[i] = (byte)
          (Integer.parseInt(s.substring(offset++, ++offset), 16) ^ key);
      }
      plain = new String(bytes,"UTF-8");
    }
    else {
      plain = s;
    }
  }

  public String getString() throws UnsupportedEncodingException {
    return plain;
  }
 
  // Convert an obfuscated file into a plain-text file
  public static void main(String[] args) throws Exception {
    System.out.println("Decoding "+args[0]);
    Deobfuscator d = new Deobfuscator(new FileInputStream(args[0]));
    FileWriter out = new FileWriter(args[0]);
    out.write(d.getString());
    out.close();
    System.out.println("Done!");
    System.exit(0);
  }
}
