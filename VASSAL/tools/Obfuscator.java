/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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

import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.util.Random;

/**
 * Utility class that handles simple obfuscation of a file's contents, to prevent the casual cheat of hand-editing a
 * logfile
 * 
 * @author rkinney
 * 
 */
public class Obfuscator {
  public static final String HEADER = "!VCS";
  
  private static final Random rand = new Random();

  private String encrypted;
  private byte key;
  
  public Obfuscator(byte[] contents) {
    key = (byte) rand.nextInt(256);
    StringBuffer buffer = new StringBuffer(HEADER);
    buffer.append(Integer.toHexString(key));
    for (int i=0;i<contents.length;++i) {
      buffer.append(Integer.toHexString(contents[i]));
    }
    encrypted = buffer.toString();
  }
  
  public void write(OutputStream out) throws IOException {
    out.write(encrypted.getBytes("UTF-8"));
  }
}
