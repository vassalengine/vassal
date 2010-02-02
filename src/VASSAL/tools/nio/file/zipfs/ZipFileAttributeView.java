/*
 * Copyright 2007-2009 Sun Microsystems, Inc.  All Rights Reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *   - Neither the name of Sun Microsystems nor the names of its
 *     contributors may be used to endorse or promote products derived
 *     from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package VASSAL.tools.nio.file.zipfs;

import VASSAL.tools.nio.file.*;

//import java.nio.file.*;
import java.io.IOException;

public class ZipFileAttributeView extends ZipFileBasicAttributeView {

  /** Creates a new instance of ZipFileAttributeView */
  public ZipFileAttributeView(ZipFilePath file) {
    super(file);
  }

  @Override
  public String name() {
    return "zip";
  }

  @Override
  public ZipFileAttributes readAttributes() throws IOException {
    return new ZipFileAttributes(file);
  }

// FIXME: should make comment, method settable

  @Override
  public Object getAttribute(String attribute) throws IOException {
    final ZipFileAttributes zfa = readAttributes();

    if (attribute.equals("comment")) {
      return zfa.comment();
    }

    if (attribute.equals("compressedSize")) {
      return zfa.compressedSize();
    }

    if (attribute.equals("crc")) {
      return zfa.crc();
    }

    if (attribute.equals("extra")) {
      return zfa.extra();
    }

    if (attribute.equals("method")) {
      return zfa.method();
    }

    if (attribute.equals("name")) {
      return zfa.name();    
    }
  
    if (attribute.equals("isArchiveFile")) {
      return zfa.isArchiveFile();
    }

    if (attribute.equals("versionMadeBy")) {
      return zfa.versionMadeBy();
    }

    if (attribute.equals("extAttrs")) {
      return zfa.externalAttrs();
    }

    return null;
  }

  @Override
  public void setAttribute(String attribute, Object value) throws IOException {

    if (attribute.equals("comment")) {
    }
    else if (attribute.equals("extra")) {
    }
    else if (attribute.equals("method")) {
    }
    else if (attribute.equals("extAttrs")) {
    }

    throw new UnsupportedOperationException();
  }
}
