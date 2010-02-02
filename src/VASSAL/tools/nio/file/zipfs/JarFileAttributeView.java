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

public class JarFileAttributeView extends ZipFileAttributeView {

  /** Creates a new instance of JarFileAttributeView */
  public JarFileAttributeView(ZipFilePath file) {
    super(file);
  }

  @Override
  public String name() {
    return "jar";
  }

  @Override
  public JarFileAttributes readAttributes() throws IOException {
    return new JarFileAttributes(file);
  }

  @Override
  public Object getAttribute (String attribute) throws IOException {
    final JarFileAttributes jfa = readAttributes();
    if (attribute.equals("manifestAttributes")) {
      return jfa.getManifestAttributes();
    }
    if (attribute.equals("entryAttributes")) {
      return jfa.getEntryAttributes();
    }
    return super.getAttribute(attribute);
  }

  @Override
  public void setAttribute(String attribute, Object value) throws IOException {
    throw new UnsupportedOperationException();
  }
}
