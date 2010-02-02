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

import VASSAL.tools.nio.file.attribute.*;
import VASSAL.tools.nio.file.*;
//import java.nio.file.attribute.*;
//import java.nio.file.*;
import java.io.IOException;



public class ZipFileBasicAttributeView implements BasicFileAttributeView,
                                                  AttributeViewByName {
  // encapsulates the object that we are bound too

  protected final ZipFilePath file;

  /** Creates a new instance of ZipFileAttributeView */
  public ZipFileBasicAttributeView(ZipFilePath file) {
    this.file = file;
  }

  public String name() {
    return "basic";
  }

  public BasicFileAttributes readAttributes() throws IOException {
    return new ZipFileBasicAttributes(file);
  }

  public void setTimes(FileTime mtime, FileTime atime, FileTime ctime)
                                                           throws IOException {
    if (atime != null) throw new UnsupportedOperationException();
    if (ctime != null) throw new UnsupportedOperationException();

    if (!file.exists()) throw new NoSuchFileException(file.toString());

    final ZipFileSystem fs = file.getFileSystem();

    try {
      fs.writeLock(file);

      final Path rpath = fs.externalize(file);

      ZipEntryInfo ze = fs.getInfo(file);
      if (ze == null) {
        ze = ZipUtils.getFakeEntry(file, rpath);
        fs.putInfo(file, ze);
      }
      
      ze.lastModifiedTime = ZipUtils.javaToDosTime(mtime.toMillis());
    }
    finally {
      fs.writeUnlock(file);
    }
  }

  public Object getAttribute(String attribute) throws IOException {
    final BasicFileAttributes bfa = readAttributes();

    if (attribute.equals("lastModifiedTime")) {
      return bfa.lastModifiedTime();
    }
    if (attribute.equals("lastAccessTime")) {
      return bfa.lastAccessTime();
    }
    if (attribute.equals("creationTime")) {
      return bfa.creationTime();
    }
    if (attribute.equals("size")) {
      return bfa.size();
    }
    if (attribute.equals("isRegularFile")) {
      return bfa.isRegularFile();
    }
    if (attribute.equals("isDirectory")) {
      return bfa.isDirectory();
    }
    if (attribute.equals("isSymbolicLink")) {
      return bfa.isSymbolicLink();
    }
    if (attribute.equals("isOther")) {
      return bfa.isOther();
    }
    if (attribute.equals("fileKey")) {
      return bfa.fileKey();
    }

    return null;
  }

  public void setAttribute(String attribute, Object value) throws IOException {
    if (attribute.equals("lastModifiedTime")) { 
      setTimes((FileTime) value, null, null);
    }
    else {
      throw new UnsupportedOperationException();
    }
  }
}
