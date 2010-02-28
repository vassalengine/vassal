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

//import java.nio.file.*;
import java.io.*;

public class ZipFileAttributes extends ZipFileBasicAttributes {

  /** Creates a new instance of ZipFileAttributes */
  private String[] version = {
    "FAT file system (DOS, OS/2, NT)",
    "Amiga",
    "VMS (VAX or Alpha AXP)",
    "UNIX",
    "VM/CMS",
    "Atari",
    "HPFS file system (OS/2, NT 3.x)",
    "Macintosh",
    "Z-System",
    "CP/M",
    "TOPS-20",
    "NTFS file system (NT)",
    "SMS/QDOS",
    "Acorn RISC OS",
    "VFAT file system (Win95, NT)",
    "MVS",
    "BeOS (BeBox or PowerMac)",
    "Tandem"
  };

  public ZipFileAttributes(ZipFilePath file) throws IOException {
    super(file);
  }

  public byte[] comment() {
    return ze.comment;
  }

  public int compressedSize() {
    return ze.compSize;
  }

  public long crc() {
    return ze.crc;
  }

  public byte[] extra() {
    return ze.extraField;
  }

  public int method() {
    return ze.method;
  }

  public byte[] name() {
    return ze.filename;
  }

  public boolean isArchiveFile() {
    return ze.isArchiveFile;
  }

  public String versionMadeBy() {
    int ver = (ze.versionMadeBy >> 8);
    if (ver >= 0 && ver < 17) {
      return version[ver];
    }
    else {
      return "unused";
    }
  }

  public int externalAttrs() {
    return ze.extAttrs;
  }
}
