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

import VASSAL.tools.nio.channels.SeekableByteChannel;
import VASSAL.tools.nio.channels.FileChannelAdapter;
import VASSAL.tools.nio.file.*;
import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.FileSystems;
import VASSAL.tools.nio.file.Path;
//import VASSAL.tools.nio.file.attribute.Attributes;
import VASSAL.tools.nio.file.attribute.BasicFileAttributes;

import static VASSAL.tools.nio.file.zipfs.ZipHeaderConstants.*;

//import java.nio.file.*;
//import java.nio.channels.SeekableByteChannel;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
//import static com.sun.nio.zipfs.ZipHeaderConstants.*;
import java.io.*;
import java.net.URI;
//import java.nio.channels.FileChannel;
//import java.nio.file.FileSystem;
//import java.nio.file.FileSystems;
//import java.nio.file.Path;
import java.util.*;
import java.util.Calendar;
import java.util.TimeZone;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.zip.*;

/**
 * This class implements static methods for reading the zip file contents
 * It reads all the entries and caches them into a weak has map.
 * Note that Zip reader may not work well for self extracting zips and
 * other variants.
 * This is a demo quality implementation, but can be enhanced easily.
 */
public class ZipUtils {

  /**
   * This map stores all the zip files that are extracted during the
   * course of time when zip operations are performed on the zip file.
   * The key for the map is the zip file reference.
   * The zip file entries themselves are stored as map entries whose
   * keys are zip file references to the entries.
   */
  public static Map<URI,Map<ZipFilePath,ZipEntryInfo>> cachedEntries =
      new HashMap<URI,Map<ZipFilePath,ZipEntryInfo>>();
  private static final boolean debug = true;
  private static final FileSystem defFileSystem = FileSystems.getDefault();

  private static ByteBuffer getHeaderField(SeekableByteChannel ch,
                                           long offset, int nBytes)
                                                           throws IOException {
    final ByteBuffer buf = ByteBuffer.allocate(nBytes);
    buf.order(ByteOrder.LITTLE_ENDIAN);
    ch.position(offset);
    final int read = ch.read(buf);
    if (read <= 0) return null;
    buf.flip();
    return buf;
  }

  private static int readInt(SeekableByteChannel ch, long offset)
                                                           throws IOException {
    final ByteBuffer buf = getHeaderField(ch, offset, 4);
    return buf.getInt();
  }

  private static int readShort(SeekableByteChannel ch, long offset)
                                                           throws IOException {
    final ByteBuffer buf = getHeaderField(ch, offset, 2);
    return buf.getShort();
  }

  private static byte[] readBytes(SeekableByteChannel ch, long offset, int len)
                                                           throws IOException {
    final ByteBuffer buf = getHeaderField(ch, offset, len);
    return buf.array();
  }

  static long locateEndOfCentralDirRecord(SeekableByteChannel ch, FileRef file)
                                                           throws IOException {
    long fileLen = ch.size();
    // read the file backwards 4 bytes at a time
    long backOffset = fileLen - C_END_RECORD_MIN_OFF;
    byte[] signature = new byte[]{0x06, 0x05, 0x4b, 0x50};
    int matchedIndex = 0;
    for (; (backOffset >= 0); backOffset--) {
      ByteBuffer buf = getHeaderField(ch, backOffset, 1);
      if (buf == null) {
        break;
      }

      byte b;
      if ((b = buf.get()) == signature[matchedIndex]) {
        matchedIndex++;
      } 
      else {
        matchedIndex = 0;
      }

      if (matchedIndex == 4) {
        return backOffset; // this needs to be verified.
      }
    }
    throw new IOException("Could not locate the central header");
  }

  /**
   * logicalRef is the Logical file reference to the zip file.
   * LogicalRef is of type: ZipFileRef
   * <tt>file</tt> is the reference to the physical location of the file
   * and is of type FileRef.
   */
  public static Map<ZipFilePath, ZipEntryInfo> fillEntries(
                        ZipFilePath zipPath, FileRef file) throws IOException {

    SeekableByteChannel ch = ZipIO.open(file);
    ZipFileSystem m = zipPath.getFileSystem();
    FileSystem fs = null;
    Map<ZipFilePath, ZipEntryInfo> entries =
        new HashMap<ZipFilePath, ZipEntryInfo>();

    long endOfCentralDirOff = locateEndOfCentralDirRecord(ch, file);
    int totalEntries = readShort(ch,
        endOfCentralDirOff + C_TOTAL_ENTRIES_OFF);
    long centralDirOff = readInt(ch,
        endOfCentralDirOff + C_DIR_START_OFF);
    Path fileName1 = zipPath.getName();
    boolean isJar = false;
    if (fileName1 != null) {
      isJar = isJar(fileName1.toString());
    }
    else {
      isJar = isJar(zipPath.getFileSystem().getZipFileSystemFile());
    }

    for (int count = 0; count < totalEntries; count++) {
      int sig;
      if ((sig = readInt(ch, centralDirOff)) != CENTRAL_FILE_HDR_SIG) {
        throw new IOException("Not the beginning of the central directory!");
      }
      ZipEntryInfo ze = new ZipEntryInfo();
      JarEntryInfo jentry = null;
      ze.versionMadeBy = readShort(ch, centralDirOff + C_VER_MADE_BY);
      ze.method = readShort(ch, centralDirOff + C_COMP_METHOD_OFF);
      ze.lastModifiedTime = readInt(ch, centralDirOff + C_LAST_MOD_TIME_OFF);
      ze.crc = readInt(ch, centralDirOff + C_CRC_OFF);
      ze.compSize = readInt(ch, centralDirOff + C_COMP_SIZE_OFF);
      ze.size = readInt(ch, centralDirOff + C_UCOMP_SIZE_OFF);

      int filenameLen = readShort(ch, centralDirOff + C_FILE_NAME_LEN_OFF);
      int extraFieldLen = readShort(ch, centralDirOff + C_EXTRA_FLD_LEN_OFF);
      int commentLen = readShort(ch, centralDirOff + C_COMMENT_LEN_OFF);

      ze.extAttrs = readInt(ch, centralDirOff + C_EXT_ATTR_OFF);

      // check which address the offset is relative to
      ze.streamOffset = readInt(ch, centralDirOff + C_REL_OFF_LOCAL_HDR_OFF);
      // the above line will give offset for the file name.

      ze.filename = readBytes(ch, centralDirOff + C_FILE_NAME_OFF, filenameLen);
      if (extraFieldLen > 0) {
        ze.extraField = readBytes(ch, centralDirOff + C_FILE_NAME_OFF +
            filenameLen, extraFieldLen);
      }
      if (commentLen > 0) {
        ze.comment = readBytes(ch, centralDirOff + C_FILE_NAME_OFF +
            filenameLen + extraFieldLen, commentLen);
      }
      centralDirOff = centralDirOff + C_FILE_NAME_OFF +
          filenameLen + extraFieldLen + commentLen;

      String fn = new String(ze.filename);
      ZipFilePath entryPath = zipPath.resolve(fn);
      ze.isArchiveFile = entryPath.isArchiveFile();
      ze.isDirectory = (fn.endsWith("/") ? true : false);
      ze.isRegularFile = !ze.isDirectory;
      if (isJar) {
        jentry = new JarEntryInfo(ze);
        JarFile jarfile = new JarFile(file.toString());
        Manifest manifest = jarfile.getManifest();
        Attributes attrs = null;
        if (manifest != null) {
          attrs = manifest.getMainAttributes();
        }
        if (attrs != null) {
          jentry.manifestMainAttrs = attrs.entrySet();
        }
        JarEntry jarentry = jarfile.getJarEntry(new String(ze.filename));
        if (jarentry != null) {
          Attributes attributes = jarentry.getAttributes();
          if (attributes != null) {
            jentry.entryAttributes = attributes.entrySet();
          }
        }
      }
      // cache the entry
      if (!isJar) {
        entries.put(entryPath, ze);
      }
      else {
        entries.put(entryPath, jentry);
      }
    }
    ch.close();

    //root entry

    ZipEntryInfo rootentry = new ZipEntryInfo();
    rootentry.isDirectory = true;
    rootentry.isRegularFile = false;
    JarEntryInfo jarRootEntry = null;
    if (isJar) {
      jarRootEntry = new JarEntryInfo(rootentry);
    }
    ZipFilePath root1 = zipPath.getRoot();
    ZipFilePath root = (root1 == null) ? zipPath.toAbsolutePath().getRoot() : root1;
    if (isJar) {
      entries.put(root, jarRootEntry);
    }
    else {
      entries.put(root, rootentry);
    }
    return entries;
  }

  public static boolean isJar(String path) {
    String lowerCase = path.toLowerCase();
    return (lowerCase.endsWith(".jar"));
  }

  public static void extractZip(ZipFilePath f)
      throws IOException {
    Map<ZipFilePath, ZipEntryInfo> entries;
    FileRef refToPhysicalZipFile = f;
    if (f.isNestedZip()) {
      refToPhysicalZipFile = extractNestedZip(f);
    }
    else {
      refToPhysicalZipFile = defFileSystem.getPath(f.getFileSystem().getZipFileSystemFile());//zp.zipPath;
    }
    entries = fillEntries(f, refToPhysicalZipFile);
    cachedEntries.put(f.toUri0(), entries);
  }

  /**
   *  getKey() returns path upto archive file if exists
   *  otherwise path to Zip file in native filesytem
   */
  public static ZipFilePath getKey(ZipFilePath zp) {
    int count = zp.getEntryNameCount();
    if ((count == 0 || count == 1) && !zp.isArchiveFile()) { // / or /a/b/c
      ZipFilePath root1 = zp.getRoot();
      ZipFilePath root = (root1 == null) ? zp.toAbsolutePath().getRoot() : root1;
      return root; //zp.zipPath;
    }
    if (count > 1 && !zp.isArchiveFile()) {  // /a.zip/e/f --> /a.zip
      return zp.getParentEntry();
    }
    return zp; //no change /a.zip, /x/b.zip, /a.zip/b.zip, /a.zip/b.jar
  }

  /*
   * Returns a map containing the all the entries of the given zip file
   */
  public static Map<ZipFilePath, ZipEntryInfo> getEntries(ZipFilePath file)
                                                           throws IOException {

    //getKey() returns path upto archive file if exists
    //otherwise path to Zip file in native filesytem

    ZipFilePath key = getKey(file);
    Map<ZipFilePath, ZipEntryInfo> entries = cachedEntries.get(key.toUri0());
    if (entries == null) {
      extractZip(key);
    }
    entries = cachedEntries.get(key.toUri0());
    if (entries == null) {
      throw new IOException(
          "Zip entries for the file could not be found:" + key);
    }
    return entries;
  }

  /**
   * Returns an entry refered by the given file reference
   */
  public static ZipEntryInfo getEntry(FileRef file1)
      throws IOException {
    Map<ZipFilePath, ZipEntryInfo> entries = null;
    ZipEntryInfo ze = null;
    ZipFilePath file = ((ZipFilePath) file1).toAbsolutePath();
    int entryCount = file.getEntryNameCount();
    if (file.isArchiveFile() && entryCount == 1) {
      ZipFilePath root1 = file.getRoot();
      ZipFilePath root = (root1 == null) ? (file.toAbsolutePath().getRoot()) : root1;
      entries = ZipUtils.getEntries(root);
      ze = getElement(entries, file);
      ze.isDirectory = true; // Since it is Archive
      ze.isRegularFile = false;
    }
    else if (file.isArchiveFile() && entryCount > 1) {
      ZipFilePath path = file.getParentEntry();
      entries = ZipUtils.getEntries(path);
      ze = getElement(entries, file);
      ze.isDirectory = true; // Since it is Archive
      ze.isArchiveFile = false;
    }
    else {
      entries = ZipUtils.getEntries(file);
      ze = getElement(entries, file);
    }

    if (ze == null) {
      throw new NoSuchFileException(
          "Zip file entry not found:" + file);
    }
    return ze;
  }

  static ZipEntryInfo getElement(Map<ZipFilePath, ZipEntryInfo> entries, ZipFilePath zfp)
      throws IOException {

    ZipEntryInfo zei = null;
    zei = entries.get(zfp);
    if (zei != null) {
      if (zfp.getNameCount() == 0) { //zfp.equals(zfp.getRoot())
        Path p = Paths.get(zfp.getFileSystem().getZipFileSystemFile());
        try {
          long time = VASSAL.tools.nio.file.attribute.Attributes
            .readBasicFileAttributes(p).lastModifiedTime().toMillis();
          zei.lastModifiedTime = javaTimeToDosTime(time);
        }
        catch (IOException e) {
          throw e;
        }
      }
      return zei;
    }
    for (ZipFilePath f : entries.keySet()) {
      if (f.startsWith(zfp)) {
        if (zfp.getNameCount() == f.getNameCount()) {
          zei = entries.get(f);
          return zei;
        }
      }
    }
    for (ZipFilePath f : entries.keySet()) {
      if (f.startsWith(zfp)) {
        if (zfp.getNameCount() < f.getNameCount()) {
          zei = new ZipEntryInfo(); //it is a path component in an entry,
          zei.isDirectory = true;   // jar/zip file won't contain any information
          zei.isRegularFile = false;  // about this dir component
          // Set directory as readable and executable
          zei.extAttrs = Integer.parseInt("-1111110101111111111111111111111", 2);
          boolean isJar = false;
          if (f.getEntryNameCount() > 1) {
            isJar = f.getParentEntry().getName().toString().toLowerCase().endsWith(".jar");
          }
          else {
            isJar = f.getFileSystem().getZipFileSystemFile().toLowerCase().endsWith(".jar");
          }

          if (isJar) {
            zei = new JarEntryInfo(zei);
          }
          return zei;
        }
      }
    }
    throw new NoSuchFileException(zfp.toString()); // no matching
  }

  private static long javaTimeToDosTime(long time) {
    Calendar cal = Calendar.getInstance();
    cal.setTimeInMillis(time);
    int year = cal.get(Calendar.YEAR);
    if (year < 1980) {
      return ((1 << 21) | (1 << 16));
    }
    return ((year - 1980) << 25 | (cal.get(Calendar.MONTH) + 1) << 21 |
        cal.get(Calendar.DAY_OF_MONTH) << 16 | cal.get(Calendar.HOUR_OF_DAY) << 11 | cal.get(Calendar.MINUTE) << 5 |
        cal.get(Calendar.SECOND) >> 1);
  }

  static void remove(URI uri) {
    cachedEntries.remove(uri);
  }

  /**
   * Extract the nested zips in a given file reference
   * by extracting the intermediate zip file contents to a temporary
   * location of the Platform file system
   */
  static FileRef extractNestedZip(ZipFilePath f) throws IOException {
    if (f.getEntryNameCount() == 0) {
      return null;
    }

    String zipFile = f.getFileSystem().getZipFileSystemFile();
    int end = f.getEntryNameCount();
    ZipFile zfile = null;
    for (int i = 0; i < end; i++) {
      try {
        String nestedZip = f.getEntryName(i).toString();
        zfile = new ZipFile(zipFile);
        ZipEntry entry = zfile.getEntry(nestedZip);
        if (entry == null) {
          throw new IOException("Invalid Zip Entry:" + nestedZip);
        }
        zipFile = readFileInZip(zfile.getInputStream(entry));
      }
      finally {
        zfile.close();
      }
    }

    FileSystem m = FileSystems.getDefault();
    FileRef retrievedZip = m.getPath(zipFile);
    return retrievedZip;
  }

  static String readFileInZip(InputStream entry) throws IOException {
    File tmpFile = null;
    try {
      BufferedInputStream zipStream = new BufferedInputStream(entry);

      // unzip the file contents to a temp directory
      String prefix = "zipfs";
      String suffix = String.valueOf((new Date()).getTime());
      tmpFile = File.createTempFile(prefix, suffix);
      FileOutputStream tmpOut = new FileOutputStream(tmpFile);
      byte buf[] = new byte[1024];
      int read;
      int offset = 0;
      while ((read = zipStream.read(buf)) > 0) {
        tmpOut.write(buf, 0, read);
        offset = offset + read;
      }
      zipStream.close();
      tmpOut.close();
      tmpFile.deleteOnExit();
    }
    catch (IOException e) {
      throw e;
    }
    return tmpFile.getAbsolutePath();
  }

  static ZipEntryInfo getFakeEntry(ZipFilePath zpath, Path rpath)
                                                           throws IOException {
    // build fake ZipEntryInfo from file
    final BasicFileAttributes attrs =
      VASSAL.tools.nio.file.attribute.Attributes.readBasicFileAttributes(rpath);

    final ZipEntryInfo ze = new ZipEntryInfo();

    ze.filename = zpath.toAbsolutePath().toString().getBytes();
    ze.compSize = -1;
// FIXME: int cast is a problem---test what happens when we try to write an
// archive containing a >4GB file 
    ze.size = (int) attrs.size();
    ze.isDirectory = attrs.isDirectory();
    ze.isOtherFile = attrs.isOther();
    ze.isRegularFile = attrs.isRegularFile();
    ze.lastModifiedTime = attrs.lastModifiedTime().toMillis();

    return ze;
  }

  static long dosToJavaTime(long time) {
    final Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
    cal.clear();  // to set the milliseconds 0
    cal.set((int) (((time >> 25) & 0x7f) + 1980),
            (int) (((time >> 21) & 0x0f) - 1), // Calendar months are 0-based
            (int) ( (time >> 16) & 0x1f),
            (int) ( (time >> 11) & 0x1f),
            (int) ( (time >>  5) & 0x3f),
            (int) ( (time <<  1) & 0x3e));
    return cal.getTimeInMillis();
  }

  static long javaToDosTime(long time) {
    final Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("UTC"));
    cal.setTimeInMillis(time);
   
    return (((cal.get(Calendar.YEAR) - 1980) << 25) |
            ((cal.get(Calendar.MONTH) + 1)   << 21) |
            ( cal.get(Calendar.DATE)         << 16) |
            ( cal.get(Calendar.HOUR_OF_DAY)  << 11) |
            ( cal.get(Calendar.MINUTE)       <<  5) |
            ( cal.get(Calendar.SECOND)       >>  1)) & 0xffffffffL;
  }
}
