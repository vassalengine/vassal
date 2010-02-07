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

import java.io.File;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
//import java.nio.channels.SeekableByteChannel;
//import java.nio.file.*;
//import java.nio.file.DirectoryStream.Filter;
//import java.nio.file.spi.*;
import java.util.*;
//import java.nio.file.attribute.*;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
//import java.nio.file.attribute.Attributes;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import VASSAL.tools.nio.channels.FileChannelAdapter;
import VASSAL.tools.nio.channels.SeekableByteChannel;
import VASSAL.tools.nio.file.*;
import VASSAL.tools.nio.file.DirectoryStream.Filter;
import VASSAL.tools.nio.file.attribute.*;
import VASSAL.tools.nio.file.spi.*;

import static VASSAL.tools.nio.file.zipfs.ZipFileSystem.DELETED;

import VASSAL.tools.io.IOUtils;

/**
 * Jar/Zip path implementation of Path
 * We use "/" as the Zip File entry seperator.
 * @author    Rajendra Gutupalli,Jaya Hangal
 */
public class ZipFilePath extends Path {

  private ZipFileSystem fs;
  //zip file separator
  public static final String separator = "/";
  // path inside zip and it can contain nested zip/jar paths
  private final byte[] path;
  // array of offsets of components in path - created lazily
  private volatile ArrayList<Integer> offsets;
  // array of offsets of entry elements in the path
  private volatile ArrayList<Integer> entryOffsets;
  // resolved path for locating zip inside zip file
  // resloved path does not contain ./ and .. components
  private final byte[] pathForZip;
  private ZipFilePath pathToZip;
  private final byte[] pathForPrint;

  // package-private
  ZipFilePath(ZipFileSystem fs, byte[] pathInZip) {
    this.fs = fs;
    this.path = pathInZip;
    this.pathForPrint = pathInZip;
    boolean isAbs = (path[0] == '/');
    String toResolve = new String(path);
    if (!isAbs) {
      String defdir = fs.getDefaultDir();
      boolean endsWith = defdir.endsWith("/");
      if (endsWith) {
        toResolve = defdir + toResolve;
      }
      else {
        toResolve = defdir + "/" + toResolve;
      }
    }
    pathForZip = ZipPathParser.resolve(toResolve).getBytes();
  }

  // if given path is resolved
  ZipFilePath(ZipFileSystem fs, byte[] pathInZip, byte[] pathForZip) {
    this.fs = fs;
    this.path = pathForZip;
    this.pathForZip = pathForZip;
    this.pathForPrint = pathInZip; //given path
  }

  private ZipFilePath checkPath(Path path) {
    if (path == null) {
      throw new NullPointerException();
    }
  
    if (!(path instanceof ZipFilePath)) {
      throw new ProviderMismatchException();
    }

    return (ZipFilePath) path;
  }

  public boolean isNestedZip() {
    Pattern pattern = Pattern.compile("\\.(?i)(zip|jar)");
    Matcher matcher = null;
    for (int i = 0; i < getNameCount(); i++) {
      String entry = getName(i).toString();
      matcher = pattern.matcher(entry);
      if (matcher.find()) {
        return true;
      }
    }
    return false;
  }

  public boolean isArchiveFile() {
    final Path name = getName();
    if (name == null) {
      return false;
    }
    String fileName = name.toString().toLowerCase();
    return (fileName.endsWith(".zip") || fileName.endsWith(".jar"));
  }

  /**
   * A path represents directory if it ends with '/'.
   * The normalize method does not remove the trailing '/'
   */
  public boolean isDirectory() {
    try {
// FIXME
      fs.begin();
      try {
        ZipFilePath resolved = getResolvedPathForZip();
        return Attributes.readBasicFileAttributes(resolved, LinkOption.NOFOLLOW_LINKS).isDirectory();
      }
      catch (IOException e) {
        return false;
      }
    }
    finally {
      fs.end();
    }
  }

  static int nextSeparator(byte[] path, int index) {
    final int length = path.length;
    while (index < length && path[index] != '/') {
      index++;
    }
    return index;
  }

  static int nextNonSeparator(byte[] path, int index) {
    final int length = path.length;
    while (index < length && path[index] == '/') {
      index++;
    }
    return index;
  }

  // create offset list if not already created
  private void initOffsets() {
    if (offsets == null) {
      final ArrayList<Integer> list = new ArrayList<Integer>();
      final int pathLen = path.length;
      int index = nextNonSeparator(path, 0) - 1;

      int root = index;

      while ((index = nextSeparator(path, index + 1)) < pathLen && (index + 1 != pathLen)) {
        list.add(index + 1); // plus 1 for file separator
      }

      if (root + 1 < index) { // begin index
        list.add(0, root + 1);
      }

      offsets = list;
    }
  }

  private void initEntryOffsets() {
    if (entryOffsets == null) {

      ArrayList<Integer> list1 = new ArrayList<Integer>();
      int count = getNameCount();
      Pattern pattern = Pattern.compile("\\.(?i)(zip|jar)");
      Matcher matcher = null;
      int i = 0;
      int off = 0;
      while (i < (count - 1)) {
        String name = getName(i).toString();
        matcher = pattern.matcher(name);
        if (matcher.find()) {
          off = offsets.get(i + 1);
          list1.add(off);
        }
        i++;
      }
      if (count > 0) {
        int firstNonSeparatorIndex = nextNonSeparator(path, 0);
        list1.add(0, firstNonSeparatorIndex);
      }
      entryOffsets = list1;
    }
  }

  @Override
  public ZipFilePath getRoot() {
    if (this.isAbsolute()) {
      return new ZipFilePath(this.fs, new byte[]{path[0]});
    } 
    else {
      return null;
    }
  }

  @Override
  public Path getName() {
    initOffsets();
    if (offsets.size() == 0) {
      return null;
    }
    String result = subString(offsets.get(offsets.size() - 1), path.length);
    result = (result.endsWith("/")) ? result.substring(0, result.length() - 1) : result;
    return new ZipFilePath(this.fs, result.getBytes());
  }

  public ZipFilePath getEntryName() {
    initEntryOffsets();
    if (entryOffsets.size() == 0) {
      return null;
    }
    String result = subString(entryOffsets.get(entryOffsets.size() - 1), path.length);
    result = (result.endsWith("/")) ? result.substring(0, result.length() - 1) : result;
    return new ZipFilePath(this.fs, result.getBytes());

  }

  @Override
  public ZipFilePath getParent() {
    final int count = getNameCount();

    // a root has no parent
    if (count == 0) {
      return null;
    }

    // a single-name path
    if (count == 1) {
      // has the root as its parent if absolute, and no parent otherwise
      return isAbsolute() ?
        new ZipFilePath(this.fs, new byte[]{path[0]}) : null;
    }

    // all other paths have a parent
    int position = offsets.get(count - 1);
    String parent = subString(0, position - 1);
    return new ZipFilePath(this.fs, parent.getBytes());
  }

  public ZipFilePath getParentEntry() {
    int entryCount = getEntryNameCount();
    if (entryCount == 0 || entryCount == 1) {
      return null;
    }
    int position = entryOffsets.get(entryCount - 1);
    String parent = subString(0, position - 1);
    byte[] parentBytes = parent.getBytes();
    ZipFilePath path1 = new ZipFilePath(this.fs, parentBytes);
    return path1;
  }

  @Override
  public int getNameCount() {
    initOffsets();
    return offsets.size();
  }

  public int getEntryNameCount() {
    initEntryOffsets();
    return entryOffsets.size();
  }

  @Override
  public ZipFilePath getName(int index) {
    initOffsets();
    if (index < 0 || index >= offsets.size()) {
      throw new IllegalArgumentException();
    }

    if (index == offsets.size() - 1) {
      String s = subString(offsets.get(index), path.length);
      s = (s.endsWith("/")) ? s.substring(0, s.length() - 1) : s;
      return new ZipFilePath(this.fs, s.getBytes());
    }

    byte[] pathInBytes = subString(offsets.get(index), offsets.get(index + 1) - 1).getBytes();
    return new ZipFilePath(this.fs, pathInBytes);
  }

  public ZipFilePath getEntryName(int index) {
    initEntryOffsets();
    if (index < 0 || index >= entryOffsets.size()) {
      throw new IllegalArgumentException();
    }
    if (index == entryOffsets.size() - 1) {
      String s = subString(entryOffsets.get(index), path.length);
      s = (s.endsWith("/")) ? s.substring(0, s.length() - 1) : s;
      return new ZipFilePath(this.fs, s.getBytes());
    }
    byte[] pathInBytes = subString(entryOffsets.get(index), entryOffsets.get(index + 1) - 1).getBytes();
    return new ZipFilePath(this.fs, pathInBytes);
  }

  String subString(int beginIndex, int endIndex) {
    int length = endIndex - beginIndex;
    byte[] arr = new byte[length];
    System.arraycopy(path, beginIndex, arr, 0, length);
    return new String(arr);
  }

  @Override
  public ZipFilePath subpath(int beginIndex, int endIndex) {
    initOffsets();
    if (beginIndex < 0) {
      throw new IllegalArgumentException();
    }

    if (beginIndex >= (1 + offsets.size())) {
      throw new IllegalArgumentException();
    }

    if (endIndex > (1 + offsets.size())) {
      throw new IllegalArgumentException();
    }

    if (beginIndex >= endIndex) {
      throw new IllegalArgumentException();
    }

    if (offsets.size() == 0) {
      throw new IllegalArgumentException();
    }

    String result = subString(
      offsets.get(beginIndex),
      endIndex < offsets.size() ? offsets.get(endIndex) : path.length
    );

    if (result.endsWith("/")) result = result.substring(0, result.length()-1);

    return new ZipFilePath(fs, result.getBytes());
  }

  public ZipFilePath subEntryPath(int beginIndex, int endIndex) {
    initEntryOffsets();
    if (beginIndex < 0) {
      throw new IllegalArgumentException();
    }
    if (beginIndex >= (1 + entryOffsets.size())) {
      throw new IllegalArgumentException();
    }
    if (endIndex > (1 + entryOffsets.size())) {
      throw new IllegalArgumentException();
    }
    if (beginIndex >= endIndex) {
      throw new IllegalArgumentException();
    }

    int elements = endIndex - beginIndex;
    String result = null;
    StringBuilder result1 = new StringBuilder("");
    int index = beginIndex;

    for (; elements-- != 0;) {
      if (endIndex == entryOffsets.size() && elements == 0) {
        result1.append(subString(entryOffsets.get(index), path.length));
        break;
      }
      result1.append(subString(entryOffsets.get(index), entryOffsets.get(++index)));
    }
    result = result1.toString();
    result = (result.endsWith("/")) ? result.substring(0, result.length() - 1) : result;
    return new ZipFilePath(fs, result.getBytes());
  }

  @Override
  public ZipFilePath toRealPath(boolean resolveLinks) throws IOException {
    try {
      fs.readLock(this);
      return toRealPathImpl(resolveLinks);
    }
    finally {
      fs.readUnlock(this);
    }
  }

  protected ZipFilePath toRealPathImpl(boolean resolveLinks)
                                                           throws IOException {
    final ZipFilePath realPath = new ZipFilePath(this.fs, pathForZip);
    realPath.checkAccess();
    return realPath;
  }

  @Override
  public boolean isHidden() {
    return false;
  }

  @Override
  public ZipFilePath toAbsolutePath() {
    if (isAbsolute()) {
      return this;
    } 
    else {
      // add / before the existing path
      byte[] defaultdir = fs.getDefaultDir().getBytes();
      int defaultlen = defaultdir.length;
      boolean endsWith = (defaultdir[defaultlen - 1] == '/');
      byte[] t = null;
      if (endsWith) {
        t = new byte[defaultlen + path.length];
      }
      else {
        t = new byte[defaultlen + 1 + path.length];
      }

      System.arraycopy(defaultdir, 0, t, 0, defaultlen);
      if (!endsWith) {
        t[defaultlen++] = '/';
      }
      System.arraycopy(path, 0, t, defaultlen, path.length);
      return new ZipFilePath(this.fs, t);
    }
  }

  @Override
  public URI toUri() {

    String fullPath = fs.getZipFileSystemFile();
    if (File.separatorChar == '\\') {
      fullPath = "/" + fullPath.replace("\\", "/"); // if Windows replace all separators by '/'
    }
    boolean endsWithSlash = (path[path.length - 1] == '/');  //
    byte[] t = this.path;
    if (!endsWithSlash) {
      if (this.isArchiveFile() || this.isDirectory()) {
        t = new byte[path.length + 1];
        System.arraycopy(path, 0, t, 0, path.length);
        t[t.length - 1] = '/';
      }
    }
    String pathStr = new String(t);
    if (!isAbsolute()) {
      String defaultdir = fs.getDefaultDir();
      if (defaultdir.endsWith("/")) {
        pathStr = defaultdir + pathStr;
      } else {
        pathStr = defaultdir + "/" + pathStr;
      }
    }
    try {
      return new URI("zip", "", fullPath, pathStr);
    }
    catch (Exception ex) {
      throw new AssertionError(ex);
    }
  }

  // package private

  URI toUri0() {
    try {
      String fullPath = fs.getZipFileSystemFile();
      if (File.separatorChar == '\\') {
        fullPath = "/" + fullPath.replace("\\", "/"); // if Windows replace all separators by '/'
      }
      boolean endsWithSlash = (path.length > 1 && path[path.length - 1] == '/');  //0 for root
      byte[] t = this.path;
      if (!endsWithSlash && this.isArchiveFile()) {
        t = new byte[path.length + 1];
        System.arraycopy(path, 0, t, 0, path.length);
        t[t.length - 1] = '/';
      }
      String pathStr = new String(t);
      if (!isAbsolute()) {
        String defaultdir = fs.getDefaultDir();
        if (defaultdir.endsWith("/")) {
          pathStr = defaultdir + pathStr;
        }
        else {
          pathStr = defaultdir + "/" + pathStr;
        }
      }
      return new URI("zip", "", fullPath, pathStr);
    } 
    catch (Exception ex) {
      throw new AssertionError(ex);
    }
  }

  @Override
  public Path relativize(Path other) {
    final ZipFilePath otherPath = checkPath(other); 

    if (otherPath.equals(this)) {
      return null;
    }

    if (this.isAbsolute() != otherPath.isAbsolute()) {
      throw new IllegalArgumentException();
    }

    int i = 0;
    int ti = this.getNameCount();
    int oi = otherPath.getNameCount();

    for (; i < ti && i < oi; i++) {
      if (!this.getName(i).equals(otherPath.getName(i))) {
        break;
      }
    }
    int nc = ti - i;
    byte[] arr = new byte[nc * 3];
    for (int j = 0; j < arr.length; j += 3) {
      arr[j] = arr[j + 1] = '.';
      arr[j + 2] = '/';
    }
    //contruct final path
    ZipFilePath subPath = null;
    int subpathlen = 0;
    if (i < oi) {
      subPath = otherPath.subpath(i, oi);
      subpathlen = subPath.path.length;
    }
    byte[] result = new byte[arr.length + subpathlen];
    if (nc > 0) {
      System.arraycopy(arr, 0, result, 0, arr.length - 1);
    }
    if (subpathlen > 0) {
      if (arr.length > 0) {
        result[arr.length - 1] = '/';
      }
      System.arraycopy(subPath.path, 0, result, arr.length, subpathlen);
    }
    return new ZipFilePath(this.fs, result);
  }

  @Override
  public ZipFileSystem getFileSystem() {
    return fs;
  }

  @Override
  public boolean isAbsolute() {
    return (this.path[0] == '/');
  }

  public ZipFilePath resolve(Path other) {
    if (other == null) return this;

    if (!(other instanceof ZipFilePath)) {
      throw new ProviderMismatchException();
    }

    // zip/jar path are always absolute
    ZipFilePath other1 = (ZipFilePath) other;
    if (other1.isAbsolute()) {
      return other1;
    }

    byte[] resolved = null;
    if (this.path[path.length - 1] == '/') {
      resolved = new byte[path.length + other1.path.length];
      System.arraycopy(path, 0, resolved, 0, path.length);
      System.arraycopy(other1.path, 0, resolved, path.length, other1.path.length);
    }
    else {
      resolved = new byte[path.length + 1 + other1.path.length];
      System.arraycopy(path, 0, resolved, 0, path.length);
      resolved[path.length] = '/';
      System.arraycopy(other1.path, 0, resolved, path.length + 1, other1.path.length);
    }
    return new ZipFilePath(this.fs, resolved);
  }

  @Override
  public ZipFilePath resolve(String other) {
    return resolve(getFileSystem().getPath(other));
  }

  @Override
  public boolean startsWith(Path other) {
    final ZipFilePath that = checkPath(other);

    if (that.isAbsolute() != this.isAbsolute()) {
      return false;
    }

    final int thatCount = that.getNameCount();
    if (getNameCount() < thatCount) {
      return false;
    }
      
    for (int i = 0; i < thatCount; i++) {
      if (that.getName(i).equals(getName(i))) {
        continue;
      }
      else {
        return false;
      }
    }

    return true;
  }

  @Override
  public boolean endsWith(Path other) {
    final ZipFilePath that = checkPath(other);

    if (that.isAbsolute()) {
      return this.isAbsolute() ? this.equals(that) : false;
    }

    int i = that.getNameCount();
    int j = this.getNameCount();

    if (j < i) {
      return false;
    }
  
    for (--i, --j; i >= 0; i--, j--) {
      if (that.getName(i).equals(this.getName(j))) {
        continue;
      }
      else {
        return false;
      }
    }

    return true;
  }

  public FileSystemProvider provider() {
    return fs.provider();
  }

  @Override
  public String toString() {
    return new String(pathForPrint);
  }

  @Override
  public int hashCode() {
    int hashCode = 0;
    int i = 0;

    while (i < path.length) {
      final byte v = path[i];
      hashCode = hashCode * 31 + v;
      i++;
    }
    return hashCode;
  }

  @Override
  public boolean equals(Object ob) {
    if (ob != null && ob instanceof ZipFilePath) {
      return compareTo((Path) ob) == 0;
    }
    return false;
  }

  @Override
  public int compareTo(Path other) {
    final ZipFilePath otherPath = checkPath(other);

    int len1 = path.length;
    int len2 = otherPath.path.length;

    int n = Math.min(len1, len2);
    byte v1[] = path;
    byte v2[] = otherPath.path;

    int k = 0;
    while (k < n) {
      int c1 = v1[k];
      int c2 = v2[k];
      if (c1 != c2) {
        return c1 - c2;
      }
      k++;
    }
    return len1 - len2;
  }

  @Override
  public Path createSymbolicLink(
      Path target, FileAttribute<?>... attrs) throws IOException {
    throw new UnsupportedOperationException("Not supported.");
  }

  @Override
  public Path createLink(
      Path existing) throws IOException {
    throw new UnsupportedOperationException("Not supported.");
  }

  @Override
  public Path readSymbolicLink() throws IOException {
    throw new UnsupportedOperationException("Not supported.");
  }

  @Override
  public Path createDirectory(FileAttribute<?>... attrs) throws IOException {
    try {
      fs.writeLock(this);
      return createDirectoryImpl(attrs);
    }
    finally {
      fs.writeUnlock(this);
    }
  }

  protected Path createDirectoryImpl(FileAttribute<?>... attrs)
                                                           throws IOException {
    if (existsImpl()) throw new FileAlreadyExistsException(toString());
    fs.putReal(this, fs.createTempDirectory(attrs));
    return this;
  }

  ZipFilePath getResolvedPathForZip() {
    if (pathToZip == null) {
      pathToZip = new ZipFilePath(fs, path, pathForZip);
    }
    return pathToZip;
  }

  @Override
  public InputStream newInputStream(OpenOption... options) throws IOException {
    try {
      fs.readLock(this);
      return newInputStreamImpl(options);
    }
    finally {
      fs.readUnlock(this);
    }
  }

  protected InputStream newInputStreamImpl(OpenOption... options)
                                                           throws IOException {
    // validate OpenOptions
    for (OpenOption o : options) {
      if (o != StandardOpenOption.READ) {
        throw new UnsupportedOperationException(
          "'" + o + "' is not a valid option");
      }
    } 

    final Path rpath = fs.getReal(this);
    if (rpath != null) {
      if (rpath == DELETED) throw new NoSuchFileException(toString());
      return ZipIO.wrapReadLocked(this, rpath.newInputStream(options));
    }
    else {
      final ZipFilePath realPath = getResolvedPathForZip();
      if (realPath.getNameCount() == 0) {
        throw new IOException("entry missing in the path");
      }

      return ZipIO.wrapReadLocked(this, ZipIO.in(this, options));
    }      
  }

  @Override
  public DirectoryStream<Path> newDirectoryStream(Filter<? super Path> filter)
                                                           throws IOException {
    try {
      fs.readLock(this);
      return newDirectoryStreamImpl(filter);
    }
    finally {
      fs.readUnlock(this);
    }
  }

  protected DirectoryStream<Path> newDirectoryStreamImpl(
                              Filter<? super Path> filter) throws IOException {
    return new ZipFileStream(getResolvedPathForZip(), filter);
  }

  private static final DirectoryStream.Filter<Path> acceptAllFilter =
      new DirectoryStream.Filter<Path>() {

        public boolean accept(Path entry) {
          return true;
        }
      };

  @Override
  public DirectoryStream<Path> newDirectoryStream() throws IOException {
    return newDirectoryStreamImpl();
  }

  protected DirectoryStream<Path> newDirectoryStreamImpl() throws IOException {
    return newDirectoryStreamImpl(acceptAllFilter);
  }

  @Override
  public DirectoryStream<Path> newDirectoryStream(String glob)
                                                           throws IOException {
    return newDirectoryStreamImpl(glob);
  }

  protected DirectoryStream<Path> newDirectoryStreamImpl(String glob)
                                                           throws IOException {
    if (glob.equals("*")) return newDirectoryStreamImpl();

    final PathMatcher matcher = getFileSystem().getPathMatcher("glob:" + glob);
    final DirectoryStream.Filter<Path> filter =
                                           new DirectoryStream.Filter<Path>() {
      public boolean accept(Path entry)  {
        return matcher.matches(entry.getName());
      }
    };
    return newDirectoryStreamImpl(filter);
  }

  protected void checkEmptyDirectory() throws IOException {
    if (Boolean.TRUE.equals(getAttribute("isDirectory"))) {
      DirectoryStream<Path> ds = null;
      try {
        ds = newDirectoryStreamImpl();
        if (ds.iterator().hasNext()) {
          throw new DirectoryNotEmptyException(toString());
        }
        ds.close();
      }
      finally {
        IOUtils.closeQuietly(ds);
      }
    }
  }

  @Override
  public void delete() throws IOException {
    try {
      fs.writeLock(this);
      deleteImpl();
    }
    finally {
      fs.writeUnlock(this);
    }
  }

  protected void deleteImpl() throws IOException {
    if (!exists()) throw new NoSuchFileException(toString());

    // delete only empty directories
    checkEmptyDirectory();

    fs.putReal(this, DELETED);
  }

  @Override
  public void deleteIfExists() throws IOException {
    try {
      fs.writeLock(this);
      deleteIfExistsImpl();
    }
    finally {
      fs.writeUnlock(this);
    }
  }

  protected void deleteIfExistsImpl() throws IOException {
    if (exists()) deleteImpl();
  }

  @SuppressWarnings("unchecked")
  public <V extends FileAttributeView> V getFileAttributeView(
                                        Class<V> type, LinkOption... options) {
    if (type == null) {
      throw new NullPointerException();
    }
    if (type == BasicFileAttributeView.class) {
      return (V) new ZipFileBasicAttributeView(this);
    }
    if (type == ZipFileAttributeView.class) {
      return (V) new ZipFileAttributeView(this);
    }
    if (type == JarFileAttributeView.class) {
      return (V) new JarFileAttributeView(this);
    }   
 
    return null;
  }

  private AttributeViewByName getFileAttributeView(String view) {
    if (view == null) {
      throw new NullPointerException();
    }
    if (view.equals("basic")) {
      return new ZipFileBasicAttributeView(this);
    }
    if (view.equals("zip")) {
      return new ZipFileAttributeView(this);
    }
    if (view.equals("jar")) {
      return new JarFileAttributeView(this);
    }
    return null;
  }

  protected String[] parseAttribute(String attribute) {
    final int colon = attribute.indexOf(':');

    if (colon == -1) {
      return new String[] { "basic", attribute };
    }
    else {
      return new String[] {
        attribute.substring(0, colon), 
        attribute.substring(colon+1, attribute.length())
      };
    }
  }

  @Override
  public void setAttribute(String attribute, Object value,
                                             LinkOption... options)
                                                           throws IOException {
    final String[] a = parseAttribute(attribute);

    final AttributeViewByName view = getFileAttributeView(a[0]);
    if (view == null) {
      throw new UnsupportedOperationException("view not supported");
    }

    view.setAttribute(a[1], value);
  }

  @Override
  public Object getAttribute(String attribute, LinkOption... options)
                                                           throws IOException {
    final String[] a = parseAttribute(attribute);

    final AttributeViewByName view = getFileAttributeView(a[0]);
    if (view == null) {
      throw new UnsupportedOperationException("view not supported");
    }

    return view.getAttribute(a[1]);
  }

  public Map<String,?> readAttributes(String attributes, LinkOption... options)
                                                           throws IOException {
    return readAttributes(attributes.split(","), options);
  }

  protected Map<String,?> readAttributes(String[] attributes,
                                        LinkOption... options)
                                                           throws IOException {
    final Map<String,Object> map = new HashMap<String,Object>();

    for (String attr : attributes) {
      final String[] a = parseAttribute(attr);

      if ("*".equals(a[1])) {
        if ("basic".equals(a[0])) {
          map.putAll(readAttributes(new String[] {
            "basic:lastModifiedTime",
            "basic:lastAccessTime",
            "basic:creationTime",
            "basic:size",
            "basic:isRegularFile",
            "basic:isDirectory",
            "basic:isSymbolicLink",
            "basic:isOther",
            "basic:fileKey"
          }));
        }
        else if ("zip".equals(a[0])) {
          map.putAll(readAttributes(new String[] {
            "zip:comment",
            "zip:compressedSize",
            "zip:crc",
            "zip:extra",
            "zip:method",
            "zip:name",
            "zip:isArchiveFile",
            "zip:versionMadeBy",
            "zip:extAttrs"
          }));
        }
        else if ("jar".equals(a[0])) {
          map.putAll(readAttributes(new String[] {
            "jar:manifestAttributes",
            "jar:entryAttributes"
          }));
        }
      }
      else {
// FIXME: inefficient, creates new view, attributes each time
        final AttributeViewByName view = getFileAttributeView(a[0]);
        if (view != null) { 
          map.put(a[1], view.getAttribute(a[1]));
        }
      }
    }
    
    return map;
  }

  @Override
  public FileStore getFileStore() throws IOException {
    try {
      fs.begin();
      if (isAbsolute()) {
        return ZipFileStore.create(getRoot());
      }
      else {
        return ZipFileStore.create(getResolvedPathForZip().getRoot());
      }
    }
    finally {
      fs.end();
    }
  }

  @Override
  public boolean isSameFile(Path other) throws IOException {
    if (this.equals(other)) {
      return true;
    }

    if (other == null || (!(other instanceof ZipFilePath))) {
      return false;
    }
  
    final ZipFilePath other1 = (ZipFilePath) other;

    // check whether we have a common file system
    if (!this.getFileSystem().getZipFileSystemFile().equals(
          other1.getFileSystem().getZipFileSystemFile())) {
      return false;
    }

    // check whether both (or neither) exist
    if (this.exists() != other1.exists()) {
      return false;
    }

    return this.toAbsolutePath().equals(other1.toAbsolutePath());
  }

  public WatchKey register(
      WatchService watcher,
      WatchEvent.Kind<?>[] events,
      WatchEvent.Modifier... modifiers) {
    if (watcher == null || events == null || modifiers == null) {
      throw new NullPointerException();
    }
    throw new ProviderMismatchException();
  }

  @Override
  public WatchKey register(WatchService watcher, WatchEvent.Kind<?>... events) {
    return register(watcher, events, new WatchEvent.Modifier[0]);
  }

  @Override
  public Iterator<Path> iterator() {
    return new Iterator<Path>() {

      private int i = 0;

      public boolean hasNext() {
        return (i < getNameCount());
      }

      public Path next() {
        if (i < getNameCount()) {
          Path result = getName(i);
          i++;

          return result;
        }
        else {
          throw new NoSuchElementException();
        }
      }

      public void remove() {
        throw new ReadOnlyFileSystemException();
      }
    };
  }

/*
  @Override
  public SeekableByteChannel newByteChannel(Set<? extends OpenOption> options,
                                            FileAttribute<?>... attrs)
                                                           throws IOException {
    try {
      
    }
    finally {

    }
  }
*/

  @Override
  public SeekableByteChannel newByteChannel(Set<? extends OpenOption> options,
                                            FileAttribute<?>... attrs)
                                                           throws IOException {
    // validate OpenOptions
    boolean read = false;
    boolean write = false;
    boolean append = false;
    boolean truncate_existing = false;
    boolean create_new = false;

    for (OpenOption o : options) {
      if (o instanceof StandardOpenOption) {
        switch ((StandardOpenOption) o) {
        case APPEND:            append            = true; break;
        case TRUNCATE_EXISTING: truncate_existing = true; break;
        case CREATE_NEW:        create_new        = true; break;
        case WRITE:             write             = true; break;
        case READ:              read              = true; break;
        case CREATE:                                      break;
        case DELETE_ON_CLOSE:
        case SPARSE:
        case SYNC:
        case DSYNC:
          throw new UnsupportedOperationException(
            "'" + o + "' is not a valid option");
        }
      }
      else {
        throw new UnsupportedOperationException(
          "'" + o + "' is not a valid option");
      }
    } 

    if (truncate_existing) {
      if (read) { 
        throw new UnsupportedOperationException(
          "READ is in incompatible with TRUNCATE_EXISTING");
      }
      else if (append) {
        throw new UnsupportedOperationException(
          "APPEND is in incompatible with TRUNCATE_EXISTING");
      }
    }

    if (write || append) {
      try {
        fs.writeLock(this);

        if (create_new) {
          if (exists()) throw new FileAlreadyExistsException(toString());

          // don't pass CREATE_NEW through to operations on temporary store
          options.remove(StandardOpenOption.CREATE_NEW);
        }

        Path rpath = fs.getReal(this);
        if (rpath == null || rpath == DELETED) {
          rpath = fs.createTempFile(attrs);
          fs.putReal(this, rpath);
        }

        final SeekableByteChannel ch = rpath.newByteChannel(options, attrs);
        return ZipIO.wrapWriteLocked(this, ch);
      }
      finally {
        fs.writeUnlock(this);
      }
    }
    else {
      try {
        fs.readLock(this);

        final Path rpath = fs.getReal(this);
        if (rpath != null) {
          if (rpath == DELETED) throw new NoSuchFileException(toString());
          final SeekableByteChannel ch = rpath.newByteChannel(options, attrs);
          return ZipIO.wrapReadLocked(this, ch);
        }
        else {
          final ZipFilePath zpath = getResolvedPathForZip();
          if (zpath.getNameCount() == 0) {
            throw new IOException("entry Not Found");
          }
          
          return ZipIO.wrapReadLocked(this, ZipIO.channel(this, options));
        }  
      }
      finally {
        fs.readUnlock(this);
      }
    }
  }

  @Override
  public SeekableByteChannel newByteChannel(OpenOption... options)
                                                           throws IOException {
    final Set<OpenOption> set = new HashSet<OpenOption>(options.length);
    Collections.addAll(set, options);
    return newByteChannel(set);
  }

  String getZipFile() throws IOException {

    String pathtoZip = null;
    ZipFilePath realPath = getResolvedPathForZip();
    int entryCount = realPath.getEntryNameCount();
    if (realPath.isNestedZip()) {
      if (realPath.isArchiveFile() && entryCount == 1) {
        pathtoZip = this.fs.getZipFileSystemFile();
      }
      else {
        pathtoZip = ZipUtils.extractNestedZip(realPath.getParentEntry()).toString();
      }
    }
    else {
      pathtoZip = this.fs.getZipFileSystemFile();
    }

    return pathtoZip;
  }

  @Override
  public void checkAccess(AccessMode... modes) throws IOException {
    try {
      fs.readLock(this);
      checkAccessImpl(modes);
    }
    finally {
      fs.readUnlock(this);
    }
  }

  protected void checkAccessImpl(AccessMode... modes) throws IOException {
    boolean w = false;
    boolean x = false;

    for (AccessMode mode : modes) {
      switch (mode) {
        case READ:
          break;
        case WRITE:
          w = true;
          break;
        case EXECUTE:
          x = true;
          break;
        default:
          throw new UnsupportedOperationException();
      }
    }

    final ZipFilePath resolvedZipPath = getResolvedPathForZip();
    final int nameCount = resolvedZipPath.getNameCount();
    if (nameCount == 0) {
      throw new NoSuchFileException(toString());
    }

    final Path rpath = fs.getReal(this);
    if (rpath != null) {
      // the path has been modified
      if (rpath == DELETED) throw new NoSuchFileException(toString());
      rpath.checkAccess(modes);
    }
    else {
      // the path is original to the ZIP archive
      final ZipEntryInfo ze = ZipUtils.getEntry(resolvedZipPath);

      if (w) {
        // check write access on archive file
        try {
          final Path zpath = Paths.get(fs.getZipFileSystemFile());
          zpath.checkAccess(AccessMode.WRITE);
        }
        catch (AccessDeniedException e) {
          throw (IOException) new AccessDeniedException(
            "write access denied for the file: " + toString()).initCause(e);
        }
        catch (IOException e) {
          throw (IOException) new IOException().initCause(e);
        }
      }

      if (x) {
        long attrs = ze.extAttrs;
        if (!((((attrs << 4) >> 24) & 0x04) == 0x04)) {
          throw new AccessDeniedException(
            "execute access denied for the file: " + this.toString());
        }
      }
    }
  }

  @Override
  public boolean exists() {
    try {
      fs.readLock(this);
      return existsImpl(); 
    }
    finally {
      fs.readUnlock(this);
    }
  }

  protected boolean existsImpl() {
    try {
      checkAccessImpl();
      return true;
    }
    catch (IOException x) {
      // unable to determine if file exists
    }
    return false;
  }

  @Override
  public boolean notExists() {
    try {
      fs.readLock(this);
      return notExistsImpl(); 
    }
    finally {
      fs.readUnlock(this);
    }
  }

  protected boolean notExistsImpl() {
    try {
      checkAccessImpl();
      return false;
    }
    catch (NoSuchFileException x) {
      // file confirmed not to exist
      return true;
    }
    catch (IOException x) {
      return false;
    }
  }

  /*
   * /foo//        -- >  /foo/
   * /foo/./         -- > /foo/
   * /foo/../bar       -- > /bar
   * /foo/../bar/../baz  -- > /baz
   * //foo//./bar      -- > /foo/bar
   * /../          -- > /
   * ../foo        -- > foo
   * foo/bar/..      -- > foo
   * foo/../../bar     -- > bar
   * foo/../bar      -- > bar
   **/
  @Override
  public Path normalize() {
    final String parsed = ZipPathParser.resolve(new String(path));

    return parsed.equals("") ? null :
      new ZipFilePath(fs, parsed.getBytes(), pathForZip);
  }

  @Override
  public Path createFile(FileAttribute<?>... attrs) throws IOException {
    try {
      fs.writeLock(this);
      return createFileImpl(attrs);
    }
    finally {
      fs.writeUnlock(this);
    }
  }

  protected Path createFileImpl(FileAttribute<?>... attrs) throws IOException {
    if (exists()) throw new FileAlreadyExistsException(toString());
    fs.putReal(this, fs.createTempFile(attrs));
    return this;
  }

  @Override
  public OutputStream newOutputStream(OpenOption... options)
                                                           throws IOException {
    // validate OpenOptions
    boolean append = false;
    boolean truncate_existing = false;
    boolean create_new = false;

    for (OpenOption o : options) {
      if (o instanceof StandardOpenOption) {
        switch ((StandardOpenOption) o) {
        case APPEND:            append = true;            break;
        case TRUNCATE_EXISTING: truncate_existing = true; break;
        case CREATE_NEW:        create_new = true;        break;
        case CREATE:                                      break;
        case WRITE:                                       break;
        case READ:
        case DELETE_ON_CLOSE:
        case SPARSE:
        case SYNC:
        case DSYNC:
          throw new UnsupportedOperationException(
            "'" + o + "' is not a valid option");
        }
      }
      else {
        throw new UnsupportedOperationException(
          "'" + o + "' is not a valid option");
      }
    } 

    if (append && truncate_existing) {
      throw new UnsupportedOperationException(
        "APPEND is in incompatible with TRUNCATE_EXISTING");
    }

    try {
      fs.writeLock(this);

      if (create_new) {
        if (exists()) throw new FileAlreadyExistsException(toString());

        // don't pass CREATE_NEW through to operations on temporary store
        final Set<OpenOption> oset =
          new HashSet<OpenOption>(Arrays.asList(options));
        
        oset.remove(StandardOpenOption.CREATE_NEW);
        options = oset.toArray(new OpenOption[oset.size()]);
      }

      Path rpath = fs.getReal(this);
      if (rpath == null || rpath == DELETED) {
        rpath = fs.createTempFile();
        fs.putReal(this, rpath);
      }

      return ZipIO.wrapWriteLocked(this, rpath.newOutputStream(options));
    }
    finally {
      fs.writeUnlock(this);
    }
  }

  @Override
  public Path moveTo(Path target, CopyOption... options) throws IOException {
    if (this.isSameFile(target)) return target;

    // validate CopyOptions
    boolean replace_existing = false;

    for (CopyOption option: options) {
      if (option == StandardCopyOption.REPLACE_EXISTING) {
        replace_existing = true;
      }
      else {
        throw new UnsupportedOperationException(
          "'" + option + "' is not a valid copy option");
      }
    }

    try {
      fs.writeLock(this);

      if (target.getFileSystem().provider() != fs.provider()) {
        // destination is not local
        if (replace_existing) {
          copyTo(target, StandardCopyOption.REPLACE_EXISTING,
                         StandardCopyOption.COPY_ATTRIBUTES);
        }
        else {
          copyTo(target, StandardCopyOption.COPY_ATTRIBUTES);
        }
      }
      else {
        try {
          // destination is local
          fs.writeLock(target);

          if (!replace_existing && target.exists()) {
            throw new FileAlreadyExistsException(target.toString());
          }

          final Path src = fs.removeReal(this);
          if (src != null) {
            // file is modified, just remap
            if (src == DELETED) throw new NoSuchFileException(toString());
            fs.putReal((ZipFilePath) target, src);
          }
          else {
            // file is unmodified, copy out to temp
            final Path dst = fs.createTempFile();
            copyTo(dst, StandardCopyOption.REPLACE_EXISTING,
                        StandardCopyOption.COPY_ATTRIBUTES);
          }
        }
        finally {
          fs.writeUnlock(target);
        }
      }
      
      deleteImpl();
      return target;
    }
    finally {
      fs.writeUnlock(this);
    }
  }

  @Override
  public Path copyTo(Path target, CopyOption... options) throws IOException {
    if (this.isSameFile(target)) return target;

    // validate CopyOptions
    boolean replace_existing = false;
    boolean copy_attributes = false;

    for (CopyOption o : options) {
      if (o == StandardCopyOption.REPLACE_EXISTING) {
        replace_existing = true;
      }
      else if (o == StandardCopyOption.COPY_ATTRIBUTES) {
        copy_attributes = true;
      }
      else {
        throw new UnsupportedOperationException(
          "'" + o + "' is not a valid copy option");
      }
    }

    try {
      fs.readLock(this);

      final Path src = fs.getReal(this);
      if (src != null) {
        // src is modified
        if (src == DELETED) throw new NoSuchFileException(toString());
      
        if (target.getFileSystem().provider() == fs.provider()) {
          // dst is local
          try {
            fs.writeLock(target);

            if (!replace_existing && target.exists()) {
              throw new FileAlreadyExistsException(target.toString());
            }

            Path dst = fs.getReal((ZipFilePath) target);
            if (dst == null || dst == DELETED) {
              dst = fs.createTempFile();
              fs.putReal((ZipFilePath) target, dst); 
            }

            if (copy_attributes) {
              src.copyTo(dst, StandardCopyOption.REPLACE_EXISTING,
                              StandardCopyOption.COPY_ATTRIBUTES);
            }
            else {
              src.copyTo(dst, StandardCopyOption.REPLACE_EXISTING);
            }
          }
          finally {          
            fs.writeUnlock(target);
          }
        }
        else {
          // dst is not local
          src.copyTo(target, options);
        }
      }
      else {
        // src is unmodified
        if (target.getFileSystem().provider() == fs.provider()) {
          // dst is local
          try {
            fs.writeLock(target);
      
            if (!replace_existing && target.exists()) {
              throw new FileAlreadyExistsException(target.toString());
            }

            Path dst = fs.getReal((ZipFilePath) target);
            if (dst == null || dst == DELETED) {
              dst = fs.createTempFile();
              fs.putReal((ZipFilePath) target, dst); 
            }

            copyToTarget(dst, copy_attributes);
          }
          finally {
            fs.writeUnlock(target);
          }
        }
        else {
          // dst is not local
          if (!replace_existing && target.exists()) {
            throw new FileAlreadyExistsException(target.toString());
          }

          copyToTarget(target, copy_attributes);
        }
      }
    }
    finally {
      fs.readUnlock(this);
    }

    return target;
  }

  protected void copyToTarget(Path target, boolean copyAttributes)
                                                           throws IOException {
    // read attributes of source file
    final BasicFileAttributes attrs = Attributes.readBasicFileAttributes(this);

    if (attrs.isDirectory()) {
// FIXME
      target.deleteIfExists();
      target.createDirectory();
    }
    else {
      InputStream in = newInputStream();
      try {
        // open target file for writing
        OutputStream out = target.newOutputStream();
        // simple copy loop
        try {
          final byte[] buf = new byte[8192];
          int n = 0;
          for (;;) {
            n = in.read(buf);
            if (n < 0)
              break;
            out.write(buf, 0, n);
          }
  
        }
        finally {
          out.close();
        }
      }
      finally {
        in.close();
      }
    }

    // copy basic attributes to target
    if (copyAttributes) {
      final BasicFileAttributeView view =
        target.getFileAttributeView(BasicFileAttributeView.class);

      try {
        view.setTimes(attrs.lastModifiedTime(),
                      attrs.lastAccessTime(),
                      attrs.creationTime());
      }
      catch (IOException e) {
        // rollback
        try {
// FIXME
          target.delete();
        }
        catch (IOException ignore) { }
        throw e;
      }
    }
  }
}
