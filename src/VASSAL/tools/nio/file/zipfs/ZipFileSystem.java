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
import VASSAL.tools.nio.file.attribute.*;
import VASSAL.tools.nio.file.spi.*;

import java.io.Closeable;
import java.io.File;
//import java.nio.file.*;
//import java.nio.file.attribute.*;
//import java.nio.file.spi.*;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import VASSAL.tools.io.IOUtils;

public class ZipFileSystem extends FileSystem {

  private final ZipFileSystemProvider provider;
  //path upto first zip file
  // for example in Win c:/foo/bar.zip/a/b/c - zipFile represents c:/foo/bar.zip
  // this contains real path following the links (change it, if no need to follow links)
  private final String zipFile;
  private final String defaultdir;
  private final ReentrantReadWriteLock closeLock = new ReentrantReadWriteLock();
  private boolean open = true;

  private final Set<Closeable> closeables =
    Collections.synchronizedSet(new HashSet<Closeable>());

  protected final ConcurrentMap<ZipFilePath,Path> real =
    new ConcurrentHashMap<ZipFilePath,Path>();

  // dummy value for real map
  static final Path DELETED = new ZipFilePath(null, null, null);

  protected final ConcurrentMap<ZipFilePath,ZipEntryInfo> info =
    new ConcurrentHashMap<ZipFilePath,ZipEntryInfo>();
 
  ZipFileSystem(ZipFileSystemProvider provider, FileRef fref) {
    this(provider, fref.toString(), "/");
  }

  ZipFileSystem(ZipFileSystemProvider provider, String path, String defaultDir) {
    this.provider = provider;
    this.zipFile = path;
    this.defaultdir = defaultDir;
  }

  @Override
  public FileSystemProvider provider() {
    return provider;
  }

  @Override
  public String getSeparator() {
    return "/";
  }

  @Override
  public boolean isOpen() {
    return open;
  }

  @Override
  public boolean isReadOnly() {
    return false;
  }

  @Override
  public void close() throws IOException {
    try {
      closeLock.writeLock().lock();

      if (!open) return;
      
      final URI root = getPath("/").toUri();
      implClose(root);

      flush();

      open = false;
    }
    finally {
      closeLock.writeLock().unlock();
    }
  }

  Path externalize(ZipFilePath path) throws IOException {
    try {
      writeLock(path);

      Path ext = getReal(path);
      if (ext == DELETED) throw new NoSuchFileException(path.toString());

      if (ext == null) {
        ext = createTempFile();
        path.copyTo(ext, StandardCopyOption.REPLACE_EXISTING);
        putReal(path, ext);
      }

      return ext;
    }
    finally {
      writeUnlock(path);
    }
  }

  Path createTempFile(FileAttribute<?>... attrs) throws IOException {
    final Path tmp =
      Paths.get(File.createTempFile("zipfs", "tmp").toString());

    tmp.deleteIfExists();
    tmp.createFile(attrs);

    return tmp;
  }

  Path createTempDirectory(FileAttribute<?>... attrs) throws IOException {
    final Path tmp =
      Paths.get(File.createTempFile("zipfs", "tmp").toString());

    tmp.deleteIfExists();
    tmp.createDirectory(attrs);

    return tmp;
  }

  public void flush() throws IOException {
    try {
      closeLock.writeLock().lock();

      // no modifications, nothing to do
      if (real.isEmpty()) return;

      // create a temp file into which to write the new ZIP archive
      final Path nzip =
        Paths.get(File.createTempFile("rwzipfs", "zip").toString());

      ZipOutputStream out = null;
      try {
        out = new ZipOutputStream(nzip.newOutputStream());

        ZipFile zf = null;
        try {
          zf = new ZipFile(getZipFileSystemFile());
        
          // copy all unchanged files from old archive to new archive
          final Enumeration<? extends ZipEntry> en = zf.entries();
          while (en.hasMoreElements()) {
            final ZipEntry e = en.nextElement();
            final ZipFilePath path = getPath(e.toString());
    
            final Path rpath = getReal(path);
            if (rpath != null) continue;

            if (Boolean.TRUE.equals(path.getAttribute("isDirectory"))) {
              out.putNextEntry(e);
            }
            else {
              InputStream in = null;
              try {
                in = zf.getInputStream(e);
         
                // We can't reuse entries for compressed files because
                // there's no way to reset the fields to acceptable values. 
                final ZipEntry ze = new ZipEntry(e.getName());
                ze.setMethod(ZipEntry.DEFLATED);

// FIXME: preserve attribs?
                out.putNextEntry(ze);
                IOUtils.copy(in, out);
                out.closeEntry();
                in.close();
              }
              finally {
                IOUtils.closeQuietly(in);
              }
            }
          }
        
          zf.close();
        }
        finally {
          IOUtils.closeQuietly(zf);
        }

        // copy all new files to new archive
        for (Map.Entry<ZipFilePath,Path> e : real.entrySet()) {
          final Path rpath = e.getValue();
          if (rpath == DELETED) continue;  // path was deleted, skip

// FIXME: preserve attribs?
          final ZipEntry ze = new ZipEntry(rpath.toString());
          if (Boolean.TRUE.equals(rpath.getAttribute("isDirectory"))) {
            out.putNextEntry(ze);
          }
          else {
            ze.setMethod(ZipEntry.DEFLATED);

            InputStream in = null;
            try {
              in = rpath.newInputStream();
              out.putNextEntry(ze);
              IOUtils.copy(in, out);
              out.closeEntry();
              in.close();
            }
            finally {
              IOUtils.closeQuietly(in);
            }
          }
        }
  
        out.close();
      }
      finally {
        IOUtils.closeQuietly(out);
      }

      final Path ozip = Paths.get(getZipFileSystemFile());

      // replace the old archive with the new one
      nzip.moveTo(ozip, StandardCopyOption.REPLACE_EXISTING);

      // blow away cached header info
      ZipUtils.remove(getPath("/").toUri());
      info.clear();

      // delete external versions of new files
      for (Path rpath : real.values()) {
        if (rpath != DELETED) rpath.delete();
      }
      real.clear();
    }
    finally {
      closeLock.writeLock().unlock();
    }
  }

  protected final ConcurrentMap<Path,ReentrantReadWriteLock> locks =
    new ConcurrentHashMap<Path,ReentrantReadWriteLock>();

  void readLock(Path path) {
    // acquire read lock on root
    begin();

    // acquire read lock on path
    path = path.toAbsolutePath();

    final ReentrantReadWriteLock nl = new ReentrantReadWriteLock();
    ReentrantReadWriteLock l = locks.putIfAbsent(path, nl);
    if (l == null) l = nl;

    l.readLock().lock();
  }

  void readUnlock(Path path) {
    path = path.toAbsolutePath();

    // release read lock on path
    locks.get(path).readLock().unlock();

    // release read lock on root
    end(); 
  }

  void writeLock(Path path) {
    // acquire read lock on root
    begin(); 

    path = path.toAbsolutePath();

    // acquire write lock on path
    final ReentrantReadWriteLock nl = new ReentrantReadWriteLock();
    ReentrantReadWriteLock l = locks.putIfAbsent(path, nl);
    if (l == null) l = nl;

    l.writeLock().lock();
  }

  void writeUnlock(Path path) {
    path = path.toAbsolutePath();

    // release write lock on path
    locks.get(path).writeLock().unlock();

    // release read lock on root
    end(); 
  }

  void begin() {
    closeLock.readLock().lock();
    if (!isOpen()) {
      throw new ClosedFileSystemException();
    }
  }

  void end() {
    closeLock.readLock().unlock();
  }

  Path getReal(ZipFilePath zpath) {
    return real.get(zpath.toAbsolutePath());
  }

  Path putReal(ZipFilePath zpath, Path rpath) throws IOException {
    zpath = zpath.toAbsolutePath();
    removeInfo(zpath);
    final Path old = real.put(zpath, rpath);
    if (old != null && old != DELETED) old.delete();
    return old;
  }

  Path removeReal(ZipFilePath zpath) {
    zpath = zpath.toAbsolutePath();
    removeInfo(zpath);
    return real.remove(zpath);
  }

  ZipEntryInfo getInfo(ZipFilePath zpath) {
    return info.get(zpath.toAbsolutePath());
  }

  ZipEntryInfo putInfo(ZipFilePath zpath, ZipEntryInfo zinfo) {
    return info.put(zpath.toAbsolutePath(), zinfo);
  }

  ZipEntryInfo removeInfo(ZipFilePath zpath) {
    return info.remove(zpath.toAbsolutePath());
  }

// FIXME
  // Free all cached Zip/Jar files
  private void implClose(URI root) throws IOException {
    ZipUtils.remove(root); // remove cached filesystem
    provider.removeFileSystem(root);

    final Iterator<Closeable> i = closeables.iterator();
    while (i.hasNext()) {
      i.next().close();
      i.remove();
    }
  }

  boolean registerCloseable(Closeable obj) {
    return closeables.add(obj);
  }

  boolean unregisterCloseable(Closeable obj) {
    return closeables.remove(obj);
  }

  @Override
  public Iterable<Path> getRootDirectories() {
    try {
      begin();
      ArrayList<Path> pathArr = new ArrayList<Path>();
      ZipFilePath root = new ZipFilePath(this, new byte[]{'/'});
      pathArr.add(root);
      return pathArr;
    }
    finally {
      end();
    }
  }

  String getDefaultDir() {
    return defaultdir;
  }

  String getZipFileSystemFile() {
    return zipFile;
  }

  @Override
  public ZipFilePath getPath(String path) {
    if (path == null) {
      throw new NullPointerException();
    }
    if (path.equals("")) {
      throw new InvalidPathException(path, "path should not be empty");
    }

    try {
      begin();
      final byte[] parsedPath = ZipPathParser.normalize(path).getBytes();
      return new ZipFilePath(this, parsedPath);
    }
    finally {
      end();
    }
  }

  @Override
  public UserPrincipalLookupService getUserPrincipalLookupService() {
    return null;
  }

  @Override
  public WatchService newWatchService() {
    throw new UnsupportedOperationException();
  }

  @Override
  public Iterable<FileStore> getFileStores() {
    try {
      begin();
      Iterator<Path> iterator = this.getRootDirectories().iterator();
      final ZipFileStoreIterator zipIterator = new ZipFileStoreIterator(iterator);
      return new Iterable<FileStore>() {

        public Iterator<FileStore> iterator() {
          return zipIterator;
        }
      };
    }
    finally {
      end();
    }
  }

// FIXME: What is this for?!
  private static class ZipFileStoreIterator implements Iterator<FileStore> {

    private final Iterator<Path> roots;
    private FileStore next;

    ZipFileStoreIterator(Iterator<Path> root) {
      roots = root;
    }

    private FileStore readNext() {
      for (;;) {
        if (!roots.hasNext()) {
          return null;
        }
        try {
          ZipFilePath root = (ZipFilePath) roots.next();
          FileStore fs = ZipFileStore.create(root);
          if (fs != null) {
            return fs;
          }
        }
        catch (IOException e) {
        }
      }
    }

    public synchronized boolean hasNext() {
      if (next != null) {
        return true;
      }
      next = readNext();
      return (next != null);

    }

    public synchronized FileStore next() {
      if (next == null) {
        next = readNext();
      }
      if (next == null) {
        throw new NoSuchElementException();
      }
      else {
        FileStore result = next;
        next = null;
        return result;
      }
    }

    public void remove() {
      throw new UnsupportedOperationException("");
    }
  }

  private static final Set<String> supportedFileAttributeViews =
    Collections.unmodifiableSet(
    new HashSet<String>(Arrays.asList("basic", "zip", "jar")));

  @Override
  public Set<String> supportedFileAttributeViews() {
    return supportedFileAttributeViews;
  }

  @Override
  public String toString() {
    return getZipFileSystemFile();
  }

  @Override
  public PathMatcher getPathMatcher(String syntaxAndInput) {
    int pos = syntaxAndInput.indexOf(':');
    if (pos <= 0 || pos == syntaxAndInput.length()) {
      throw new IllegalArgumentException();
    }
    String syntax = syntaxAndInput.substring(0, pos);
    String input = syntaxAndInput.substring(pos + 1);

    if (syntax.equalsIgnoreCase("glob")) {

     // This can be implemented in following ways.
     // 1. Create a regex expression from the glob syntax - Takes time. Lets do it afterwards.
     //
     // 2. Hack the code sun.nio.fs.Globes uisng Reflection- This is not Good.
     //     Class gc = Globs.class;
     //     Method m = gc.getDeclaredMethod("toUnixRegexPattern",String.class);
     //     m.setAccessible(true); // As this method is package prviate
     //     String s = (String)m.invoke(null, "*.java");
     //     Pattern pattern = Pattern.compile ( s );
     //
     // 3. GetDefault file systems matcher - This is very simple. It servers the purpose.
     //  Lets do it.

      PathMatcher matcher = FileSystems.getDefault().getPathMatcher(syntaxAndInput);
      return matcher;

    }
    else if (!syntax.equalsIgnoreCase("regex")) {
      throw new UnsupportedOperationException("Syntax '" + syntax +
          "' not recognized");
    }
    final Pattern pattern = Pattern.compile(input);
    return new PathMatcher() {

      public boolean matches(Path path) {
        // match on file name only
        Path name = path.getName();
        if (name == null) {
          return false;
        }
        return pattern.matcher(name.toString()).matches();
      }
    };
  }
}
