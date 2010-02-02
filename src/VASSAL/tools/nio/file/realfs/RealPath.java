package VASSAL.tools.nio.file.realfs;

import static VASSAL.tools.nio.file.StandardCopyOption.ATOMIC_MOVE;
import static VASSAL.tools.nio.file.StandardCopyOption.REPLACE_EXISTING;
import static VASSAL.tools.nio.file.StandardOpenOption.APPEND;
import static VASSAL.tools.nio.file.StandardOpenOption.CREATE_NEW;
import static VASSAL.tools.nio.file.StandardOpenOption.READ;
import static VASSAL.tools.nio.file.StandardOpenOption.TRUNCATE_EXISTING;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import VASSAL.tools.io.IOUtils;

import VASSAL.tools.nio.channels.FileChannelAdapter;
import VASSAL.tools.nio.file.AbstractPath;
import VASSAL.tools.nio.file.AccessDeniedException;
import VASSAL.tools.nio.file.AccessMode;
import VASSAL.tools.nio.file.AtomicMoveNotSupportedException;
import VASSAL.tools.nio.file.CopyOption;
import VASSAL.tools.nio.file.DirectoryNotEmptyException;
import VASSAL.tools.nio.file.DirectoryStream;
import VASSAL.tools.nio.file.FileAlreadyExistsException;
import VASSAL.tools.nio.file.FileStore;
import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.FileSystemException;
import VASSAL.tools.nio.file.LinkOption;
import VASSAL.tools.nio.file.NoSuchFileException;
import VASSAL.tools.nio.file.NotDirectoryException;
import VASSAL.tools.nio.file.OpenOption;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.StandardOpenOption;
import VASSAL.tools.nio.file.StandardCopyOption;
import VASSAL.tools.nio.file.WatchEvent;
import VASSAL.tools.nio.file.WatchKey;
import VASSAL.tools.nio.file.WatchService;
import VASSAL.tools.nio.file.attribute.BasicFileAttributeView;
import VASSAL.tools.nio.file.attribute.FileAttribute;
import VASSAL.tools.nio.file.attribute.FileAttributeView;
import VASSAL.tools.nio.file.attribute.FileTime;

public abstract class RealPath extends AbstractPath {
  protected final File file;
  protected final RealFileSystem fs;

  public RealPath(String path, RealFileSystem fs) {
    super(new File(path).toString().getBytes());

    this.file = new File(path);
    this.fs = fs;
  }

  protected int[] splitPath(byte[] rawpath) {
    // File ctor removes duplicate separators. Hence, each
    // instance of separator splits two names.

    final ArrayList<Integer> sl = new ArrayList<Integer>();
    int i = 0;

    // find end of root, if present
    i = findRootSep(rawpath);
    sl.add(i++);

    // if at end, then we are just a root
    if (i >= rawpath.length) return new int[0];

    // record positions of all separators
    final byte[] sep = getFileSystem().getSeparator().getBytes();

// FIXME: note that this fails for multibyte seps
    for ( ; i < rawpath.length; ++i) {
      if (rawpath[i] == sep[0]) sl.add(i);
    }

    // record end of path
    sl.add(rawpath.length);

// FIXME: replace with a method in ArrayUtils or something from Apache Commons
    // convert from List<Integer> to int[]
    final int[] seps = new int[sl.size()];
    for (i = 0; i < seps.length; ++i) seps[i] = sl.get(i);
// END FIXME

    // The result is a list of offsets for the starts of the names, plus
    // a final element for the position of end of the path.
    return seps;
  }

  /**
   * Returns the position of the separator after the root element.
   *
   * @param s the {@code String} to check
   *
   * @return the position of the separator following the root element, or
   * -1 if the path is relative
   */
  protected abstract int findRootSep(byte[] s);
  
  public void checkAccess(AccessMode... modes) throws IOException {
    if (!file.exists()) throw new NoSuchFileException(file.toString());

    for (AccessMode m : modes) {
      switch (m) {
      case READ:
        if (!file.canRead()) {
          throw new AccessDeniedException(file.toString());
        }
        break;
      case WRITE:
        if (!file.canWrite()) {
          throw new AccessDeniedException(file.toString());
        }
        break;
      case EXECUTE:
      default:
        throw new UnsupportedOperationException(m.toString());
      }
    }
  }
  
  public Path createDirectory(FileAttribute<?>... attrs) throws IOException {
    if (attrs.length > 0) throw new UnsupportedOperationException();
    if (file.exists()) throw new FileAlreadyExistsException(file.toString());
    if (!file.mkdir()) throw new FileSystemException(file.toString());
    return this;
  }

  public Path createFile(FileAttribute<?>... attrs) throws IOException {
    if (attrs.length > 0) throw new UnsupportedOperationException();
    if (file.exists()) throw new FileAlreadyExistsException(file.toString());
    if (!file.createNewFile()) throw new FileSystemException(file.toString());
    return this;
  }

  public void delete() throws IOException {
    if (!file.exists()) throw new NoSuchFileException(file.toString());
    if (file.isDirectory() && file.list().length > 0)
      throw new DirectoryNotEmptyException(file.toString());
    if (!file.delete()) throw new FileSystemException(file.toString());
  }

  public void deleteIfExists() throws IOException {
    if (exists()) delete();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || !(o instanceof RealPath)) return false;
    return file.equals(((RealPath) o).file);
  }

  public boolean exists() {
    return file.exists();
  }

  public <V extends FileAttributeView> V getFileAttributeView(
    Class<V> type, LinkOption... options)
  {
    if (options.length > 0) throw new UnsupportedOperationException();
    if (!BasicFileAttributeView.class.equals(type)) return null;
    return type.cast(new RealFileAttributeView(this));
  }

  public Object getAttribute(String attribute, LinkOption... options)
                                                           throws IOException {
    if (options.length > 0) throw new UnsupportedOperationException();

    if (attribute.indexOf(':') == -1) attribute = "basic:" + attribute;
    final int colon = attribute.indexOf(':');
    final String vname = attribute.substring(0, colon);
    final String aname = attribute.substring(colon+1);

    if ("basic".equals(vname)) {
      final RealFileAttributeView view = new RealFileAttributeView(this);
      final RealFileAttributes attrs = view.readAttributes();

      if ("lastModifiedTime".equals(aname)) {
        return attrs.lastModifiedTime();
      }
      else if ("lastAccessTime".equals(aname)) {
       return attrs.lastAccessTime();
      }
      else if ("creationTime".equals(aname)) {
        return attrs.creationTime();
      }
      else if ("size".equals(aname)) {
        return attrs.size();
      }
      else if ("isRegularFile".equals(aname)) {
        return attrs.isRegularFile();
      }
      else if ("isDirectory".equals(aname)) {
        return attrs.isDirectory();
      }
      else if ("isOther".equals(aname)) {
        return attrs.isOther();
      }
      else if ("fileKey".equals(aname)) {
        return attrs.fileKey();
      }
    }

    return null;
  }

  public FileStore getFileStore() throws IOException {
    // RealFileSystem has only one FileStore
    return fs.getFileStores().iterator().next();
  }

  public FileSystem getFileSystem() {
    return fs;
  }

  @Override
  public int hashCode() {
    return file.hashCode();
  }

  public boolean isHidden() throws IOException {
    return file.isHidden();
  }

  public boolean isSameFile(Path other) {
    return this.equals(other);
  }

  public Path moveTo(Path target, CopyOption... options) throws IOException {
    if (!target.isSameFile(this)) {
      boolean replace = false;

      for (CopyOption c : options) {
        if (c == REPLACE_EXISTING) replace = true;
        else if (c == ATOMIC_MOVE) {
          throw new AtomicMoveNotSupportedException(
            this.toString(), target.toString(), ""
          );
        }
        else throw new UnsupportedOperationException(c.toString());
      }

      if (!replace && target.exists()) {
        throw new FileAlreadyExistsException(
          file.toString(), target.toString(), ""
        );
      }

      if (fs == target.getFileSystem()) {
        // we're on this fs, do the easy thing
        if (!file.renameTo(((RealPath) target).file)) {
          throw new FileSystemException(
            file.toString(), target.toString(), ""
          );
        }
      }
      else {
        // different fs: copy, then delete
        copyTo(target, options);
        delete();
      }
    }
    
    return target;
  }

  public FileChannelAdapter newByteChannel(OpenOption... options)
                                                           throws IOException {
    final Set<StandardOpenOption> opt = standardOpenOptionSet(options);
    if (opt.isEmpty()) opt.add(READ);

    return newByteChannel(opt);
  }

  protected Set<StandardOpenOption> standardOpenOptionSet(OpenOption... opts) {
    final Set<StandardOpenOption> opt =
      EnumSet.noneOf(StandardOpenOption.class);

    for (OpenOption o : opts) {
      if (o instanceof StandardOpenOption) {
        opt.add((StandardOpenOption) o);
      }
      else {
        throw new UnsupportedOperationException(o.toString());
      }
    }

    return opt;
  }

  public FileChannelAdapter newByteChannel(
    Set<? extends OpenOption> options, FileAttribute<?>... attrs)
                                                           throws IOException {
    return new FileChannelAdapter(
      fs.provider().newFileChannel(this, options, attrs));
  }

  public DirectoryStream<Path> newDirectoryStream() throws IOException {
    if (!file.isDirectory()) throw new NotDirectoryException(file.toString());
    return new RealDirectoryStream(this);
  }

  public DirectoryStream<Path> newDirectoryStream(
    DirectoryStream.Filter<? super Path> filter) throws IOException {

    if (!file.isDirectory()) throw new NotDirectoryException(file.toString());
    return new RealDirectoryStream(this, filter);
  }

  public DirectoryStream<Path> newDirectoryStream(String glob)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }

  public FileInputStream newInputStream(OpenOption... options)
                                                           throws IOException {
    for (OpenOption o : options) {
      if (o != StandardOpenOption.READ) {
        throw new UnsupportedOperationException(o.toString());
      }
    }

    try {
      return new FileInputStream(file);
    }
    catch (FileNotFoundException e) {
      throw (IOException) new NoSuchFileException(toString()).initCause(e);
    }
  }

  public FileOutputStream newOutputStream(OpenOption... options)
                                                           throws IOException {
    final Set<StandardOpenOption> opts = standardOpenOptionSet(options); 
    
    if (opts.contains(READ)) throw new IllegalArgumentException();

    if (opts.contains(CREATE_NEW) && file.exists())
      throw new FileAlreadyExistsException(file.toString());

    boolean append = false;
    if (opts.contains(APPEND)) {
      if (opts.contains(TRUNCATE_EXISTING))
        throw new IllegalArgumentException();
     
      append = true;
    }

    return new FileOutputStream(file, append);
  }

  public boolean notExists() {
    return !file.exists();
  }

  public Map<String,?> readAttributes(String attributes, LinkOption... options)
                                                           throws IOException {
    if (options.length > 0) throw new UnsupportedOperationException();

    final Map<String,Object> map = new HashMap<String,Object>();

    for (String attr : attributes.split(",")) {
      if ("*".equals(attr) || ("basic:*".equals(attr))) {
        map.putAll(readAttributes("basic:lastModifiedTime,basic:lastAccessTime,basic:creationTime,basic:size,basic:isRegularFile,basic:isDirectory,basic:isSymbolicLink,basic:isOther,basic:fileKey"));
        return map;
      }
      else {
        if (attr.indexOf(':') == -1) attr = "basic:" + attr;
        map.put(attr, getAttribute(attr));
      }
    }

    return map;
  }

  public void setAttribute(String attribute,
                           Object value, LinkOption... options)
                                                           throws IOException {
    if (options.length > 0) throw new UnsupportedOperationException();

    if (attribute.indexOf(':') == -1) attribute = "basic:" + attribute;
    final int colon = attribute.indexOf(':');
    final String vname = attribute.substring(0, colon);
    final String aname = attribute.substring(colon+1);
                                                       
    if ("basic".equals(vname)) {
      final RealFileAttributeView view = new RealFileAttributeView(this);

      if ("lastModifiedTime".equals(aname)) {
        view.setTimes((FileTime) value, null, null);
      }
      else {
        throw new UnsupportedOperationException(attribute);
      }
    }
    else {
      throw new UnsupportedOperationException(attribute);
    }   
  }

  public Path toAbsolutePath() {
    return fs.getPath(file.getAbsolutePath());
  }

  public Path toRealPath(boolean resolveLinks) throws IOException {
    return fs.getPath(file.getCanonicalPath());
  }

  public URI toUri() {
    return file.toURI();
  }

  public int compareTo(Path owner) {
    return toString().compareTo(owner.toString());
  }

  public Path copyTo(Path target, CopyOption... options) throws IOException {
    if (!target.isSameFile(this)) {
      boolean replace = false;

      for (CopyOption c : options) {
        if (c == StandardCopyOption.REPLACE_EXISTING) replace = true;
        else throw new UnsupportedOperationException(c.toString());
      }

      if (!replace && target.exists()) {
        throw new FileAlreadyExistsException(
          this.toString(), target.toString(), ""
        );
      }

      if (Boolean.TRUE.equals(getAttribute("isDirectory"))) {
        target.deleteIfExists();
        target.createDirectory();
      }
      else {
        InputStream in = null;
        OutputStream out = null;
        try {
          in = this.newInputStream();
          out = target.newOutputStream();
          IOUtils.copy(in, out);
          in.close();
          out.close();
        }
        finally {
          IOUtils.closeQuietly(in);
          IOUtils.closeQuietly(out);
        }
      }
    }

    return target;
  }

  public Path createLink(Path existing) throws IOException {
    throw new UnsupportedOperationException();
  }

  public Path createSymbolicLink(Path target, FileAttribute<?>... attrs)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }

  public Path readSymbolicLink() throws IOException {
    throw new UnsupportedOperationException();
  }

  public WatchKey register(WatchService watcher, WatchEvent.Kind<?>... events)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }

  public WatchKey register(WatchService watcher,
                           WatchEvent.Kind<?>[] events,
                           WatchEvent.Modifier... modifiers)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }
}
