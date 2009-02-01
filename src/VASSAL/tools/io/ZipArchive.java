package VASSAL.tools.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FilterInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.AbstractQueuedSynchronizer;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.zip.CheckedOutputStream;
import java.util.zip.Checksum;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import VASSAL.tools.FileUtils;
import static VASSAL.tools.IterableEnumeration.iterate;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ZipArchive implements FileArchive {
  private final File archiveFile;
  private ZipFile zipFile;

  private boolean modified = false;
  private boolean closed = false;

  private static class Entry {
    public ZipEntry ze;
    public File file;

    public Entry(ZipEntry ze, File file) {
      this.ze = ze;
      this.file = file;
    }
  }

  private static class CountingReadWriteLock implements ReadWriteLock {
    public Lock readLock()  { return r; }
    public Lock writeLock() { return w; }
    
    private final ReadLock r  = new ReadLock();
    private final WriteLock w = new WriteLock();

    private final Sync sync = new Sync();

    private class ReadLock implements Lock {
      public void lock()   { sync.acquireShared(0); }
      public void unlock() { sync.releaseShared(0); }

      public void lockInterruptibly() {
        throw new UnsupportedOperationException();
      }

      public Condition newCondition() {
        throw new UnsupportedOperationException();
      }
  
      public boolean tryLock() {
        throw new UnsupportedOperationException();
      }

      public boolean tryLock(long time, TimeUnit unit) {
        throw new UnsupportedOperationException();
      }
    }

    private class WriteLock implements Lock {
      public void lock()   { sync.acquire(0); }
      public void unlock() { sync.release(0); }

      public void lockInterruptibly() {
        throw new UnsupportedOperationException();
      }

      public Condition newCondition() {
        throw new UnsupportedOperationException();
      }
  
      public boolean tryLock() {
        throw new UnsupportedOperationException();
      }

      public boolean tryLock(long time, TimeUnit unit) {
        throw new UnsupportedOperationException();
      }
    }

    // Read states are positive, the write state is -1.
    // State 0 means that no locks are held. 

    private static class Sync extends AbstractQueuedSynchronizer {
      private static final long serialVersionUID = 1L;

      @Override
      protected boolean tryAcquire(int dummy) {
        return compareAndSetState(0, -1);
      }

      @Override
      protected boolean tryRelease(int dummy) {
        if (getState() != -1) throw new IllegalMonitorStateException();
        return compareAndSetState(-1, 0);
      }

      @Override
      protected int tryAcquireShared(int dummy) {
        final int s = getState();
        return s >= 0 && compareAndSetState(s, s+1) ? 1 : -1;
      }

      @Override
      protected boolean tryReleaseShared(int dummy) {
        final int s = getState();
        if (s < 1) throw new IllegalMonitorStateException();
        return compareAndSetState(s, s-1);
      }
    }
  }

  private final Map<String,Entry> entries = new HashMap<String,Entry>();

  private final ReadWriteLock rwl = new CountingReadWriteLock();
  private final Lock r = rwl.readLock();
  private final Lock w = rwl.writeLock();

  /**
   * Opens a ZIP archive.
   *
   * @param path the name of the archive
   * @throws IOException
   */
  public ZipArchive(String path) throws IOException {
    this(path, false);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param file the name of the archive
   * @throws IOException
   */
  public ZipArchive(File file) throws IOException {
    this(file, false);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param path the name of the archive
   * @param truncate if <code>true</code>, truncate the archive file on open
   * @throws IOException
   */
  public ZipArchive(String path, boolean truncate) throws IOException {
    this(new File(path), truncate);
  }

  /**
   * Opens a ZIP archive.
   *
   * @param file the name of the archive
   * @param truncate if <code>true</code>, truncate the archive file on open
   * @throws IOException
   */
  public ZipArchive(File file, boolean truncate) throws IOException {
    if (file == null) throw new IllegalArgumentException();
    this.archiveFile = file;

    if (truncate) archiveFile.delete();
    else if (archiveFile.exists()) readEntries();
  }

  /**
   * Copies a ZIP archive.
   *
   * @param src the name of the source archive
   * @param dst the name of the destination archive
   * @throws IOException
   */
  public ZipArchive(FileArchive src, String dst) throws IOException {
    this(src, new File(dst));
  }

  /**
   * Copies a ZIP archive.
   *
   * @param src the name of the source archive
   * @param dst the name of the destination archive
   * @throws IOException
   */
  public ZipArchive(FileArchive src, File dst) throws IOException {
    this(dst, true);    

    final byte[] buf = new byte[8192];

    // copy each entry to the new archive
    for (String name : src.getFiles()) {
      InputStream in = null;
      try {
        in = src.read(name);
  
        OutputStream out = null;
        try {
          out = write(name);
          IOUtils.copy(in, out, buf);
          out.close();
        }
        finally {
          IOUtils.closeQuietly(out);
        }
      }
      finally {
        IOUtils.closeQuietly(in);
      }
    }

    flush();
  }

  /**
   * Returns the path to the archive.
   *
   * @return the path
   */
  public String getName() {
    return archiveFile.getPath();
  }

  /**
   * Returns the {@link File} for the archive.
   *
   * @return the file
   */
  public File getFile() {
    return archiveFile;
  }

  /** 
   * Queries whether the archive is closed.
   *
   * @return <code>true</code> if the archive is closed
   */
  public boolean isClosed() {
    return closed;
  } 

  /** 
   * Queries whether the archive is modified.
   * 
   * @return <code>true</code> if the archive is modified
   */
  public boolean isModified() {
    return modified;
  }

  /**
   * Returns an {@link InputStream} for reading a file from the archive.
   *
   * @param path the file to read
   * @return the <code>InputStream</code> 
   * @throws IOException
   */
  public InputStream read(String path) throws IOException {
    r.lock();
    try {
      openIfClosed();

      final Entry e = entries.get(path);
      if (e == null) throw new FileNotFoundException(path + " not in archive");
 
      InputStream in = null; 
      if (e.file != null) {
        in = new FileInputStream(e.file);
      }
      else if (zipFile != null) {
        in = zipFile.getInputStream(e.ze);
      }
      else {
        throw new FileNotFoundException(path + " not in archive");
      }

      return new ZipArchiveInputStream(in);
    }
    catch (IOException ex) {
      r.unlock();
      throw ex;
    }
  }

  /**
   * Returns an {@link OutputStream} for writing a file to the archive.
   * The file to be written will be compressed.
   *
   * @param path the file to write
   * @return the <code>OutputStream</code> 
   * @throws IOException
   */
  public OutputStream write(String path) throws IOException {
    return write(path, true);
  }

  /**
   * Returns an {@link OutputStream} for writing a file to the archive.
   *
   * @param path the file to write
   * @param compress whether to compress the file 
   * @return the <code>OutputStream</code> 
   * @throws IOException
   */
  public OutputStream write(String path, boolean compress) throws IOException {
    w.lock();
    try {
      openIfClosed();

      modified = true;

      // update the entries map
      Entry e = entries.get(path);
      if (e == null) {
        e = new Entry(null, null);
        entries.put(path, e);
      }

      // set up new ZipEntry
      final ZipEntry ze = new ZipEntry(path);
      ze.setMethod(compress ? ZipEntry.DEFLATED : ZipEntry.STORED);
      e.ze = ze;
    
      // clean up old temp file
      if (e.file != null) e.file.delete();

      // create new temp file
      e.file = File.createTempFile("zip", ".tmp");

      return new ZipArchiveOutputStream(
        new FileOutputStream(e.file), new CRC32(), e.ze);
    }
    catch (IOException ex) {
      w.unlock();
      throw ex;
    }
  }

  public void add(String path, String extPath) throws IOException {
    add(path, new File(extPath));
  }

  public void add(String path, File extPath) throws IOException {
    FileInputStream in = null;
    try {
      in = new FileInputStream(extPath);
      add(path, in);
      in.close();
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  public void add(String path, byte[] bytes) throws IOException {
    add(path, new ByteArrayInputStream(bytes));
  }

  public void add(String path, InputStream in) throws IOException {
    OutputStream out = null;
    try {
      out = write(path);
      IOUtils.copy(in, out);
      out.close();
    }
    finally {
      IOUtils.closeQuietly(out);
    }
  }

  /**
   * Removes a file from the archive.
   *
   * @param path the file to remove
   * @return <code>true</code> if the file existed
   * @throws IOException
   */
  public boolean remove(String path) throws IOException {
    w.lock();
    try {
      openIfClosed();

      final Entry e = entries.remove(path);
      if (e != null) modified = true;

      if (e.file != null) e.file.delete();
      return e != null;
    }
    finally {
      w.unlock();
    }
  }

  /**
   * Reverts changes to the archive which have not yet been flushed.
   *
   * @throws IOException
   */
  public void revert() throws IOException {
    w.lock();
    try {
      if (!modified) return;

      // delete all temporary files
      for (String name : entries.keySet()) {
        final Entry e = entries.get(name);
        if (e.file != null) e.file.delete();
      }

      readEntries();
      modified = false; 
    }
    finally {
      w.unlock();
    }
  }

  /**
   * Flushes all pending writes to disk.
   *
   * @throws IOException
   */
  public void flush() throws IOException {
    w.lock();
    try {
      writeToDisk();
    }
    finally {
      w.unlock();
    }
  }
  
  private void writeToDisk() throws IOException {
    if (!modified) return;

    // write all files to a temporary zip archive
    final File tmpFile =
      File.createTempFile("tmp", ".zip", archiveFile.getParentFile());
    ZipOutputStream out = null;
    try {
      out = new ZipOutputStream(
              new BufferedOutputStream(
                new FileOutputStream(tmpFile)));
      out.setLevel(9);

      final byte[] buf = new byte[8192];

      if (zipFile != null) {
        zipFile.close();
  
        // copy unmodified file into the temp archive
        ZipInputStream in = null;
        try {
          in = new ZipInputStream(
                 new BufferedInputStream(
                   new FileInputStream(archiveFile)));

          ZipEntry ze = null;
          while ((ze = in.getNextEntry()) != null) {
            // skip modified or removed entries
            final Entry e = entries.get(ze.getName());
            if (e == null || e.file != null) continue;

            // We can't reuse entries for compressed files because there's
            // no way to reset all fields to acceptable values.
            if (ze.getMethod() == ZipEntry.DEFLATED) {
              ze = new ZipEntry(ze.getName());
            }

            out.putNextEntry(ze);
            IOUtils.copy(in, out, buf);

            entries.remove(ze.getName()); 
          }

          in.close();
        }
        finally {
          IOUtils.closeQuietly(in);
        }
      }

      for (String name : entries.keySet()) {
        final Entry e = entries.get(name);

        // write new or modified file into the temp archive
        FileInputStream in = null;
        try {
          in = new FileInputStream(e.file);
          out.putNextEntry(e.ze);
          IOUtils.copy(in, out, buf);
          in.close();
        }
        finally {
          IOUtils.closeQuietly(in);
        }
      }

      out.close();
    }
    finally {
      IOUtils.closeQuietly(out);
    }

    FileUtils.move(tmpFile, archiveFile);

/*
      // Replace old archive by temp archive. Note that we must try to delete
      // the old archive first because on some platforms (Windows, blech!)
      // File.renameTo() fails if the destination file already exists.
      if ((archiveFile.exists() && !archiveFile.delete()) ||
           !tmpFile.renameTo(archiveFile)) {
        throw new IOException(
          "Unable to overwrite " + archiveFile.getAbsolutePath() +
          ", data written to " + tmpFile.getAbsolutePath() + " instead.");
      }
*/

    readEntries();
    modified = false;
  }

  /**
   * Closes the archive. Attempts to manipulate the archive after calling
   * close will result in an {@link IOException}.
   *
   * @throws IOException
   */
  public void close() throws IOException {
    w.lock();
    try {
      if (!closed) {
        writeToDisk();

        closed = true;
        modified = false;
        zipFile = null;
        entries.clear();
      }
    }
    finally {
      w.unlock();
    }
  }

  /**
   * Queries whether a file in the archive.
   *
   * @param path the file's path
   * @return <code>true</code> if the file is in the archive
   * @throws IOException
   */
  public boolean contains(String path) throws IOException {
    r.lock();
    try {
      openIfClosed();
      return entries.containsKey(path);
    }
    finally {
      r.unlock();
    }
  }

  /**
   * Returns the size of a file in the archive.
   *
   * @param path the file's path
   * @return the file's size in bytes
   * @throws FileNotFoundException if <code>path</code> is not in the archive
   * @throws IOException
   */
  public long getSize(String path) throws IOException {
    r.lock();
    try {
      openIfClosed();

      final Entry e = entries.get(path);
      if (e == null) throw new FileNotFoundException(path + " not in archive");
      return e.file == null ? e.ze.getSize() : e.file.length();
    }
    finally {
      r.unlock();
    }
  } 

  /**
   * Returns the list of files in the archive.
   *
   * @return the list of files in the archive
   * @throws IOException
   */
  public List<String> getFiles() throws IOException {
    r.lock();
    try {
      openIfClosed();

      final ArrayList<String> names = new ArrayList<String>();
      for (String n : entries.keySet()) {
        names.add(n);
      }
      return names;
    }
    finally {
      r.unlock();
    }
  }

  /**
   * Returns a list of files recursively from the given root directory in
   * the archive.
   *
   * @param root a directory in the archive
   * @return a list of files under the given directory
   * @throws FileNotFoundException if <code>root</code> is not in the archive
   * @throws IOException
   */ 
  public List<String> getFiles(String root) throws IOException {
    if (root.length() == 0) return getFiles();

    r.lock();
    try {
      openIfClosed();

      if (!entries.containsKey(root))
        throw new FileNotFoundException(root + "not in archive");

      root += File.separator;
      final ArrayList<String> names = new ArrayList<String>();

      for (String n : entries.keySet()) {
        if (n.startsWith(root)) {
          names.add(n);
        }
      }

      return names;
    }
    finally {
      r.unlock();
    }
  }

  /** Rebuilds the {@link ZipEntries} from our underlying {@link ZipFile}. */
  private synchronized void readEntries() throws IOException {
    entries.clear();
    zipFile = new ZipFile(archiveFile);

    for (ZipEntry e : iterate(zipFile.entries())) {
      entries.put(e.getName(), new Entry(e, null));
    }
  }

  /** Opens the archive if it is closed. */
  private synchronized void openIfClosed() throws IOException {
    if (closed) {
      readEntries();
      modified = false;
      closed = false;
    }
  }

  /** An {@link InputStream} which releases the read lock on close. */
  private class ZipArchiveInputStream extends FilterInputStream {
    public ZipArchiveInputStream(InputStream in) {
      super(in);
    }

    private boolean closed = false;

    @Override
    public void close() throws IOException {
      if (closed) return;

      try {
        super.close();
      }
      finally {
        r.unlock();
        closed = true;
      }
    }
  }

  /**
   * An {@link OutputStream} which calculates a checksum, counts bytes
   * written, and releases the write lock on close.
   */
  private class ZipArchiveOutputStream extends CheckedOutputStream {
    private ZipEntry entry;
    private long count = 0;

    public ZipArchiveOutputStream(OutputStream out,
                                  Checksum cksum, ZipEntry e) {
      super(out, cksum);
      entry = e;
    }

    @Override
    public void write(byte[] bytes, int off, int len) throws IOException {
      super.write(bytes, off, len);
      count += len;      
    }

    @Override
    public void write(int b) throws IOException {
      super.write(b);
      ++count;
    }

    @Override
    public void flush() throws IOException {
      super.flush();
      entry.setSize(count);
      entry.setCrc(getChecksum().getValue());
    }

    private boolean closed = false;

    @Override
    public void close() throws IOException {
      if (closed) return;
    
      try {
        super.close();
      }
      finally {
        w.unlock();
        closed = true;
      }
    }
  }

  public static void main(String[] args) throws IOException {
    
    final ZipArchive archive = new ZipArchive("test.zip");

    // write test
    archive.add("NOTES", "NOTES");
    archive.add("README.txt", "README.txt");

    archive.flush();

    // read test
    
    InputStream in = null; 
    try {
      in = archive.read("NOTES");
      IOUtils.copy(in, System.out); 
      in.close();
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    archive.close();
  }
}
