package VASSAL.tools.nio.file.zipfs;

import java.io.IOException;
import java.util.ConcurrentModificationException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import VASSAL.tools.nio.file.ClosedDirectoryStreamException;
import VASSAL.tools.nio.file.DirectoryStream;
import VASSAL.tools.nio.file.DirectoryStream.Filter;
import VASSAL.tools.nio.file.NoSuchFileException;
import VASSAL.tools.nio.file.NotDirectoryException;
import VASSAL.tools.nio.file.Path;

public class ZipFileStream implements DirectoryStream<Path> {

  protected final ZipFilePath dir; 
  protected final Filter<? super Path> filter;  

  protected Iterator<Path> iterator;
  protected volatile boolean closed = false;

  public ZipFileStream(ZipFilePath dir, Filter<? super Path> filter)
                                                           throws IOException {
    if (!dir.exists()) {
      throw new NoSuchFileException(dir.toString());
    }

    if (!Boolean.TRUE.equals(dir.getAttribute("isDirectory"))) {
      throw new NotDirectoryException(dir.toString());
    }

    this.dir = dir;
    this.filter = filter;
  }

  public void close() throws IOException {
    closed = true;
  }
 
  protected ConcurrentModificationException wrap(Throwable cause) {
    return (ConcurrentModificationException)
      new ConcurrentModificationException().initCause(cause);
  }

  public Iterator<Path> iterator() {
    if (closed) throw new IllegalStateException("stream is closed");

    synchronized (this) {
      if (iterator != null)
        throw new IllegalStateException("iterator has already been fecthed");

      try {
        iterator = new ZipFileStreamIterator();
      }
      catch (IOException e) {
        throw new IllegalStateException(e);
      }

      return iterator;
    }
  }

  protected class ZipFileStreamIterator implements Iterator<Path> {

    protected Iterator<Path> it;

    protected Path next;
    protected Path prev;

    public ZipFileStreamIterator() throws IOException {

      final ZipFileSystem fs = dir.getFileSystem(); 
      final int nameCount = dir.getNameCount();

      final Set<Path> s = new HashSet<Path>();

      // find all original children of this directory
      for (ZipFilePath f : fs.getAllInfo().keySet()) {
        // skip files which are not in this directory
        if (nameCount + 1 > f.getNameCount() || !f.startsWith(dir)) {
          continue;
        }

        s.add(dir.resolve(f.getName(nameCount)));
      }

      // find all modified children of this directory
      for (Map.Entry<ZipFilePath,Path> e : fs.real.entrySet()) {
        final ZipFilePath f = e.getKey();

        if (nameCount + 1 > f.getNameCount() || !f.startsWith(dir)) {
          // skip files which are not in this directory
          continue;
        }
       
        if (e.getValue() == null) {
          // remove files which have been deleted
          s.remove(f);
        }
        else {
          // add files which are new
          s.add(f);
        }
      }

      it = s.iterator();
    }

    protected boolean accept(Path p) {
      try {
        return filter == null || filter.accept(p);
      }
      catch (IOException e) {
        throw wrap(e);
      }
    }

    public synchronized boolean hasNext() {
      if (closed) throw wrap(new ClosedDirectoryStreamException());

      if (next != null) return true;

      while (it.hasNext()) {
        next = it.next();
        if (accept(next)) return true;
      }

      next = null; 
      return false;
    }

    public synchronized Path next() {
      if (closed) throw wrap(new ClosedDirectoryStreamException());
      if (next == null) throw new NoSuchElementException();

      prev = next;
      next = null;
      return prev;
    }

    public synchronized void remove() {
      if (closed) throw wrap(new ClosedDirectoryStreamException());
      if (prev == null) throw new IllegalStateException();
      
      try {
        prev.delete();
      }
      catch (IOException e) {
        throw wrap(e);
      }

      prev = null;
    }
  }
} 
