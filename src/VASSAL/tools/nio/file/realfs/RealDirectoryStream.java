package VASSAL.tools.nio.file.realfs;

import java.io.Closeable;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.NoSuchElementException;

import VASSAL.tools.nio.file.ClosedDirectoryStreamException;
import VASSAL.tools.nio.file.DirectoryStream;
import VASSAL.tools.nio.file.Path;

public class RealDirectoryStream implements DirectoryStream<Path> {
  private final RealPath parent;
  private final Filter<? super Path> filter;
  private boolean closed = false;

  public RealDirectoryStream(RealPath parent) {
    this(parent, null);
  }

  public RealDirectoryStream(RealPath parent, Filter<? super Path> filter) {
    this.parent = parent;
    this.filter = filter;
  }

  public void close() throws IOException {
    closed = true;
  }

  public Iterator<Path> iterator() {
    return new Iterator<Path>() {
      private final File[] ch = 
        parent.file.listFiles(filter == null ? null : new FilterAdapter());

      private int pos = 0;

      private Path cur;

      public boolean hasNext() {
        if (closed) throwIt(new ClosedDirectoryStreamException());
        return pos < ch.length;
      }

      public Path next() {
        if (closed) throwIt(new ClosedDirectoryStreamException());
        if (!hasNext()) throw new NoSuchElementException();

        cur = parent.fs.getPath(ch[pos++].toString());
        return cur;
      }

      public void remove() {
        if (closed) throwIt(new ClosedDirectoryStreamException());
        if (cur == null) throw new IllegalStateException();

        try {
          cur.delete();
        }
        catch (IOException e) {
          throwIt(e);
        }        

        cur = null; 
      }
    };
  }

  private class FilterAdapter implements FileFilter {
    public boolean accept(File file) {
      try {
        return filter.accept(parent.fs.getPath(file.toString()));
      }
      catch (IOException e) {
        throwIt(e);
        return false; // to make the compiler happy
      }
    }
  }

  private static void throwIt(Throwable cause) {
    throw (ConcurrentModificationException)
      new ConcurrentModificationException().initCause(cause);
  }
}
