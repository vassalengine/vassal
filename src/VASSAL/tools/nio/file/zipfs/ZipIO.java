package VASSAL.tools.nio.file.zipfs; 

import java.io.FilterInputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.util.EnumSet;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;

import VASSAL.tools.io.IOUtils;
import VASSAL.tools.nio.channels.FileChannelAdapter;
import VASSAL.tools.nio.channels.SeekableByteChannel;
import VASSAL.tools.nio.file.FileRef;
import VASSAL.tools.nio.file.OpenOption;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.Paths;
import VASSAL.tools.nio.file.StandardOpenOption;
import VASSAL.tools.nio.file.spi.FileSystemProvider;


class ZipIO {
  private ZipIO() {}

  static InputStream wrapReadLocked(ZipFilePath path, InputStream in) {
    return new LockedInputStream(path, in);
  }

  static OutputStream wrapWriteLocked(ZipFilePath path, OutputStream in) {
    return new LockedOutputStream(path, in);
  }

  static SeekableByteChannel wrapReadLocked(ZipFilePath path,
                                                      SeekableByteChannel ch) {
    return new ReadLockedChannel(path, ch);
  }

  static SeekableByteChannel wrapWriteLocked(ZipFilePath path,
                                                      SeekableByteChannel ch) {
    return new WriteLockedChannel(path, ch);
  }

  private static class LockedInputStream extends FilterInputStream {
    private final ZipFileSystem fs;
    private final ZipFilePath path;
    private boolean closed = false;

    public LockedInputStream(ZipFilePath path, InputStream in) {
      super(in);
      this.path = path;
      this.fs = path.getFileSystem();

      fs.readLock(path);
      fs.registerCloseable(this);
    }

    @Override
    public void close() throws IOException {
      if (closed) return;

      try {
        in.close();
        closed = true;
      }
      finally {
        fs.unregisterCloseable(this);
        fs.readUnlock(path);
      }
    }
  }

  private static class LockedOutputStream extends FilterOutputStream {
    private final ZipFileSystem fs;
    private final ZipFilePath path;
    private boolean closed = false;

    public LockedOutputStream(ZipFilePath path, OutputStream out) {
      super(out);
      this.path = path;
      this.fs = path.getFileSystem();
    
      fs.writeLock(path);
      fs.registerCloseable(this);
    }

    @Override
    public void close() throws IOException {
      if (closed) return;

      try {
        out.close();
        closed = true;
      }
      finally {
        fs.unregisterCloseable(this);
        fs.writeUnlock(path);
      }
    }
  }

  private static class RegisteredChannel implements SeekableByteChannel {
    protected final ZipFileSystem fs;
    protected final ZipFilePath path;
    protected final SeekableByteChannel ch;
    protected boolean closed = false;

    public RegisteredChannel(ZipFilePath path, SeekableByteChannel ch) {
      this.ch = ch;
      this.path = path;
      this.fs = path.getFileSystem();
      
      fs.registerCloseable(this);
    }

    public long position() throws IOException {
      return ch.position();
    }

    public SeekableByteChannel position(long newPosition) throws IOException {
      ch.position(newPosition);
      return this;
    }
    
    public int read(ByteBuffer dst) throws IOException {
      return ch.read(dst);
    }
    
    public long size() throws IOException {
      return ch.size();
    }

    public SeekableByteChannel truncate(long size) throws IOException {
      ch.truncate(size);
      return this;
    }

    public int write(ByteBuffer src) throws IOException {
      return ch.write(src);
    }

    public boolean isOpen() {
      return ch.isOpen();
    }

    public void close() throws IOException {
      try {
        ch.close();
        closed = true;
      }
      finally {
        fs.unregisterCloseable(this);
      }
    }
  }

  private static class ReadLockedChannel extends RegisteredChannel {
    public ReadLockedChannel(ZipFilePath path, SeekableByteChannel ch) {
      super(path, ch);
      fs.readLock(path);
    }

    @Override
    public void close() throws IOException {
      if (closed) return;    

      try {
        super.close();
      }
      finally {
        fs.readUnlock(path);
      }
    }
  }

  private static class WriteLockedChannel extends RegisteredChannel {
    public WriteLockedChannel(ZipFilePath path, SeekableByteChannel ch) {
      super(path, ch);
      fs.writeLock(path);
    }

    @Override
    public void close() throws IOException {
      if (closed) return;

      try {
        super.close();
      }
      finally {
        fs.writeUnlock(path);
      }
    }
  }

  static InputStream in(ZipFilePath path, OpenOption... opts)
                                                           throws IOException {
    final String zf = path.getZipFile();
    final ZipFile zfile = new ZipFile(zf);
    final String entryStr =
      path.getEntryName(path.getEntryNameCount() - 1).toString();

// FIXME: zfile not closed along some paths!!! 
    final ZipEntry entry = zfile.getEntry(entryStr);
    if (entry == null) {
      zfile.close();
      throw new IOException("entry not found: " + entryStr);
    }

    return wrapReadLocked(path, zfile.getInputStream(entry));
  }

  static SeekableByteChannel channel(ZipFilePath path,
                                     Set<? extends OpenOption> opts)
                                                           throws IOException {
    final Path pathToZip = extractFile(path);
    final FileSystemProvider prov = pathToZip.getFileSystem().provider();

    final FileChannelAdapter fca =
      new FileChannelAdapter(prov.newFileChannel(pathToZip, opts));

    return (opts.contains(StandardOpenOption.WRITE) ||
            opts.contains(StandardOpenOption.APPEND)) ?
      wrapWriteLocked(path, fca) : wrapReadLocked(path, fca);
  }

  private static Path extractFile(ZipFilePath path) throws IOException {
    final String zf = path.getZipFile();

    Path pathToZip = null;
    ZipFile zfile = null;
    try {
      zfile = new ZipFile(zf);
    
      final String entryStr =
        path.getEntryName(path.getEntryNameCount() - 1).toString();

      final ZipEntry entry = zfile.getEntry(entryStr);
      if (entry == null) {
        throw new IOException("entry not found: " + entryStr);
      }

      final InputStream in = zfile.getInputStream(entry);
      pathToZip = Paths.get(ZipUtils.readFileInZip(in));
      zfile.close();
    }
    finally {
      IOUtils.closeQuietly(zfile);
    }

    return pathToZip;
  }

  static SeekableByteChannel open(FileRef fr) throws IOException {
    final Set<StandardOpenOption> opts = EnumSet.of(StandardOpenOption.READ);
    final Path p = (Path) fr;

    return new FileChannelAdapter(
      p.getFileSystem().provider().newFileChannel(p, opts));
  }
}
