package VASSAL.tools.nio.channels;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileLock;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

import VASSAL.tools.nio.file.OpenOption;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.attribute.FileAttribute;

public abstract class AsynchronousFileChannel implements AsynchronousChannel {
  protected AsynchronousFileChannel() {}

  public static AsynchronousFileChannel open(
    Path file,
    Set<? extends OpenOption> optoins,
    ExecutorService executor,
    FileAttribute<?>... attrs) throws IOException
  {
    throw new UnsupportedOperationException();
  }

  public static AsynchronousFileChannel open(Path file, OpenOption... options)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }

  public abstract long size() throws IOException;

  public abstract AsynchronousFileChannel truncate(long size)
                                                            throws IOException;

  public abstract void force(boolean metaData) throws IOException;

  public abstract <A> void lock(
    long position,
    long size,
    boolean shared,
    A attachment,
    CompletionHandler<FileLock,? super A> handler);
 
  public final <A> void lock(A attachment,
                             CompletionHandler<FileLock,? super A> handler) {
    throw new UnsupportedOperationException();
  }

  public abstract Future<FileLock> lock(long position,
                                        long size, boolean shared);

  public final Future<FileLock> lock() {
    throw new UnsupportedOperationException();
  }

  public abstract FileLock tryLock(long position, long size, boolean shared)
                                                            throws IOException;

  public final FileLock tryLock() throws IOException {
    throw new UnsupportedOperationException();
  }

  public abstract <A> void read(
    ByteBuffer dst,
    long position,
    A attachment,
    CompletionHandler<Integer,? super A> handler);

  public abstract Future<Integer> read(ByteBuffer dst, long position);

  public abstract <A> void write(
    ByteBuffer src,
    long positoin,
    A attachment,
    CompletionHandler<Integer,? super A> handler);

  public abstract Future<Integer> write(ByteBuffer src, long position);
}
