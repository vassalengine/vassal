package VASSAL.tools.nio.channels;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

/**
 * A wrapper to make {@link FileChannel} a {@link SeekableByteChannel}.
 *
 * @author Joel Uckelman
 * @since 3.2
 */ 
public class FileChannelAdapter implements SeekableByteChannel {
  protected final FileChannel fc;

  public FileChannelAdapter(FileChannel fc) {
    if (fc == null) throw new NullPointerException();
    this.fc = fc;
  }

  public void close() throws IOException {
    fc.close();
  }

  public boolean isOpen() {
    return fc.isOpen();
  }

  public long position() throws IOException {
    return fc.position();
  }

  public SeekableByteChannel position(long newPosition) throws IOException {
    fc.position(newPosition);
    return this;
  }

  public int read(ByteBuffer dst) throws IOException {
    return fc.read(dst);
  }

  public long size() throws IOException {
    return fc.size();
  }

  public SeekableByteChannel truncate(long size) throws IOException {
    fc.truncate(size);
    return this;
  }

  public int write(ByteBuffer src) throws IOException {
    return fc.write(src);
  }
}
