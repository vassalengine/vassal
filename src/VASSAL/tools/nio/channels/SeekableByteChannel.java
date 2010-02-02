package VASSAL.tools.nio.channels;

import java.io.IOException;
import java.nio.channels.ByteChannel;

public interface SeekableByteChannel extends ByteChannel {
  public long position() throws IOException;

  public SeekableByteChannel position(long newPosition) throws IOException;

  public long size() throws IOException;

  public SeekableByteChannel truncate(long size) throws IOException;
}
