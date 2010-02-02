package VASSAL.tools.nio.file;

import java.io.Closeable;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

public abstract class WatchService implements Closeable {
  protected WatchService() {}

  public abstract void close() throws IOException;

  public abstract WatchKey poll();

  public abstract WatchKey poll(long timeout, TimeUnit unit)
                                                   throws InterruptedException; 

  public abstract WatchKey take(); 
}
