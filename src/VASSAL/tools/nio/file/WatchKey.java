package VASSAL.tools.nio.file;

import java.util.List;

public abstract class WatchKey {
  protected WatchKey() {}

  public abstract void cancel();

  public abstract boolean isValid();

  public abstract List<WatchEvent<?>> pollEvents();

  public abstract boolean reset();
}
