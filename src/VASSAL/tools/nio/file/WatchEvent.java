package VASSAL.tools.nio.file;

public abstract class WatchEvent<T> {
  protected WatchEvent() {}

  public abstract T context();

  public abstract int count();

  public abstract WatchEvent.Kind<T> kind();

  public static interface Kind<T> {
    public String name();

    public Class<T> type();
  }

  public static interface Modifier {
    public String name();
  }
}
