package VASSAL.tools.nio.file;

import java.io.Closeable;
import java.io.IOException;

public interface DirectoryStream<T> extends Closeable, Iterable<T> {
  public static interface Filter<T> {
    public boolean accept(T entry) throws IOException;
  }
}
