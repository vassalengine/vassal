package VASSAL.tools.lang;

import java.io.IOException;

@FunctionalInterface
public interface Callback<T> {
  public void receive(T obj) throws IOException;
}
