package VASSAL.tools.lang;

import java.io.IOException;

@FunctionalInterface
public interface Callback<T> {
  void receive(T obj) throws IOException;
}
