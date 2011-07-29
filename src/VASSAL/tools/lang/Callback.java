package VASSAL.tools.lang;

import java.io.IOException;

public interface Callback<T> {
  public void receive(T obj) throws IOException;
}
