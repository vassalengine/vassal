package VASSAL.tools.nio.file.attribute;

public interface FileAttribute<T> {
  public String name();

  public T value();
}
