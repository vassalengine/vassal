package VASSAL.tools.nio.file;

import java.io.IOException;

import VASSAL.tools.nio.file.attribute.FileAttributeView;
import VASSAL.tools.nio.file.attribute.FileStoreAttributeView;

public abstract class FileStore {
  public abstract Object getAttribute(String attribute) throws IOException;

  public abstract <V extends FileStoreAttributeView> V getFileStoreAttributeView(Class<V> type);

  public abstract boolean isReadOnly();

  public abstract String name();

  public abstract boolean supportsFileAttributeView(Class<? extends FileAttributeView> type);

  public abstract boolean supportsFileAttributeView(String name);

  public abstract String type();
}
