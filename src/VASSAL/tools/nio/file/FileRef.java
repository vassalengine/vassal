package VASSAL.tools.nio.file;

import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Map;

import VASSAL.tools.nio.file.attribute.FileAttributeView;

public interface FileRef {
  public Object getAttribute(String attribute, LinkOption... options)
                                                            throws IOException;

  public <V extends FileAttributeView> V getFileAttributeView(
    Class<V> type, LinkOption... options);

  public InputStream newInputStream(OpenOption... options) throws IOException;

  public OutputStream newOutputStream(OpenOption... options) throws IOException;

  public Map<String,?> readAttributes(String attributes, LinkOption... options)
                                                            throws IOException;
  
  public void setAttribute(String attribute,
                           Object value, LinkOption... options)
                                                            throws IOException;
}
