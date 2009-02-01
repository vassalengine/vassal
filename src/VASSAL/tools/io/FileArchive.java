package VASSAL.tools.io;

import java.io.Closeable;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.List;

public interface FileArchive extends Closeable {
  public String getName();
  public File getFile();

  public InputStream read(String path) throws IOException;
  public OutputStream write(String path) throws IOException;

  public void add(String path, String extPath) throws IOException;
  public void add(String path, File extPath) throws IOException;
  public void add(String path, byte[] bytes) throws IOException;
  public void add(String path, InputStream in) throws IOException;

  public boolean remove(String path) throws IOException;

  public void revert() throws IOException;

  public void flush() throws IOException;
  public void close() throws IOException;

  public boolean contains(String path) throws IOException;

  public boolean isClosed();
  public boolean isModified();

  public long getSize(String path) throws IOException;

  public List<String> getFiles() throws IOException;
  public List<String> getFiles(String root) throws IOException;
}
