package VASSAL.tools.nio.file;

public interface FSHandler {
  public FileSystem setup() throws Throwable;
  public void teardown(FileSystem fs) throws Throwable;
}
