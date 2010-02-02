package VASSAL.tools.nio.file;

public class NotDirectoryException extends FileSystemException {
  private static final long serialVersionUID = 1L;

  public NotDirectoryException(String file) {
    super(file);
  }
}
