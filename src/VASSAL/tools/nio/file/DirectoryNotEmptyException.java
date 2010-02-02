package VASSAL.tools.nio.file;

public class DirectoryNotEmptyException extends FileSystemException {
  private static final long serialVersionUID = 1L;

  public DirectoryNotEmptyException(String file) {
    super(file);
  }
}
