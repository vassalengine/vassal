package VASSAL.tools.nio.file;

public class FileSystemAlreadyExistsException extends RuntimeException {
  private static final long serialVersionUID = 1L;

  public FileSystemAlreadyExistsException() {
  }

  public FileSystemAlreadyExistsException(String msg) {
    super(msg);
  }
}
