package VASSAL.tools.nio.file;

public class FileSystemNotFoundException extends RuntimeException {
  private static final long serialVersionUID = 1l;

  public FileSystemNotFoundException() {
    super();
  }

  public FileSystemNotFoundException(String msg) {
    super(msg);
  }
}
