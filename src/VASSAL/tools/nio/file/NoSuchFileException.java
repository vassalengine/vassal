package VASSAL.tools.nio.file;

public class NoSuchFileException extends FileSystemException {
  private static final long serialVersionUID = 1L;

  public NoSuchFileException(String file) {
    super(file);
  }
    
  public NoSuchFileException(String file, String other, String reason) {
    super(file, other, reason);
  }
}
