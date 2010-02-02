package VASSAL.tools.nio.file;

public class AccessDeniedException extends FileSystemException {
  private static final long serialVersionUID = 1L;

  public AccessDeniedException(String file) {
    super(file);
  }
    
  public AccessDeniedException(String file, String other, String reason) {
    super(file, other, reason);
  }
}
