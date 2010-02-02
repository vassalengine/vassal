package VASSAL.tools.nio.file;

public class FileAlreadyExistsException extends FileSystemException {
  private static final long serialVersionUID = 1L;

  public FileAlreadyExistsException(String file) {
    super(file);
  }
    
  public FileAlreadyExistsException(String file, String other, String reason) {
    super(file, other, reason);
  }
}
