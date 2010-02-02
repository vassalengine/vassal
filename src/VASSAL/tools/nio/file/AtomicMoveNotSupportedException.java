package VASSAL.tools.nio.file;

public class AtomicMoveNotSupportedException extends FileSystemException {
  private static final long serialVersionUID = 1L;

  public AtomicMoveNotSupportedException(String source,
                                        String target, String reason) {
    super(source, target, reason);
  }
}
