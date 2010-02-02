package VASSAL.tools.nio.file;

public class ProviderMismatchException extends IllegalArgumentException {
  private static final long serialVersionUID = 1L;

  public ProviderMismatchException() {}

  public ProviderMismatchException(String msg) {
    super(msg);
  }
}
