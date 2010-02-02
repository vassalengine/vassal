package VASSAL.tools.nio.file;

public class ProviderNotFoundException extends RuntimeException {
  private static final long serialVersionUID = 1l;

  public ProviderNotFoundException() {
    super();
  }

  public ProviderNotFoundException(String msg) {
    super(msg);
  }
}
