package VASSAL.tools.nio.file;

public class InvalidPathException extends IllegalArgumentException {
  private static final long serialVersionUID = 1L;

  protected final int index;
  protected final String input;
  protected final String msg;

  public InvalidPathException(String input, String reason) {
    this(input, reason, -1);
  }

  public InvalidPathException(String input, String reason, int index) {
    super(reason);
    this.index = index;
    this.input = input;

    final StringBuilder sb = new StringBuilder(getReason());
    if (index != -1) sb.append(" at ").append(index);
    sb.append(": ").append(input);

    msg = sb.toString();
  }

  public int getIndex() {
    return index;
  }

  public String getInput() {
    return input;
  }

  @Override
  public String getMessage() {
    return msg;
  }

  public String getReason() {
    return super.getMessage();
  }
}
