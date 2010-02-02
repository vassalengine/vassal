package VASSAL.tools.nio.file;

import java.io.IOException;

public class FileSystemException extends IOException {
  private static final long serialVersionUID = 1L;

  private final String file;
  private final String other;
  private final String reason;

  public FileSystemException(String file) {
    this(file, null, null);
  }
  
  public FileSystemException(String file, String other, String reason) {
    super();
    this.file = file;
    this.other = other;
    this.reason = reason;
  }

  public String getFile() {
    return file;
  }
 
  public String getOtherFile() {
    return other;
  }

  public String getReason() {
    return reason;
  }

  @Override
  public String getMessage() {
    if (file == null && other == null) return getReason();

    final StringBuilder sb = new StringBuilder();
    if (file != null)        sb.append(file);
    if (other != null)       sb.append(" -> ").append(other);
    if (getReason() != null) sb.append(": ").append(getReason());
    return sb.toString();
  }
}
