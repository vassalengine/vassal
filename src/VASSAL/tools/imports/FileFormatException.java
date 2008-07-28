package VASSAL.tools.imports;

import java.io.IOException;

/**
 * File cannot be interpreted.  Either the file is not what VASSAL thinks it is
 * or it is currupted in some way.
 */
public class FileFormatException extends IOException {

  private static final long serialVersionUID = 0L;

  FileFormatException() {
  }

  public FileFormatException(String s) {
    super(s);
  }
}