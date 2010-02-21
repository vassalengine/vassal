package VASSAL.tools;

import java.io.File;
import java.net.URI;

public class URIUtils {
  protected URIUtils() {}

  public static URI toURI(String scheme, File file) {
    // this handles getting the slashes the right way
    final URI furi = file.getAbsoluteFile().toURI();
    return URI.create(furi.toString().replaceFirst("^file", scheme));
  }
}
