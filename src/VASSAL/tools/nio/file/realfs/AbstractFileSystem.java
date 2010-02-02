package VASSAL.tools.nio.file.realfs;

import java.io.IOException;

import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.PathMatcher;
import VASSAL.tools.nio.file.WatchService;
import VASSAL.tools.nio.file.attribute.UserPrincipalLookupService;

abstract class AbstractFileSystem extends FileSystem {

  public PathMatcher getPathMatcher(String syntaxAndPatttern) {
    throw new UnsupportedOperationException();
  }

  public UserPrincipalLookupService getUserPrincipalLookupService() {
    throw new UnsupportedOperationException();
  }

  public WatchService newWatchService() throws IOException {
    throw new UnsupportedOperationException();
  }
}
