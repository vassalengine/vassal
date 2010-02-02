package VASSAL.tools.nio.file;

import java.net.URI;

import VASSAL.tools.nio.file.spi.FileSystemProvider;

public final class Paths {
  public static Path get(String path) {
    return FileSystems.getDefault().getPath(path);
  }

  public static Path get(URI uri) {
    final String scheme = uri.getScheme();

    for (FileSystemProvider p : FileSystemProvider.installedProviders()) {
      if (p.getScheme().equalsIgnoreCase(scheme)) {
        return p.getPath(uri);
      }
    }

    throw new FileSystemNotFoundException("no provider for " + scheme);
  }
}
