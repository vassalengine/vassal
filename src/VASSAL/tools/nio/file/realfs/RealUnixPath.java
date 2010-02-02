package VASSAL.tools.nio.file.realfs;

import VASSAL.tools.nio.file.InvalidPathException;

public class RealUnixPath extends RealPath {
  public RealUnixPath(String path, RealFileSystem fs) {
    super(path, fs);
    
    // paths containing nulls are evil
    final int i = path.indexOf("\u0000");
    if (i != -1) {
      throw new InvalidPathException(path, "null not permitted", i);
    }
  }

  /** {@inheritDoc} */
  protected int findRootSep(byte[] s) {
    return s[0] == '/' ? 0 : -1;
  }
}
