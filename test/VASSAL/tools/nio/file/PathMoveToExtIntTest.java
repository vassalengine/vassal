package VASSAL.tools.nio.file;

import java.io.IOException;

public abstract class PathMoveToExtIntTest extends PathMoveToTest {
  public PathMoveToExtIntTest(FSHandler fac, String src, String dst,
                              CopyOption[] opts, Object expected) {
    super(fac, src, dst, opts, expected);
  }

  protected Path getSrc() throws IOException {
    return Paths.get(src);
  }

  protected Path getDst() throws IOException {
    return fs.getPath(dst);
  }
}
