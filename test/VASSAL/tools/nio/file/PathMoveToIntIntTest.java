package VASSAL.tools.nio.file;

import java.io.IOException;

public abstract class PathMoveToIntIntTest extends PathMoveToTest {
  public PathMoveToIntIntTest(FSHandler fac, String src, String dst,
                              CopyOption[] opts, Object expected) {
    super(fac, src, dst, opts, expected);
  }

  protected Path getSrc() throws IOException {
    return fs.getPath(src);
  }

  protected Path getDst() throws IOException {
    return fs.getPath(dst);
  }
}
