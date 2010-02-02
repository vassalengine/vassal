package VASSAL.tools.nio.file;

import java.io.IOException;

import org.junit.Test;

public abstract class PathCopyToIntExtTest extends PathCopyToTest {
  public PathCopyToIntExtTest(FSHandler fac, String src, String dst,
                              CopyOption[] opts, Object expected) {
    super(fac, src, dst, opts, expected);
  }

  protected Path getSrc() throws IOException {
    return fs.getPath(src);
  }

  protected Path getDst() throws IOException {
    return Paths.get(dst);
  }
}
