package VASSAL.tools.nio.file;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathIsSameFileTest extends AbstractPathMethodTest {
  protected final String left;
  protected final String right;

  public PathIsSameFileTest(FSHandler fac, String left,
                            String right, Object expected) {
    super(fac, expected);

    this.left = left;
    this.right = right;
  }

  protected void doTest() throws IOException {
    final Path other = right == null ? null : fs.getPath(right);
    assertEquals(expected, fs.getPath(left).isSameFile(other));
  }
}
