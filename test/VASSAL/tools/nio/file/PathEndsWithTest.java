package VASSAL.tools.nio.file;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathEndsWithTest extends AbstractPathMethodTest {
  protected final String left;
  protected final String right;

  public PathEndsWithTest(FSHandler fac, String left,
                          String right, Object expected) {
    super(fac, expected);

    this.left = left;
    this.right = right;
  }

  protected void doTest() {
    assertEquals(expected, fs.getPath(left).endsWith(fs.getPath(right)));
  }
}
