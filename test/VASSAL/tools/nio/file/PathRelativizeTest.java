package VASSAL.tools.nio.file;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathRelativizeTest extends AbstractPathMethodTest {
  protected final String left;
  protected final String right;

  public PathRelativizeTest(FSHandler fac, String left,
                            String right, Object expected) {
    super(fac, expected);

    this.left = left;
    this.right = right;
  }

  protected void doTest() {
    final Path path = fs.getPath(left).relativize(fs.getPath(right));
    final String result = path == null ? null : path.toString();
    assertEquals(expected, result);
  }
}
