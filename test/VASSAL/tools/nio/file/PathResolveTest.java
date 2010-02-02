package VASSAL.tools.nio.file;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathResolveTest extends AbstractPathMethodTest {
  protected final String left;
  protected final String right;

  public PathResolveTest(FSHandler fac, String left,
                         String right, Object expected) {
    super(fac, expected);

    this.left = left;
    this.right = right;
  }

  protected void doTest() {
    assertEquals(
      expected,
      fs.getPath(left).resolve(fs.getPath(right)).toString()
    );
  }
}
