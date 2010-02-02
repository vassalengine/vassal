package VASSAL.tools.nio.file;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathCompareToTest extends AbstractPathMethodTest {
  protected final String left;
  protected final String right;

  public PathCompareToTest(FSHandler fac, String left,
                           String right, Object expected) {
    super(fac, expected);

    this.left = left;
    this.right = right;
  }

  protected void doTest() {
    int result = fs.getPath(left).compareTo(fs.getPath(right));

    // clamp values to {-1,0,1}
    if (result > 1) result = 1;
    else if (result < -1) result = -1;

    assertEquals(expected, result);
  }
}
