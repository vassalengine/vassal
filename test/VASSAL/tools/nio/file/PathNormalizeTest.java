package VASSAL.tools.nio.file;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathNormalizeTest extends AbstractPathMethodTest {
  protected final String input;

  public PathNormalizeTest(FSHandler fac, String input, Object expected) {
    super(fac, expected);

    this.input = input;
  }

  protected void doTest() {
    final Path path = fs.getPath(input).normalize();
    final String result = path == null ? null : path.toString();
    assertEquals(expected, result);
  }
}
