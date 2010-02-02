package VASSAL.tools.nio.file;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathSetAttributeTest extends AbstractPathMethodTest {
  protected final String input;
  protected final String attr;
  protected final Object value;

  public PathSetAttributeTest(FSHandler fac, String input, String attr,
                              Object value, Object expected) {
    super(fac, expected);

    this.input = input;
    this.attr = attr;
    this.value = value;
  }

  protected void doTest() throws IOException {
    final Path path = fs.getPath(input);
    path.setAttribute(attr, value);
    assertEquals(path.getAttribute(attr), value);
  }
}
