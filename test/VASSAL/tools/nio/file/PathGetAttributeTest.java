package VASSAL.tools.nio.file;

import java.io.IOException;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathGetAttributeTest extends AbstractPathMethodTest {
  protected final String input;
  protected final String attr;

  public PathGetAttributeTest(FSHandler fac, String input,
                              String attr, Object expected) {
    super(fac, expected);

    this.input = input;
    this.attr = attr;
  }

  protected void doTest() throws IOException {
    final Object result = fs.getPath(input).getAttribute(attr);
    
    if (expected instanceof byte[]) {
      assertArrayEquals((byte[]) expected, (byte[]) result);
    }
    else {
      assertEquals(expected, result);
    }
  }
}
