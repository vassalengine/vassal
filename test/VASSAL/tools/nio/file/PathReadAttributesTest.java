package VASSAL.tools.nio.file;

import java.io.IOException;
import java.util.Map;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathReadAttributesTest extends AbstractPathMethodTest {
  protected final String input;
  protected final String attrs;

  public PathReadAttributesTest(FSHandler fac, String input,
                                String attrs, Object expected) {
    super(fac, expected);

    this.input = input;
    this.attrs = attrs;
  }

  protected void doTest() throws IOException {
    final Map<String,?> result = fs.getPath(input).readAttributes(attrs);
    assertEquals(expected, result); 
  }
}
