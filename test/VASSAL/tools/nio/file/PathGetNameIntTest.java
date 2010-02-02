package VASSAL.tools.nio.file;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathGetNameIntTest extends AbstractPathMethodTest {
  protected final String input;
  protected final int index;

  public PathGetNameIntTest(FSHandler fac, String input,
                            int index, Object expected) {
    super(fac, expected);

    this.input = input;
    this.index = index;
  }

  protected void doTest() {
    assertEquals(expected, fs.getPath(input).getName(index).toString());
  }
}
