package VASSAL.tools.nio.file;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathCreateFileTest extends AbstractPathMethodTest {
  protected final String input;

  public PathCreateFileTest(FSHandler fac, String input, Object expected) {
    super(fac, expected);

    this.input = input;
  }

  protected void doTest() throws IOException {
    final Path path = fs.getPath(input);
    path.createFile();
    assertEquals(true, path.getAttribute("isRegularFile"));
  }
}
