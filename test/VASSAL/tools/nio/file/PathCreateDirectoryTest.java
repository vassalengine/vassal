package VASSAL.tools.nio.file;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathCreateDirectoryTest extends AbstractPathMethodTest {
  protected final String input;

  public PathCreateDirectoryTest(FSHandler fac, String input, Object expected) {
    super(fac, expected);

    this.input = input;
  }

  protected void doTest() throws IOException {
    final Path path = fs.getPath(input);
    path.createDirectory(); 
    assertEquals(true, path.getAttribute("isDirectory"));
  }
}
