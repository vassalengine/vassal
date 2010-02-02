package VASSAL.tools.nio.file;

import java.io.IOException;
import java.io.InputStream;

import VASSAL.tools.io.IOUtils;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public abstract class PathNewInputStreamTest extends AbstractPathMethodTest {
  protected final String input;
  protected final OpenOption[] opts;

  public PathNewInputStreamTest(FSHandler fac, String input,
                                OpenOption[] opts, Object expected) {
    super(fac, expected);

    this.input = input;
    this.opts = opts;
  }

  protected void doTest() throws IOException {
    InputStream actual_in = null;
    try {
      actual_in = fs.getPath(input).newInputStream(opts);
      
      InputStream expected_in = null;
      try {
        expected_in = Paths.get((String) expected).newInputStream();
        assertTrue(IOUtils.contentEquals(actual_in, expected_in));
        expected_in.close();
      }
      finally {
        IOUtils.closeQuietly(expected_in);
      }

      actual_in.close();
    }
    finally {
      IOUtils.closeQuietly(actual_in);
    }
  }
}
