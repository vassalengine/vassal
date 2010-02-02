package VASSAL.tools.nio.file;

import java.io.IOException;

import VASSAL.tools.io.FileUtils;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public abstract class PathMoveToTest extends AbstractPathMethodTest {
  protected final String src;
  protected final String dst;
  protected final CopyOption[] opts;

  public PathMoveToTest(FSHandler fac, String src, String dst,
                        CopyOption[] opts, Object expected) {
    super(fac, expected);

    this.src = src;
    this.dst = dst;
    this.opts = opts == null ? new CopyOption[0] : opts;
  }

  protected abstract Path getSrc() throws IOException;
  protected abstract Path getDst() throws IOException;

  protected void doTest() throws IOException {
    final Path sp = getSrc();
    final Path dp = getDst();

    try {
      if (Boolean.TRUE.equals(sp.getAttribute("isDirectory"))) {

        final Path ret = sp.moveTo(dp, opts);

        assertTrue(!sp.exists());
        assertTrue(dp.exists());
        assertEquals(dp, ret); 
        assertTrue(Boolean.TRUE.equals(dp.getAttribute("isDirectory")));
      }
      else {
        final byte[] expectedBytes = FileUtils.readFileToByteArray(sp);
      
        final Path ret = sp.moveTo(dp, opts);

        assertTrue(!sp.exists());
        assertTrue(dp.exists());
        assertEquals(dp, ret); 
  
        final byte[] actualBytes = FileUtils.readFileToByteArray(dp);

        assertArrayEquals(expectedBytes, actualBytes);
      }
    }
    finally {
      dp.deleteIfExists();
    }
  }
}
