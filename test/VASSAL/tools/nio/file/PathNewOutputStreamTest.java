package VASSAL.tools.nio.file;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import VASSAL.tools.io.IOUtils;
import VASSAL.tools.io.FileUtils;

import static org.junit.Assert.assertTrue;

import org.junit.Test;

public abstract class PathNewOutputStreamTest extends AbstractPathMethodTest {
  protected final String src_s;
  protected final String dst_s;
  protected final OpenOption[] opts;

  public PathNewOutputStreamTest(FSHandler fac, String src_s, String dst_s,
                                 OpenOption[] opts, Object expected) {
    super(fac, expected);

    this.src_s = src_s;
    this.dst_s = dst_s;
    this.opts = opts;
  }

  protected void doTest() throws IOException {
    final Path src = Paths.get(src_s);
    final Path dst = fs.getPath(dst_s);
    final Path exp = Paths.get((String) expected);

    OutputStream out = null;
    try {
      out = dst.newOutputStream(opts);

      InputStream in = null;
      try {
        in = src.newInputStream();
        IOUtils.copy(in, out);
        in.close();
      }
      finally {
        IOUtils.closeQuietly(in);
      }

      out.close();
    }
    finally {
      IOUtils.closeQuietly(out);
    }

    assertTrue(FileUtils.contentEquals(exp, dst));
  }
}
