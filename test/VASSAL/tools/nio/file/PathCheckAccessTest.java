package VASSAL.tools.nio.file;

import java.io.IOException;
import java.util.EnumSet;
import java.util.Set;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public abstract class PathCheckAccessTest extends AbstractPathMethodTest {
  protected final String input;
  protected final int mode;

  public PathCheckAccessTest(FSHandler fac, String input,
                             int mode, Object expected) {
    super(fac, expected);

    this.input = input;
    this.mode = mode;
  }

  protected void doTest() throws IOException {
    fs.getPath(input).checkAccess(o(mode));
  }

  // Input is a UNIX-style rwx mode in octal.
  private static AccessMode[] o(int mode) {
    final Set<AccessMode> s = EnumSet.noneOf(AccessMode.class);

    if ((mode & 01) > 0) s.add(AccessMode.EXECUTE);
    if ((mode & 02) > 0) s.add(AccessMode.WRITE);
    if ((mode & 04) > 0) s.add(AccessMode.READ);
        
    return s.toArray(new AccessMode[s.size()]);
  }
}
