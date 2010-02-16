package VASSAL.tools.nio.file.zipfs;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import VASSAL.tools.io.IOUtils;
import VASSAL.tools.nio.file.AbstractMethodTest;
import VASSAL.tools.nio.file.FileSystemAlreadyExistsException;
import VASSAL.tools.nio.file.FileSystems;
import VASSAL.tools.nio.file.NoSuchFileException;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.Paths;

import static VASSAL.tools.nio.file.AbstractMethodTest.t;
import static VASSAL.tools.nio.file.StandardOpenOption.*;

@RunWith(Suite.class)
@SuiteClasses({
  ZipFileSystemProviderTest.NewFileSystemTest.class
})
public class ZipFileSystemProviderTest {

  protected static final String testDir =
    "test/VASSAL/tools/nio/file/zipfs/readtest/".replace("/", File.separator);

  @RunWith(Parameterized.class)
  public static class NewFileSystemTest extends AbstractMethodTest {
    protected final String zf;
    protected final Map<String,?> env;   
 
    public NewFileSystemTest(String zf, Map<String,?> env, Object expected) {
      super(expected);
      this.zf = zf;
      this.env = env;
    }

    protected void doTest() throws IOException {
      final Path zfPath = Paths.get(testDir + zf).toAbsolutePath();
      final URI uri = URI.create("zip://" + zfPath.toString());
      
      ZipFileSystem fs = null;
      try {
        fs = (ZipFileSystem) FileSystems.newFileSystem(uri, env);
        fs.close();
      }
      finally {
        IOUtils.closeQuietly(fs);
      }
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        { "test.zip", null, null },
        { "test.zip", map("readonly", null), null },
        { "test.zip", map("bogus", "bogus"), t(IllegalArgumentException.class) },
//        { "foo", null, t(IOException.class) },
        { "notAFile", null, null },
        { "notAFile", map("readonly", null), t(NoSuchFileException.class) },
      });
    }

    protected static Map<String,?> map(Object... kv) {
      final Map<String,Object> m = new HashMap<String,Object>();

      for (int i = 0; i < kv.length; i += 2) {
        m.put((String) kv[i], kv[i+1]);
      }

      return m;
    }
  }
}
