package VASSAL.tools.nio.file.realfs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;

import org.junit.Before;
import org.junit.Test;

import VASSAL.Info;

public class RealUnixPathOldTest extends RealPathTest {
  @Before
  public void setUp() throws Exception {
    super.setUp();
    assumeTrue(!Info.isWindows());
  }

/*
  @Test
  public void testFindRootSepUnixRoot() {
    assertEquals(0, pathTestingDirectory.findRootSep("/TestServer/testDir"));
  }
  
  public void testFindRootSepNonRoot() {
    assertEquals(-1, pathTestingDirectory.findRootSep("somethingelse"));
  }
*/

  @Test
  public void testRealPathStringRealFileSystem() {
    RealPath p1 = new RealUnixPath(testFileCreated.getPath(), fs);
    assertEquals(p1.toString(), testFileCreated.toString());
  }
}
