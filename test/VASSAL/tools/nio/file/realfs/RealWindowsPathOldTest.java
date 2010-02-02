package VASSAL.tools.nio.file.realfs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;

import org.junit.Before;
import org.junit.Test;

import VASSAL.Info;

public class RealWindowsPathOldTest extends RealPathTest {

  @Before
  public void setUp() throws Exception {
    super.setUp();
    assumeTrue(Info.isWindows());
  }
 
/* 
  @Test
  public void testFindRootSepWindowsDriveRoot() {
    assertEquals(2, pathTestingDirectory.findRootSep("D:\\TestDir\\TestDir2"));
  }

  @Test
  public void testFindRootSepWindowsUncRoot() {
    assertEquals(1, pathTestingDirectory.findRootSep("\\\\TestServer\\testDir"));
  }

  @Test
  public void testFindRootSepNonRoot() {
    assertEquals(-1, pathTestingDirectory.findRootSep("somethingelse"));
  }
*/

  @Test
  public void testRealPathStringRealFileSystem() {
    RealPath p1 = new RealWindowsPath(testFileCreated.getPath(), fs);
    assertEquals(p1.toString(), testFileCreated.toString());
  }

}
