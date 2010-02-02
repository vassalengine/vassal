package VASSAL.tools.nio.file.zipfs;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import static org.junit.Assert.*;

import VASSAL.tools.nio.file.AccessDeniedException;
import VASSAL.tools.nio.file.AccessMode;
import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.FileSystems;
import VASSAL.tools.nio.file.FSHandler;
import VASSAL.tools.nio.file.NoSuchFileException;
import VASSAL.tools.nio.file.OpenOption;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.Paths;
import VASSAL.tools.nio.file.PathCheckAccessTest;
import VASSAL.tools.nio.file.PathCreateDirectoryTest;
import VASSAL.tools.nio.file.PathExistsTest;
import VASSAL.tools.nio.file.PathGetAttributeTest;
import VASSAL.tools.nio.file.PathIsSameFileTest;
import VASSAL.tools.nio.file.PathNewInputStreamTest;
import VASSAL.tools.nio.file.PathNotExistsTest;
import VASSAL.tools.nio.file.PathReadAttributesTest;
import VASSAL.tools.nio.file.PathToAbsolutePathTest;
import VASSAL.tools.nio.file.PathToRealPathTest;
//import VASSAL.tools.nio.file.PathToUriTest;
import VASSAL.tools.nio.file.attribute.FileTime;

import static VASSAL.tools.nio.file.StandardOpenOption.APPEND;
import static VASSAL.tools.nio.file.StandardOpenOption.READ;

import static VASSAL.tools.nio.file.AbstractPathMethodTest.t;

@RunWith(Suite.class)
@SuiteClasses({
  ZipFilePathReadTest.CheckAccessTest.class,
  ZipFilePathReadTest.ExistsTest.class,
  ZipFilePathReadTest.GetAttributeTest.class,
  ZipFilePathReadTest.IsSameFileTest.class,
  ZipFilePathReadTest.NewInputStreamTest.class,
  ZipFilePathReadTest.NotExistsTest.class,
  ZipFilePathReadTest.ReadAttributesTest.class,
  ZipFilePathReadTest.ToAbsolutePathTest.class,
  ZipFilePathReadTest.ToRealPathTest.class
//  ZipFilePathReadTest.ToUriTest.class
})
public class ZipFilePathReadTest {

  protected static ZipFileSystem fs;

  protected static final FSHandler fac = new FSHandler() {
    public FileSystem setup() { return fs; }
      
    public void teardown(FileSystem fs) {}
  };

  protected static final String testDir =
    "test/VASSAL/tools/nio/file/zipfs/".replace("/", File.separator);

  protected static final String zfName = "test.zip";
  protected static final String zfPathName = testDir + zfName;

  protected static Path zfPath;
  protected static URI zfURI;

  @BeforeClass
  public static void setupFS() throws IOException {
    zfPath = Paths.get(zfPathName).toAbsolutePath();
    zfURI = URI.create("zip://" + zfPath.toString());

    fs = (ZipFileSystem) FileSystems.newFileSystem(zfURI, null);
  }

  @RunWith(Parameterized.class)
  public static class CheckAccessTest extends PathCheckAccessTest {
    public CheckAccessTest(String input, int mode, Object expected) {
      super(ZipFilePathReadTest.fac, input, mode, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input          Mode  Expected
        { "/fileInZip",    00,  null                           },
        { "/fileInZip",    01,  t(AccessDeniedException.class) },
        { "/fileInZip",    02,  null                           },
        { "/fileInZip",    03,  t(AccessDeniedException.class) },
        { "/fileInZip",    04,  null                           },
        { "/fileInZip",    05,  t(AccessDeniedException.class) },
        { "/fileInZip",    06,  null                           },
        { "/fileInZip",    07,  t(AccessDeniedException.class) },
        { "/fileNotInZip", 00,  t(NoSuchFileException.class)   },
        { "fileInZip",     00,  null                           },
        { "fileInZip",     01,  t(AccessDeniedException.class) },
        { "fileInZip",     02,  null                           },
        { "fileInZip",     03,  t(AccessDeniedException.class) },
        { "fileInZip",     04,  null                           },
        { "fileInZip",     05,  t(AccessDeniedException.class) },
        { "fileInZip",     06,  null                           }, 
        { "fileInZip",     07,  t(AccessDeniedException.class) },
        { "fileNotInZip",  00,  t(NoSuchFileException.class)   }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ExistsTest extends PathExistsTest {
    public ExistsTest(String input, Object expected) {
      super(ZipFilePathReadTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input           Expected
        { "/fileInZip",    true  },
        { "/fileNotInZip", false },
        { "fileInZip",     true  },
        { "fileNotInZip",  false }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class NotExistsTest extends PathNotExistsTest {
    public NotExistsTest(String input, Object expected) {
      super(ZipFilePathReadTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input           Expected
        { "/fileInZip",    false },
        { "/fileNotInZip", true  },
        { "fileInZip",     false },
        { "fileNotInZip",  true  }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetAttributeTest extends PathGetAttributeTest {
    public GetAttributeTest(String path, String attr, Object expected) {
      super(ZipFilePathReadTest.fac, path, attr, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Path            Attribute     Expected
        { "/fileNotInZip", "basic:size", t(NoSuchFileException.class) },
        { "/fileInZip", null,            t(NullPointerException.class) },
        { "/fileInZip", "whatever",      null                          },
        { "/fileInZip", "lastModifiedTime", FileTime.fromMillis(1259797814000L) },
        { "/fileInZip", "lastAccessTime",   FileTime.fromMillis(-1L) },
        { "/fileInZip", "creationTime",     FileTime.fromMillis(-1L) },
        { "/fileInZip", "size",           0L },
        { "/fileInZip", "isRegularFile",  true  },
        { "/fileInZip", "isDirectory",    false },
        { "/fileInZip", "isSymbolicLink", false },
        { "/fileInZip", "isOther",        false },
        { "/fileInZip", "fileKey",        null  },
        { "/fileInZip", "basic:lastModifiedTime", FileTime.fromMillis(1259797814000L) },
        { "/fileInZip", "basic:lastAccessTime",   FileTime.fromMillis(-1L) },
        { "/fileInZip", "basic:creationTime",     FileTime.fromMillis(-1L) },
        { "/fileInZip", "basic:size",           0L },
        { "/fileInZip", "basic:isRegularFile",  true  },
        { "/fileInZip", "basic:isDirectory",    false },
        { "/fileInZip", "basic:isSymbolicLink", false },
        { "/fileInZip", "basic:isOther",        false },
        { "/fileInZip", "basic:fileKey",        null  },
        { "/fileInZip", "zip:comment",          null  },
        { "/fileInZip", "zip:crc",              0L    },
//        { "/fileInZip", "zip:extra",            null  },
        { "/fileInZip", "zip:method",           0     },
        { "/fileInZip", "zip:name",             "fileInZip".getBytes() },
        { "/fileInZip", "zip:isArchiveFile",    false },
        { "/fileInZip", "zip:versionMadeBy",    "UNIX" },
//        { "/fileInZip", "zip:extAttrs",         0 },
        { "/dirInZip", null,                    t(NullPointerException.class) },
        { "/dirInZip", "whatever",              null },
        { "/dirInZip", "basic:lastModifiedTime", FileTime.fromMillis(1259921068000L) },
        { "/dirInZip", "basic:lastAccessTime", FileTime.fromMillis(-1L) },
        { "/dirInZip", "basic:creationTime",   FileTime.fromMillis(-1L) },
        { "/dirInZip", "basic:size",           0L },
        { "/dirInZip", "basic:isRegularFile",  false  },
        { "/dirInZip", "basic:isDirectory",    true },
        { "/dirInZip", "basic:isSymbolicLink", false },
        { "/dirInZip", "basic:isOther",        false },
        { "/dirInZip", "basic:fileKey",        null  },
        { "/dirInZip", "zip:comment",          null  },
        { "/dirInZip", "zip:crc",              0L    },
//        { "/dirInZip", "zip:extra",            null  },
        { "/dirInZip", "zip:method",           0     },
        { "/dirInZip", "zip:name",             "dirInZip/".getBytes() },
        { "/dirInZip", "zip:isArchiveFile",    false },
        { "/dirInZip", "zip:versionMadeBy",    "UNIX" },
//        { "/dirInZip", "zip:extAttrs",         0 }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class IsSameFileTest extends PathIsSameFileTest {
    public IsSameFileTest(String left, String right, Object expected) {
      super(ZipFilePathReadTest.fac, left, right, expected);
    }

// FIXME: test case where providers differ
    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left            Right            Expected
        { "/fileInZip",    null,            false },
        { "/fileInZip",    "/fileInZip",    true  },
        { "/fileInZip",    "fileInZip",     true  },
        { "/fileInZip",    "/fileNotInZip", false },
        { "/fileInZip",    "fileNotInZip",  false },
        { "fileInZip",     "fileInZip",     true  },
        { "fileInZip",     "/fileInZip",    true  },
        { "fileInZip",     "/fileNotInZip", false },
        { "fileInZip",     "fileNotInZip",  false },
        { "/fileNotInZip", "/fileNotInZip", true  },
        { "/fileNotInZip", "fileNotInZip",  true  },
        { "fileNotInZip",  "/fileNotInZip", true  },
        { "fileNotInZip",  "fileNotInZip",  true  }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class NewInputStreamTest extends PathNewInputStreamTest {
    public NewInputStreamTest(String input, OpenOption[] opts,
                                                             Object expected) {
      super(ZipFilePathReadTest.fac, input, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input        Options                     Expected
        { "/fileInZip", new OpenOption[0],          testDir + "fileInZip" },
        { "/fileInZip", new OpenOption[]{ READ },   testDir + "fileInZip" },
        { "/fileInZip", new OpenOption[]{ APPEND }, t(UnsupportedOperationException.class) },
        { "/foo",       new OpenOption[0],          testDir + "foo"       },
        { "foo",        new OpenOption[0],          testDir + "foo"       },
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ReadAttributesTest extends PathReadAttributesTest {
    public ReadAttributesTest(String path, String attrs, Object expected) {
      super(ZipFilePathReadTest.fac, path, attrs, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Path         Attributes     Expected
        { "/fileInZip", "*", map(
            "lastModifiedTime", FileTime.fromMillis(1259797814000L),
            "lastAccessTime",   FileTime.fromMillis(-1L), 
            "creationTime",     FileTime.fromMillis(-1L),
            "size",             0L,
            "isRegularFile",    true,
            "isDirectory",      false,
            "isSymbolicLink",   false,
            "isOther",          false,
            "fileKey",          null)
        },
        { "/fileNotInZip", "*", t(NoSuchFileException.class) },
        { "/fileInZip", "foo:bar", map() }
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

  @RunWith(Parameterized.class)
  public static class ToAbsolutePathTest extends PathToAbsolutePathTest {
    public ToAbsolutePathTest(String input, Object expected) {
      super(ZipFilePathReadTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input        Expected
        { "/fileInZip", "/fileInZip" },
        { "fileInZip",  "/fileInZip" }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ToRealPathTest extends PathToRealPathTest {
    public ToRealPathTest(String input, boolean resLinks, Object expected) {
      super(ZipFilePathReadTest.fac, input, resLinks, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input                    Resolve?  Expected
        { "/dirInZip/../fileInZip", true,     "/fileInZip"         },
        { "/dirInZip/../fileInZip", false,    "/fileInZip"         },
        { "fileInZip",              true,     "/fileInZip"         },
        { "fileInZip",              false,    "/fileInZip"         },
        { "fileNotInZip",           true,     t(IOException.class) },
        { "fileNotInZip",           false,    t(IOException.class) }
      });
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateLink() throws IOException {
    fs.getPath("/someLink").createLink(fs.getPath("/fileInZip"));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testCreateSymbolicLink() throws IOException {
    fs.getPath("/someLink").createLink(fs.getPath("/fileInZip"));
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testReadSymbolicLink() throws IOException {
    fs.getPath("/someLink").readSymbolicLink();
  }

  @Test
  public void testGetFileSystem() {
    assertEquals(fs, fs.getPath("/fileInZip").getFileSystem());
  }

  @Test
  public void testIsHidden() {
    assertFalse(fs.getPath("/fileInZip").isHidden());
  }

// FIXME: use zfURI somehow in parameterized case... need a pointer.
  @Test
  public void testToUri() {
    final URI expected = zfURI.resolve("#/fileInZip");
    assertEquals(expected, fs.getPath("/fileInZip").toUri());
  }
}
