package VASSAL.tools.nio.file.zipfs;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.List;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.FileSystems;
import VASSAL.tools.nio.file.FSHandler;
import VASSAL.tools.nio.file.InvalidPathException;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.Paths;
import VASSAL.tools.nio.file.PathCompareToTest;
import VASSAL.tools.nio.file.PathConstructorTest;
import VASSAL.tools.nio.file.PathEndsWithTest;
import VASSAL.tools.nio.file.PathEqualsTest;
import VASSAL.tools.nio.file.PathGetNameTest;
import VASSAL.tools.nio.file.PathGetNameCountTest;
import VASSAL.tools.nio.file.PathGetNameIntTest;
import VASSAL.tools.nio.file.PathGetParentTest;
import VASSAL.tools.nio.file.PathGetRootTest;
import VASSAL.tools.nio.file.PathHashCodeTest;
import VASSAL.tools.nio.file.PathIsAbsoluteTest;
import VASSAL.tools.nio.file.PathIteratorTest;
import VASSAL.tools.nio.file.PathNormalizeTest;
import VASSAL.tools.nio.file.PathRelativizeTest;
import VASSAL.tools.nio.file.PathResolveTest;
import VASSAL.tools.nio.file.PathStartsWithTest;
import VASSAL.tools.nio.file.PathSubpathTest;
import VASSAL.tools.nio.file.PathToStringTest;

import static VASSAL.tools.nio.file.AbstractMethodTest.t;

@RunWith(Suite.class)
@SuiteClasses({
  ZipFilePathOpsTest.CompareToTest.class,
  ZipFilePathOpsTest.ConstructorTest.class,
  ZipFilePathOpsTest.EndsWithTest.class,
  ZipFilePathOpsTest.EqualsTest.class,
  ZipFilePathOpsTest.GetNameTest.class,
  ZipFilePathOpsTest.GetNameCountTest.class,
  ZipFilePathOpsTest.GetNameIntTest.class,
  ZipFilePathOpsTest.GetParentTest.class,
  ZipFilePathOpsTest.GetRootTest.class,
  ZipFilePathOpsTest.HashCodeTest.class,
  ZipFilePathOpsTest.IsAbsoluteTest.class,
  ZipFilePathOpsTest.IteratorTest.class,
  ZipFilePathOpsTest.NormalizeTest.class,
  ZipFilePathOpsTest.RelativizeTest.class,
  ZipFilePathOpsTest.ResolveTest.class,
  ZipFilePathOpsTest.StartsWithTest.class,
  ZipFilePathOpsTest.SubpathTest.class,
  ZipFilePathOpsTest.ToStringTest.class
})
public class ZipFilePathOpsTest {
  protected static FileSystem fs;

  protected static final FSHandler fac = new FSHandler() {
    public FileSystem setup() { return fs; }
      
    public void teardown(FileSystem fs) {}
  };

  protected static final String zfName = "test.zip";
  protected static final String zfPathName =
    "test/VASSAL/tools/nio/file/zipfs/".replace("/", File.separator) + zfName;

  protected static Path zfPath;

  @BeforeClass
  public static void setupFS() throws IOException {
    zfPath = Paths.get(zfPathName).toAbsolutePath();

    fs = (ZipFileSystem) FileSystems.newFileSystem(
      URI.create("zip://" + zfPath), null);
  }

  @AfterClass
  public static void tearDownFS() throws IOException {
    fs.close();
  }

  @RunWith(Parameterized.class)
  public static class CompareToTest extends PathCompareToTest{
    public CompareToTest(String left, String right, Object expected) {
      super(ZipFilePathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left       Right       Expected
        { "/",        null,       t(NullPointerException.class) },
        { "/",        "/",        0                             },
        { "/",        "/foo",    -1                             },
        { "/foo",     "/",        1                             },
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ConstructorTest extends PathConstructorTest{
    public ConstructorTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input            Expected
        { "foo\u0000bar",   t(InvalidPathException.class) }, 
        { "\u0000foo",      t(InvalidPathException.class) },
        { "bar\u0000",      t(InvalidPathException.class) },
        { "//foo\u0000bar", t(InvalidPathException.class) },
        { "//\u0000foo",    t(InvalidPathException.class) },
        { "//bar\u0000",    t(InvalidPathException.class) }, 
        { "//foo",          "/foo"                        },
        { "/foo//",         "/foo"                        },
        { "foo",            "foo"                         },
        { "foo//",          "foo"                         }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class EndsWithTest extends PathEndsWithTest {
    public EndsWithTest(String left, String right, Object expected) {
      super(ZipFilePathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left       Right       Expected
        { "/",        null,       t(NullPointerException.class) },
        { "/",        "/",        true                          },
        { "/",        "foo",      false                         },
        { "/",        "/foo",     false                         },
        { "/foo",     "foo",      true                          },
        { "/foo",     "/foo",     true                          },
        { "/foo",     "/",        false                         },
        { "/foo/bar", "bar",      true                          },
        { "/foo/bar", "foo/bar",  true                          },
        { "/foo/bar", "/foo/bar", true                          },
        { "/foo/bar", "/bar",     false                         },
        { "foo",      "foo",      true                          },
        { "foo/bar",  "bar",      true                          },
        { "foo/bar",  "foo/bar",  true                          }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class EqualsTest extends PathEqualsTest {
    public EqualsTest(String left, Object right, Object expected) {
      super(ZipFilePathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left Right           Expected
        { "/",  null,           false },
        { "/",  42,             false },
        { "/",  "/",            true  },
        { "/",  "/foo",         false },
        { "/",  Paths.get("/"), false }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetNameTest extends PathGetNameTest{
    public GetNameTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input    Expected
        { "/a/b/c", "c"  },
        { "/",      null },
        { "a/b",    "b"  },
        { "a",      "a"  },
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetNameCountTest extends PathGetNameCountTest{
    public GetNameCountTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input    Expected
        { "/a/b/c", 3 },
        { "/a/b",   2 },
        { "/a",     1 },
        { "/",      0 },
        { "a/b/c",  3 },
        { "a/b",    2 },
        { "a",      1 }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetNameIntTest extends PathGetNameIntTest{
    public GetNameIntTest(String input, int index, Object expected) {
      super(ZipFilePathOpsTest.fac, input, index, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input   Index Expected
        { "a/b/c",  -1,  t(IllegalArgumentException.class) }, 
        { "a/b/c",   0,  "a"                               },
        { "a/b/c",   1,  "b"                               },
        { "a/b/c",   2,  "c"                               },
        { "a/b/c",   3,  t(IllegalArgumentException.class) },
        { "/a/b/c", -1,  t(IllegalArgumentException.class) }, 
        { "/a/b/c",  0,  "a"                               },
        { "/a/b/c",  1,  "b"                               },
        { "/a/b/c",  2,  "c"                               },
        { "/a/b/c",  3,  t(IllegalArgumentException.class) },
        { "/",       0,  t(IllegalArgumentException.class) }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetParentTest extends PathGetParentTest{
    public GetParentTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input    Expected
        { "/a/b/c", "/a/b" },
        { "/",      null   },
        { "/a",     "/"    },
        { "a/b",    "a"    },
        { "a",      null   }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetRootTest extends PathGetRootTest {
    public GetRootTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input    Expected
        { "/a/b/c", "/"  },
        { "/",      "/"  },
        { "a/b",    null },
        { "a",      null }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class HashCodeTest extends PathHashCodeTest {
    public HashCodeTest(String left, String right, Object expected) {
      super(ZipFilePathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left       Right       Expected
        { "/",        "/",        true  },
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class IsAbsoluteTest extends PathIsAbsoluteTest {
    public IsAbsoluteTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input  Expected
        { "/",    true  },
        { "/tmp", true  },
        { "tmp",  false }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class IteratorTest extends PathIteratorTest {
    public IteratorTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input    Expected
        { "/",      new String[0]                 },
        { "/a",     new String[] { "a" }          },
        { "/a/b/c", new String[] { "a", "b", "c" }},
        { "a/b/c",  new String[] { "a", "b", "c" }}
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class NormalizeTest extends PathNormalizeTest{
    public NormalizeTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input                Expected
        { "/",                  "/"         },
        { "foo",                "foo"       },
        { ".",                  null        },
        { "..",                 ".."        },
        { "/..",                "/"         },
        { "/../..",             "/"         },
        { "foo/.",              "foo"       },
        { "./foo",              "foo"       },
        { "foo/..",             null        },
        { "../foo",             "../foo"    },
        { "../../foo",          "../../foo" },
        { "foo/bar/..",         "foo"       },
        { "foo/bar/baz/../..",  "foo"       },
        { "/foo/bar/baz/../..", "/foo"      }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class RelativizeTest extends PathRelativizeTest {
    public RelativizeTest(String left, String right, Object expected) {
      super(ZipFilePathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left     Right         Expected
        { "/a/b/c", null,         t(NullPointerException.class)     },
        { "/a/b/c", "/a/b/c",     null                              },
        { "/a/b/c", "/a/b/c/d/e", "d/e"                             },
        { "/a/b/c", "/a/x",       "../../x"                         },
        { "a/b/c", "a/b/c",       null                              }, 
        { "a/b/c", "a/b/c/d/e",   "d/e"                             }, 
        { "a/b/c", "a/x",         "../../x"                         }, 
        { "a/b/c",  "/a/b/c",     t(IllegalArgumentException.class) },
        { "a/b/c",  "/a/b/c/d/e", t(IllegalArgumentException.class) },
        { "a/b/c",  "/a/x",       t(IllegalArgumentException.class) },
        { "/a/b/c", "a/b/c",      t(IllegalArgumentException.class) },
        { "/a/b/c", "a/b/c/d/e",  t(IllegalArgumentException.class) },
        { "/a/b/c", "a/x",        t(IllegalArgumentException.class) }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ResolveTest extends PathResolveTest {
    public ResolveTest(String left, String right, Object expected) {
      super(ZipFilePathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left   Right   Expected
        { "/tmp", null,   t(NullPointerException.class) },
        { "/tmp", "foo",  "/tmp/foo"                    },
        { "/tmp", "/foo", "/foo"                        },
        { "tmp",  "foo",  "tmp/foo"                     },
        { "tmp",  "/foo", "/foo"                        }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class StartsWithTest extends PathStartsWithTest{
    public StartsWithTest(String left, String right, Object expected) {
      super(ZipFilePathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left       Right       Expected
        { "/",        null,       t(NullPointerException.class) },
        { "/",        "/",        true                          },
        { "/",        "/foo",     false                         },
        { "/foo",     "/",        true                          },
        { "/foo",     "/foo",     true                          },
        { "/foo",     "/f",       false                         },
        { "/foo/bar", "/",        true                          },
        { "/foo/bar", "/foo",     true                          },
        { "/foo/bar", "/foo/bar", true                          },
        { "/foo/bar", "/f",       false                         },
        { "/foo/bar", "foo",      false                         },
        { "/foo/bar", "foo/bar",  false                         },
        { "foo",      "foo",      true                          },
        { "foo",      "f",        false                         },
        { "foo/bar",  "foo",      true                          },
        { "foo/bar",  "foo/bar",  true                          },
        { "foo/bar",  "f",        false                         },
        { "foo/bar",  "/foo",     false                         },
        { "foo/bar",  "/foo/bar", false                         }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class SubpathTest extends PathSubpathTest{
    public SubpathTest(String input, int begin, int end, Object expected) {
      super(ZipFilePathOpsTest.fac, input, begin, end, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input       Begin End Expected
        { "/",             0, 1, t(IllegalArgumentException.class) },
        { "/foo/bar/baz", -1, 0, t(IllegalArgumentException.class) },
        { "/foo/bar/baz",  0, 1, "foo"                             },
        { "/foo/bar/baz",  0, 2, "foo/bar"                         },
        { "/foo/bar/baz",  0, 3, "foo/bar/baz"                     },
        { "/foo/bar/baz",  1, 2, "bar"                             },
        { "/foo/bar/baz",  1, 3, "bar/baz"                         },
        { "/foo/bar/baz",  2, 3, "baz"                             },
        { "/foo/bar/baz",  1, 0, t(IllegalArgumentException.class) },
        { "foo/bar/baz",  -1, 0, t(IllegalArgumentException.class) },
        { "foo/bar/baz",   0, 1, "foo"                             },
        { "foo/bar/baz",   0, 2, "foo/bar"                         },
        { "foo/bar/baz",   0, 3, "foo/bar/baz"                     },
        { "foo/bar/baz",   1, 2, "bar"                             },
        { "foo/bar/baz",   1, 3, "bar/baz"                         },
        { "foo/bar/baz",   2, 3, "baz"                             },
        { "foo/bar/baz",   1, 0, t(IllegalArgumentException.class) }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ToStringTest extends PathToStringTest {
    public ToStringTest(String input, Object expected) {
      super(ZipFilePathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input    Expected
        { "/a/b/c",  "/a/b/c" },
        { "/a/b/c/", "/a/b/c" },
        { "/",       "/"      },
        { "a/b",     "a/b"    },
        { "a",       "a"      }
      });
    }
  }
}  
