package VASSAL.tools.nio.file.realfs;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.junit.BeforeClass;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import static org.junit.Assume.assumeTrue;

import VASSAL.Info;

import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.FSHandler;
import VASSAL.tools.nio.file.InvalidPathException;
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

import static VASSAL.tools.nio.file.AbstractPathMethodTest.t;

@RunWith(Suite.class)
@SuiteClasses({
  RealWindowsPathOpsTest.CompareToTest.class,
  RealWindowsPathOpsTest.ConstructorTest.class,
  RealWindowsPathOpsTest.EndsWithTest.class,
  RealWindowsPathOpsTest.EqualsTest.class,
  RealWindowsPathOpsTest.GetNameTest.class,
  RealWindowsPathOpsTest.GetNameCountTest.class,
  RealWindowsPathOpsTest.GetNameIntTest.class,
  RealWindowsPathOpsTest.GetParentTest.class,
  RealWindowsPathOpsTest.GetRootTest.class,
  RealWindowsPathOpsTest.IsAbsoluteTest.class,
  RealWindowsPathOpsTest.IteratorTest.class,
  RealWindowsPathOpsTest.NormalizeTest.class,
  RealWindowsPathOpsTest.RelativizeTest.class,
  RealWindowsPathOpsTest.ResolveTest.class,
  RealWindowsPathOpsTest.StartsWithTest.class,
  RealWindowsPathOpsTest.SubpathTest.class,
  RealWindowsPathOpsTest.ToStringTest.class
})
public class RealWindowsPathOpsTest {
  protected static FileSystem fs;

  protected static final FSHandler fac = new FSHandler() {
    public FileSystem setup() { return fs; }
      
    public void teardown(FileSystem fs) {}
  };

  @BeforeClass
  public static void setupFS() throws IOException {
    assumeTrue(Info.isWindows());

    final RealFileSystemProvider provider = new RealFileSystemProvider();
    fs = new RealFileSystem(provider);
  }

  @RunWith(Parameterized.class)
  public static class CompareToTest extends PathCompareToTest{
    public CompareToTest(String left, String right, Object expected) {
      super(RealWindowsPathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left       Right       Expected
        { "C:\\",     null,       t(NullPointerException.class) },
        { "C:\\",     "C:\\",     0                             },
        { "C:\\",     "C:\\foo", -1                             },
        { "C:\\foo",  "C:\\",     1                             }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ConstructorTest extends PathConstructorTest{
    public ConstructorTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input                     Expected
        { ":\\foo",                  t(InvalidPathException.class) }, 
        { "C::",                     t(InvalidPathException.class) },
        { "C:\\?",                   t(InvalidPathException.class) },
        { "C:\\*",                   t(InvalidPathException.class) },
        { "C:\\abc\u0001\\foo",      t(InvalidPathException.class) },
        { "C:\\\u0019\\foo",         t(InvalidPathException.class) },
        { "\\\\server",              t(InvalidPathException.class) },    
        { "\\\\server\\",            t(InvalidPathException.class) },    
        { "\\\\server\u0019\\share", t(InvalidPathException.class) },
        { "\\\\server\\share\u0019", t(InvalidPathException.class) },
        { "foo\u0000\\bar",          t(InvalidPathException.class) },
        { "C:\\foo ",                t(InvalidPathException.class) },
        { "C:\\foo \\bar",           t(InvalidPathException.class) },
        { "C:/a/b/c",                "C:\\a\\b\\c"                 },
        { "C://a//b//c",             "C:\\a\\b\\c"                 }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class EndsWithTest extends PathEndsWithTest {
    public EndsWithTest(String left, String right, Object expected) {
      super(RealWindowsPathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left                Right                  Expected
        { "C:\\",              null,                  t(NullPointerException.class) },
        { "C:\\",              "C:\\",                true  },
        { "C:\\",              "c:\\",                true  },
        { "C:\\",              "\\",                  false },
        { "C:",                "C:",                  true  },
        { "C:",                "c:",                  true  },
        { "\\",                "\\",                  true  },
        { "C:\\foo\\bar",      "bar",                 true  },
        { "C:\\foo\\bar",      "BAR",                 true  },
        { "C:\\foo\\bar",      "foo\\bar",            true  },
        { "C:\\foo\\bar",      "Foo\\bar",            true  },
        { "C:\\foo\\bar",      "C:\\foo\\bar",        true  },
        { "C:\\foo\\bar",      "c:\\foO\\baR",        true  },
        { "C:\\foo\\bar",      "r",                   false },
        { "C:\\foo\\bar",      "\\foo\\bar",          false },
        { "\\foo\\bar",        "bar",                 true  },
        { "\\foo\\bar",        "BaR",                 true  },
        { "\\foo\\bar",        "foo\\bar",            true  },
        { "\\foo\\bar",        "foO\\baR",            true  },
        { "\\foo\\bar",        "\\foo\\bar",          true  },
        { "\\foo\\bar",        "\\Foo\\Bar",          true  },
        { "\\foo\\bar",        "oo\\bar",             false },
        { "foo\\bar",          "bar",                 true  },
        { "foo\\bar",          "BAR",                 true  },
        { "foo\\bar",          "foo\\bar",            true  },
        { "foo\\bar",          "Foo\\Bar",            true  },
        { "foo\\bar",          "ar",                  false },
        { "\\\\server\\share", "\\\\server\\share",   true  },
        { "\\\\server\\share", "\\\\server\\share\\", true  },
        { "\\\\server\\share", "shared",              false },
        { "\\\\server\\share", "\\",                  false }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class EqualsTest extends PathEqualsTest {
    public EqualsTest(String left, Object right, Object expected) {
      super(RealWindowsPathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left   Right      Expected
        { "C:\\", null,      false },
        { "C:\\", 42,        false },
        { "C:\\", "C:\\",    true  },
        { "C:\\", "C:\\foo", false }
// FIXME: should also check against Path from another provider
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetNameTest extends PathGetNameTest{
    public GetNameTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input                  Expected
        { "C:\\a\\b\\c",          "c"   },
        { "C:a\\b\\c",            "c"   },
        { "\\\\server\\share\\a", "a"   },
        { "C:\\",                 null  },
        { "C:",                   null  },
        { "\\\\server\\share",    null  },
        { "\\\\server\\share\\",  null  },
        { "a\\b",                 "b"   },
        { "foo",                  "foo" }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetNameCountTest extends PathGetNameCountTest{
    public GetNameCountTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
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
      super(RealWindowsPathOpsTest.fac, input, index, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input      Index  Expected
        { "a\\b\\c",     -1,  t(IllegalArgumentException.class) }, 
        { "a\\b\\c",      0,  "a"                               },
        { "a\\b\\c",      1,  "b"                               },
        { "a\\b\\c",      2,  "c"                               },
        { "a\\b\\c",      3,  t(IllegalArgumentException.class) },
        { "C:\\a\\b\\c", -1,  t(IllegalArgumentException.class) }, 
        { "C:\\a\\b\\c",  0,  "a"                               },
        { "C:\\a\\b\\c",  1,  "b"                               },
        { "C:\\a\\b\\c",  2,  "c"                               },
        { "C:\\a\\b\\c",  3,  t(IllegalArgumentException.class) },
        { "C:\\",         0,  t(IllegalArgumentException.class) },
        { "\\\\server\\a\\b\\c", -1, t(IllegalArgumentException.class) }, 
        { "\\\\server\\a\\b\\c",  0, "a"                               },
        { "\\\\server\\a\\b\\c",  1, "b"                               },
        { "\\\\server\\a\\b\\c",  2, "c"                               },
        { "\\\\server\\a\\b\\c",  3, t(IllegalArgumentException.class) }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetParentTest extends PathGetParentTest{
    public GetParentTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input                  Expected
        { "C:\\a\\b\\c",          "C:\\a\\b"            },
        { "C:a\\b\\c",            "C:a\\b"              },
        { "\\\\server\\share\\a", "\\\\server\\share\\" },
        { "C:\\",                 null                  },
        { "C:",                   null                  },
        { "\\\\server\\share",    null                  },
        { "\\\\server\\share\\",  null                  },
        { "a\\b",                 "a"                   },
        { "foo",                  null                  }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class GetRootTest extends PathGetRootTest {
    public GetRootTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input                  Expected
        { "C:\\a\\b\\c",          "C:\\"                },
        { "C:a\\b\\c",            "C:"                  },
        { "\\\\server\\share\\a", "\\\\server\\share\\" },
        { "C:\\",                 "C:\\"                },
        { "C:",                   "C:"                  },
        { "\\\\server\\share",    "\\\\server\\share\\" },
        { "\\\\server\\share\\",  "\\\\server\\share\\" },
        { "a\\b",                 null                  },
        { "foo",                  null                  }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class HashCodeTest extends PathHashCodeTest {
    public HashCodeTest(String left, String right, Object expected) {
      super(RealWindowsPathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left       Right       Expected
        { "C:\\foo",  "C:\\foo",  true },
        { "C:\\foo",  "c:\\FOO",  true }  // Windows is not case-sensitive
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class IsAbsoluteTest extends PathIsAbsoluteTest {
    public IsAbsoluteTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input                 Expected
        { "foo",                 false },
        { "C:",                  false },
        { "C:\\",                true  },
        { "C:\\abc",             true  },
        { "\\\\server\\share\\", true  }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class IteratorTest extends PathIteratorTest {
    public IteratorTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input    Expected
        { "C:\\",                new String[0]                 },
        { "C:",                  new String[0]                 },
        { "\\\\server\\share\\", new String[0]                 },
        { "C:\\a",               new String[] { "a" }          },
        { "C:\\a\\b\\c",         new String[] { "a", "b", "c" }},
        { "a\\b\\c",             new String[] { "a", "b", "c" }}
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class NormalizeTest extends PathNormalizeTest{
    public NormalizeTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input                               Expected
        { "C:\\",                              "C:\\"                   },
        { "C:\\.",                             "C:\\"                   },
        { "C:\\..",                            "C:\\"                   },
        { "\\\\server\\share",                 "\\\\server\\share\\"    },
        { "\\\\server\\share\\.",              "\\\\server\\share\\"    },
        { "\\\\server\\share\\..",             "\\\\server\\share\\"    },
        { "C:",                                "C:"                     },
        { "C:.",                               "C:"                     },
        { "C:..",                              "C:.."                   },
        { "\\",                                "\\"                     },
        { "\\.",                               "\\"                     },
        { "\\..",                              "\\"                     },
        { "foo",                               "foo"                    },
        { "foo\\",                             "foo"                    },
        { "foo\\..",                           null                     },
        { "C:\\foo",                           "C:\\foo"                },
        { "C:\\foo\\.",                        "C:\\foo"                },
        { "C:\\.\\foo",                        "C:\\foo"                },
        { "C:\\foo\\..",                       "C:\\"                   },
        { "C:\\..\\foo",                       "C:\\foo"                },
        { "\\\\server\\share\\foo",            "\\\\server\\share\\foo" },
        { "\\\\server\\share\\foo\\.",         "\\\\server\\share\\foo" },
        { "\\\\server\\share\\.\\foo",         "\\\\server\\share\\foo" },
        { "\\\\server\\share\\foo\\..",        "\\\\server\\share\\"    },
        { "\\\\server\\share\\..\\foo",        "\\\\server\\share\\foo" },
        { "C:foo",                             "C:foo"                  },
        { "C:foo\\.",                          "C:foo"                  },
        { "C:.\\foo",                          "C:foo"                  },
        { "C:foo\\..",                         "C:"                     },
        { "C:..\\foo",                         "C:..\\foo"              },
        { "\\foo",                             "\\foo"                  },
        { "\\foo\\.",                          "\\foo"                  },
        { "\\.\\foo",                          "\\foo"                  },
        { "\\foo\\..",                         "\\"                     },
        { "\\..\\foo",                         "\\foo"                  },
        { ".",                                 null                     },
        { "..",                                ".."                     },
        { "\\..\\..",                          "\\"                     },
        { "..\\..\\foo",                       "..\\..\\foo"            },
        { "foo\\bar\\..",                      "foo"                    },
        { "foo\\bar\\.\\..",                   "foo"                    },
        { "foo\\bar\\baz\\..\\..",             "foo"                    },
        { ".\\foo\\.\\bar\\.\\baz\\..\\.\\..", "foo"                    }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class RelativizeTest extends PathRelativizeTest {
    public RelativizeTest(String left, String right, Object expected) {
      super(RealWindowsPathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left                     Right                     Expected
        { "foo\\bar",               null,                     t(NullPointerException.class) },
        { "foo\\bar",               "foo\\bar",               null      },
        { "foo\\bar",               "foo",                    ".."      },
        { "C:\\a\\b\\c",            "C:\\a",                  "..\\.."  },
        { "\\\\server\\share\\foo", "\\\\server\\share\\bar", "..\\bar" }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ResolveTest extends PathResolveTest {
    public ResolveTest(String left, String right, Object expected) {
      super(RealWindowsPathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left   Right   Expected
        { "C:\\", null, t(NullPointerException.class) },
        { "C:\\", "foo", "C:\\foo" },
        { "C:\\", "\\\\server\\share\\bar", "\\\\server\\share\\bar" },
        { "C:\\", "C:foo", "C:\\foo" },
        { "C:\\", "D:foo", "D:foo" },
        { "\\", "foo", "\\foo" },
        { "\\", "D:bar", "D:bar" },
        { "\\", "C:\\bar", "C:\\bar" },
        { "\\", "\\\\server\\share\\bar",  "\\\\server\\share\\bar" },
        { "\\", "\\bar", "\\bar" },
        { "foo", "bar", "foo\\bar" },
        { "foo", "D:\\bar", "D:\\bar" },
        { "foo", "\\\\server\\share\\bar", "\\\\server\\share\\bar" },
        { "foo", "C:bar", "C:bar" },
        { "foo", "D:foo", "D:foo" },
        { "C:", "foo", "C:foo" },
        { "\\\\server\\share\\foo", "bar", "\\\\server\\share\\foo\\bar" },
        { "\\\\server\\share\\foo", "\\bar", "\\\\server\\share\\bar" },
        { "\\\\server\\share\\foo", "D:\\bar", "D:\\bar" },
        { "\\\\server\\share\\foo", "\\other\\share\\bar", "\\\\other\\share\\bar" },
        { "\\\\server\\share\\foo", "D:bar", "D:bar" }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class StartsWithTest extends PathStartsWithTest{
    public StartsWithTest(String left, String right, Object expected) {
      super(RealWindowsPathOpsTest.fac, left, right, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Left                Right                  Expected
        { "C:\\",              null,                  t(NullPointerException.class) },
        { "C:\\",              "C:\\",                true  },
        { "C:\\",              "c:\\",                true  },
        { "C:\\",              "C",                   false },
        { "C:\\",              "C:",                  false },
        { "C:",                "C:",                  true  },
        { "C:",                "c:",                  true  },
        { "C:",                "C",                   false },
        { "\\",                "\\",                  true  },
        { "C:\\foo\\bar",      "C:\\",                true  },
        { "C:\\foo\\bar",      "C:\\foo",             true  },
        { "C:\\foo\\bar",      "C:\\FOO",             true  },
        { "C:\\foo\\bar",      "C:\\foo\\bar",        true  },
        { "C:\\foo\\bar",      "C:\\foo\\Bar",        true  },
        { "C:\\foo\\bar",      "C:",                  false },
        { "C:\\foo\\bar",      "C",                   false },
        { "C:\\foo\\bar",      "C:foo",               false },
        { "\\foo\\bar",        "\\",                  true  },
        { "\\foo\\bar",        "\\foo",               true  },
        { "\\foo\\bar",        "\\foO",               true  },
        { "\\foo\\bar",        "\\foo\\bar",          true  },
        { "\\foo\\bar",        "\\fOo\\BaR",          true  },
        { "\\foo\\bar",        "foo",                 false },
        { "\\foo\\bar",        "foo\\bar",            false },
        { "foo\\bar",          "foo",                 true  },
        { "foo\\bar",          "foo\\bar",            true  },
        { "foo\\bar",          "\\",                  false },
        { "\\\\server\\share", "\\\\server\\share",   true  },
        { "\\\\server\\share", "\\\\server\\share\\", true  },
        { "\\\\server\\share", "\\",                  false }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class SubpathTest extends PathSubpathTest{
    public SubpathTest(String input, int begin, int end, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, begin, end, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input            Begin End Expected
        { "C:\\",               0, 1, t(IllegalArgumentException.class) },
        { "C:\\foo\\bar\\baz", -1, 0, t(IllegalArgumentException.class) },
        { "C:\\foo\\bar\\baz",  0, 1, "foo"                             },
        { "C:\\foo\\bar\\baz",  0, 2, "foo\\bar"                        },
        { "C:\\foo\\bar\\baz",  0, 3, "foo\\bar\\baz"                   },
        { "C:\\foo\\bar\\baz",  1, 2, "bar"                             },
        { "C:\\foo\\bar\\baz",  1, 3, "bar\\baz"                        },
        { "C:\\foo\\bar\\baz",  2, 3, "baz"                             },
        { "C:\\foo\\bar\\baz",  1, 0, t(IllegalArgumentException.class) },
        { "foo\\bar\\baz",     -1, 0, t(IllegalArgumentException.class) },
        { "foo\\bar\\baz",      0, 1, "foo"                             },
        { "foo\\bar\\baz",      0, 2, "foo\\bar"                        },
        { "foo\\bar\\baz",      0, 3, "foo\\bar\\baz"                   },
        { "foo\\bar\\baz",      1, 2, "bar"                             },
        { "foo\\bar\\baz",      1, 3, "bar\\baz"                        },
        { "foo\\bar\\baz",      2, 3, "baz"                             },
        { "foo\\bar\\baz",      1, 0, t(IllegalArgumentException.class) },
        { "\\\\server\\share\\foo\\bar\\baz", -1, 0, t(IllegalArgumentException.class) },
        { "\\\\server\\share\\foo\\bar\\baz", 0, 1, "foo"               },
        { "\\\\server\\share\\foo\\bar\\baz", 0, 2, "foo\\bar"          },
        { "\\\\server\\share\\foo\\bar\\baz", 0, 3, "foo\\bar\\baz"     },
        { "\\\\server\\share\\foo\\bar\\baz", 1, 2, "bar"               },
        { "\\\\server\\share\\foo\\bar\\baz", 1, 3, "bar\\baz"          },
        { "\\\\server\\share\\foo\\bar\\baz", 2, 3, "baz"               },
        { "\\\\server\\share\\foo\\bar\\baz", 1, 0, t(IllegalArgumentException.class) }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class ToStringTest extends PathToStringTest {
    public ToStringTest(String input, Object expected) {
      super(RealWindowsPathOpsTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input    Expected
        { "C:\\a\\b\\c",   "C:\\a\\b\\c" },
        { "C:\\a\\b\\c\\", "C:\\a\\b\\c" },
        { "C:\\",          "C:\\"        },
        { "a\\b",          "a\\b"        },
        { "a",             "a"           }
      });
    }
  }
}
