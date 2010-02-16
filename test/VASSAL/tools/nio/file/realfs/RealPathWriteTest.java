package VASSAL.tools.nio.file.realfs;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.List;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import VASSAL.tools.io.FileUtils;

import VASSAL.tools.nio.file.CopyOption;
import VASSAL.tools.nio.file.DirectoryNotEmptyException;
import VASSAL.tools.nio.file.FileAlreadyExistsException;
import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.FileSystems;
import VASSAL.tools.nio.file.FSHandler;
import VASSAL.tools.nio.file.NoSuchFileException;
import VASSAL.tools.nio.file.OpenOption;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.Paths;
import VASSAL.tools.nio.file.PathCopyToExtIntTest;
import VASSAL.tools.nio.file.PathCopyToIntExtTest;
import VASSAL.tools.nio.file.PathCopyToIntIntTest;
import VASSAL.tools.nio.file.PathCreateDirectoryTest;
import VASSAL.tools.nio.file.PathCreateFileTest;
import VASSAL.tools.nio.file.PathDeleteTest;
import VASSAL.tools.nio.file.PathDeleteIfExistsTest;
import VASSAL.tools.nio.file.PathMoveToExtIntTest;
import VASSAL.tools.nio.file.PathMoveToIntExtTest;
import VASSAL.tools.nio.file.PathMoveToIntIntTest;
import VASSAL.tools.nio.file.PathNewOutputStreamTest;
import VASSAL.tools.nio.file.PathSetAttributeTest;
import VASSAL.tools.nio.file.StandardCopyOption;
import VASSAL.tools.nio.file.attribute.FileTime;

import static VASSAL.tools.nio.file.AbstractMethodTest.t;

import static VASSAL.tools.nio.file.StandardOpenOption.APPEND;
import static VASSAL.tools.nio.file.StandardOpenOption.CREATE_NEW;
import static VASSAL.tools.nio.file.StandardOpenOption.READ;
import static VASSAL.tools.nio.file.StandardOpenOption.WRITE;
import static VASSAL.tools.nio.file.StandardCopyOption.REPLACE_EXISTING;

@RunWith(Suite.class)
@SuiteClasses({
  RealPathWriteTest.CopyToExtIntTest.class,
  RealPathWriteTest.CopyToIntExtTest.class,
  RealPathWriteTest.CopyToIntIntTest.class,
  RealPathWriteTest.CreateDirectoryTest.class,
  RealPathWriteTest.CreateFileTest.class,
  RealPathWriteTest.DeleteTest.class,
  RealPathWriteTest.DeleteIfExistsTest.class,
  RealPathWriteTest.MoveToExtIntTest.class,
  RealPathWriteTest.MoveToIntExtTest.class,
  RealPathWriteTest.MoveToIntIntTest.class,
  RealPathWriteTest.NewOutputStreamTest.class,
  RealPathWriteTest.SetAttributeTest.class
})
public class RealPathWriteTest {

  protected static final String thisDir =
    "test/VASSAL/tools/nio/file/realfs/writetest/".replace("/", File.separator);

  protected static final String td =
    thisDir + ("../tmp/".replace("/", File.separator));

  protected static final FSHandler fac = new FSHandler() {
    public FileSystem setup() throws IOException {
      // clear and create our test directory
      final Path tdPath = Paths.get(td);
      FileUtils.deleteIfExists(tdPath);
      FileUtils.copy(Paths.get(thisDir), tdPath);

      return FileSystems.getDefault();
    }
      
    public void teardown(FileSystem fs) throws IOException {
      // tear down our test directory
      final Path tdPath = Paths.get(td);
      FileUtils.deleteIfExists(tdPath);
    }
  };

  @RunWith(Parameterized.class)
  public static class CopyToIntIntTest extends PathCopyToIntIntTest {
    public CopyToIntIntTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(RealPathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
/*
        // Source Destination Opts  Expected
        { "foo",  null,  null, t(NullPointerException.class)             },
        { "bar",  "nay", null, t(NoSuchFileException.class)              },
        { "/bar", "nay", null, t(NoSuchFileException.class)              },
        { "foo",  "nay", null, null                                      },
        { "foo",  "yea", null, t(FileAlreadyExistsException.class)       },
        { "/foo", "nay", null, null                                      },
        { "/foo", "yea", null, t(FileAlreadyExistsException.class)       },
        { "foo",  "yea", new CopyOption[]{ REPLACE_EXISTING }, null      },
        { "/foo", "yea", new CopyOption[]{ REPLACE_EXISTING }, null      },
        { "dirInZip", "yea", null, t(FileAlreadyExistsException.class)   },
        { "dirInZip", "yea", new CopyOption[]{ REPLACE_EXISTING }, null  },
        { "/dirInZip", "yea", null, t(FileAlreadyExistsException.class)  },
        { "/dirInZip", "yea", new CopyOption[]{ REPLACE_EXISTING }, null }
*/
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class CopyToIntExtTest extends PathCopyToIntExtTest {
    public CopyToIntExtTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(RealPathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
/*
        // Source Destination Opts  Expected
        { "foo",  null,       null, t(NullPointerException.class)             },
        { "bar",  td + "nay", null, t(NoSuchFileException.class)              },
        { "/bar", td + "nay", null, t(NoSuchFileException.class)              },
        { "foo",  td + "nay", null, null                                      },
        { "foo",  td + "yea", null, t(FileAlreadyExistsException.class)       },
        { "/foo", td + "nay", null, null                                      },
        { "/foo", td + "yea", null, t(FileAlreadyExistsException.class)       },
        { "foo",  td + "yea", new CopyOption[]{ REPLACE_EXISTING }, null      },
        { "/foo", td + "yea", new CopyOption[]{ REPLACE_EXISTING }, null      },
        { "dirInZip", td + "yea", null, t(FileAlreadyExistsException.class)   },
        { "dirInZip", td + "yea", new CopyOption[]{ REPLACE_EXISTING }, null  },
        { "/dirInZip", td + "yea", null, t(FileAlreadyExistsException.class)  },
        { "/dirInZip", td + "yea", new CopyOption[]{ REPLACE_EXISTING }, null }
*/
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class CopyToExtIntTest extends PathCopyToExtIntTest {
    public CopyToExtIntTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(RealPathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
/*
        // Source Destination Opts  Expected
        { td + "foo", null,   null, t(NullPointerException.class)        },
        { td + "bar", "nay",  null, t(NoSuchFileException.class)         },
        { td + "bar", "/nay", null, t(NoSuchFileException.class)         },
        { td + "foo", "nay",  null, null                                 },
        { td + "foo", "yea",  null, t(FileAlreadyExistsException.class)  },
        { td + "foo", "/nay", null, null                                 },
        { td + "foo", "/yea", null, t(FileAlreadyExistsException.class)  },
        { td + "foo", "yea",  new CopyOption[]{ REPLACE_EXISTING }, null },
        { td + "foo", "/yea", new CopyOption[]{ REPLACE_EXISTING }, null },
        { td + "dir", "yea",  null, t(FileAlreadyExistsException.class)  },
        { td + "dir", "yea",  new CopyOption[]{ REPLACE_EXISTING }, null },
        { td + "dir", "/yea", null, t(FileAlreadyExistsException.class)  },
        { td + "dir", "/yea", new CopyOption[]{ REPLACE_EXISTING }, null }
*/
      });
    }
  }

  // FIXME: need to test with file attributes
  @RunWith(Parameterized.class)
  public static class CreateDirectoryTest extends PathCreateDirectoryTest {
    public CreateDirectoryTest(String input, Object expected) {
      super(RealPathWriteTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
/*
        // Input           Expected
        { "/dirInZip",     t(FileAlreadyExistsException.class) },
        { "/foodir",       null                                },
        { "dirInZip",      t(FileAlreadyExistsException.class) },
        { "bardir",        null                                }
*/
      });
    }
  }
  
  // FIXME: need to test with file attributes
  @RunWith(Parameterized.class)
  public static class CreateFileTest extends PathCreateFileTest {
    public CreateFileTest(String input, Object expected) {
      super(RealPathWriteTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
/*
        // Input           Expected
        { "/fileInZip",    t(FileAlreadyExistsException.class) },
        { "/bar",          null                                },
        { "fileInZip",     t(FileAlreadyExistsException.class) },
        { "bar",           null                                }
*/
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class DeleteTest extends PathDeleteTest {
    public DeleteTest(String input, Object expected) {
      super(RealPathWriteTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
/*
        // Input       Expected
        { "/",         t(IOException.class)                },
        { "/notAFile", t(NoSuchFileException.class)        },
        { "/foo",      null                                },
        { "/dirInZip", t(DirectoryNotEmptyException.class) }
*/
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class DeleteIfExistsTest extends PathDeleteIfExistsTest {
    public DeleteIfExistsTest(String input, Object expected) {
      super(RealPathWriteTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
/*
        // Input
        { "/",         t(IOException.class)                },
        { "/notAFile", null                                },
        { "/foo",      null                                },
        { "/dirInZip", t(DirectoryNotEmptyException.class) }
*/
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class MoveToIntIntTest extends PathMoveToIntIntTest {
    public MoveToIntIntTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(RealPathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Source Destination Opts  Expected
/*
        { "foo",  null,       null, t(NullPointerException.class)             },
        { "bar",  td + "nay", null, t(NoSuchFileException.class)              },
        { "/bar", td + "nay", null, t(NoSuchFileException.class)              },
        { "foo",  td + "nay", null, null                                      },
        { "foo",  td + "yea", null, t(FileAlreadyExistsException.class)       },
        { "/foo", td + "nay", null, null                                      },
        { "/foo", td + "yea", null, t(FileAlreadyExistsException.class)       },
        { "foo",  td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null      },
        { "/foo", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null      },
        { "dirInZip", td + "yea", null, t(FileAlreadyExistsException.class)   },
        { "dirInZip", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null  },
        { "/dirInZip", td + "yea", null, t(FileAlreadyExistsException.class)  },
        { "/dirInZip", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null }
*/
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class MoveToIntExtTest extends PathMoveToIntExtTest {
    public MoveToIntExtTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(RealPathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Source Destination Opts  Expected
/*
        { "foo",  null,       null, t(NullPointerException.class)             },
        { "bar",  td + "nay", null, t(NoSuchFileException.class)              },
        { "/bar", td + "nay", null, t(NoSuchFileException.class)              },
        { "foo",  td + "nay", null, null                                      },
        { "foo",  td + "yea", null, t(FileAlreadyExistsException.class)       },
        { "/foo", td + "nay", null, null                                      },
        { "/foo", td + "yea", null, t(FileAlreadyExistsException.class)       },
        { "foo",  td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null      },
        { "/foo", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null      },
        { "dirInZip", td + "yea", null, t(FileAlreadyExistsException.class)   },
        { "dirInZip", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null  },
        { "/dirInZip", td + "yea", null, t(FileAlreadyExistsException.class)  },
        { "/dirInZip", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null }
*/
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class MoveToExtIntTest extends PathMoveToExtIntTest {
    public MoveToExtIntTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(RealPathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Source Destination Opts  Expected
/*
        { "foo",  null,       null, t(NullPointerException.class)             },
        { "bar",  td + "nay", null, t(NoSuchFileException.class)              },
        { "/bar", td + "nay", null, t(NoSuchFileException.class)              },
        { "foo",  td + "nay", null, null                                      },
        { "foo",  td + "yea", null, t(FileAlreadyExistsException.class)       },
        { "/foo", td + "nay", null, null                                      },
        { "/foo", td + "yea", null, t(FileAlreadyExistsException.class)       },
        { "foo",  td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null      },
        { "/foo", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null      },
        { "dirInZip", td + "yea", null, t(FileAlreadyExistsException.class)   },
        { "dirInZip", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null  },
        { "/dirInZip", td + "yea", null, t(FileAlreadyExistsException.class)  },
        { "/dirInZip", td + "yea", new MoveOption[]{ REPLACE_EXISTING }, null }
*/
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class NewOutputStreamTest extends PathNewOutputStreamTest {
    public NewOutputStreamTest(String src, String dst,
                               OpenOption[] opts, Object expected) {
      super(RealPathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Source     Destination   Options            Expected
        { td + "foo", td + "bar", new OpenOption[0], td + "foo" },
        { td + "foo", td + "bar", new OpenOption[]{ READ }, t(IllegalArgumentException.class) },
        { td + "foo", td + "bar", new OpenOption[]{ APPEND }, td + "foo" },
        { td + "foo", td + "alsofoo", new OpenOption[]{ APPEND }, td + "foofoo" },
        { td + "foo", td + "dst", new OpenOption[]{ CREATE_NEW }, t(FileAlreadyExistsException.class) },
        { td + "foo", td + "bar", new OpenOption[]{ CREATE_NEW }, td + "foo" }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class SetAttributeTest extends PathSetAttributeTest {
    public SetAttributeTest(String path, String attr,
                            Object value, Object expected) {
      super(RealPathWriteTest.fac, path, attr, value, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
/*
        // Path   Attribute   Value   Expected
        { "foo", "bogus",       null, t(UnsupportedOperationException.class) },
        { "foo", "basic:bogus", null, t(UnsupportedOperationException.class) },
        { "foo", "bogus:bogus", null, t(UnsupportedOperationException.class) },
        { "foo", "basic:size",  null, t(UnsupportedOperationException.class) },
        { "foo", "lastModifiedTime", FileTime.fromMillis(315532800000L), null }
*/
      });
    }
  }
}
