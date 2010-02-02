package VASSAL.tools.nio.file.zipfs;

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
import VASSAL.tools.nio.file.PathSetAttributeTest;
import VASSAL.tools.nio.file.StandardCopyOption;
import VASSAL.tools.nio.file.attribute.FileTime;

import static VASSAL.tools.nio.file.AbstractPathMethodTest.t;

import static VASSAL.tools.nio.file.StandardCopyOption.REPLACE_EXISTING;

@RunWith(Suite.class)
@SuiteClasses({
  ZipFilePathWriteTest.CopyToExtIntTest.class,
  ZipFilePathWriteTest.CopyToIntExtTest.class,
  ZipFilePathWriteTest.CopyToIntIntTest.class,
  ZipFilePathWriteTest.CreateDirectoryTest.class,
  ZipFilePathWriteTest.CreateFileTest.class,
  ZipFilePathWriteTest.DeleteTest.class,
  ZipFilePathWriteTest.DeleteIfExistsTest.class,
  ZipFilePathWriteTest.MoveToExtIntTest.class,
  ZipFilePathWriteTest.MoveToIntExtTest.class,
  ZipFilePathWriteTest.MoveToIntIntTest.class,
  ZipFilePathWriteTest.SetAttributeTest.class
})
public class ZipFilePathWriteTest {

  protected static final String thisDir =
    "test/VASSAL/tools/nio/file/zipfs/".replace("/", File.separator);

  protected static final String td =
    thisDir + ("test/".replace("/", File.separator));

  protected static final String zfName = "write.zip";
  protected static final String zfPathName = td + zfName;

  protected static final FSHandler fac = new FSHandler() {
    public FileSystem setup() throws IOException {
      // clear and create our test directory
      final Path tdPath = Paths.get(td);
      FileUtils.deleteIfExists(tdPath);
      tdPath.createDirectory();
      tdPath.resolve("yea").createFile();
      
      Paths.get(thisDir + "foo").copyTo(tdPath.resolve("foo"));
      tdPath.resolve("dir").createDirectory(); 

      // work in a copy of write.zip
      final Path zfWrite = Paths.get(zfPathName).toAbsolutePath();
      final Path zfRead = Paths.get(thisDir + "write.zip").toAbsolutePath();
      zfRead.copyTo(zfWrite, REPLACE_EXISTING);

      final URI zfURI = URI.create("zip://" + zfWrite.toString());
      return FileSystems.newFileSystem(zfURI, null);
    }
      
    public void teardown(FileSystem fs) throws IOException {
      fs.close();

      // tear down our test directory
      final Path tdPath = Paths.get(td);
      FileUtils.deleteIfExists(tdPath);
    }
  };

  @RunWith(Parameterized.class)
  public static class CopyToIntIntTest extends PathCopyToIntIntTest {
    public CopyToIntIntTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(ZipFilePathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
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
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class CopyToIntExtTest extends PathCopyToIntExtTest {
    public CopyToIntExtTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(ZipFilePathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
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
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class CopyToExtIntTest extends PathCopyToExtIntTest {
    public CopyToExtIntTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(ZipFilePathWriteTest.fac, src, dst, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
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
      });
    }
  }

  // FIXME: need to test with file attributes
  @RunWith(Parameterized.class)
  public static class CreateDirectoryTest extends PathCreateDirectoryTest {
    public CreateDirectoryTest(String input, Object expected) {
      super(ZipFilePathWriteTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input           Expected
        { "/dirInZip",     t(FileAlreadyExistsException.class) },
        { "/foodir",       null                                },
        { "dirInZip",      t(FileAlreadyExistsException.class) },
        { "bardir",        null                                }
      });
    }
  }
  
  // FIXME: need to test with file attributes
  @RunWith(Parameterized.class)
  public static class CreateFileTest extends PathCreateFileTest {
    public CreateFileTest(String input, Object expected) {
      super(ZipFilePathWriteTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input           Expected
        { "/fileInZip",    t(FileAlreadyExistsException.class) },
        { "/bar",          null                                },
        { "fileInZip",     t(FileAlreadyExistsException.class) },
        { "bar",           null                                }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class DeleteTest extends PathDeleteTest {
    public DeleteTest(String input, Object expected) {
      super(ZipFilePathWriteTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input           Expected
        { "/notAFile",     t(NoSuchFileException.class)        },
        { "/foo",          null                                },
        { "/dirInZip",     t(DirectoryNotEmptyException.class) }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class DeleteIfExistsTest extends PathDeleteIfExistsTest {
    public DeleteIfExistsTest(String input, Object expected) {
      super(ZipFilePathWriteTest.fac, input, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Input
        { "/notAFile", null                                },
        { "/foo",      null                                },
        { "/dirInZip", t(DirectoryNotEmptyException.class) }
      });
    }
  }

  @RunWith(Parameterized.class)
  public static class MoveToIntIntTest extends PathMoveToIntIntTest {
    public MoveToIntIntTest(String src, String dst,
                            CopyOption[] opts, Object expected) {
      super(ZipFilePathWriteTest.fac, src, dst, opts, expected);
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
      super(ZipFilePathWriteTest.fac, src, dst, opts, expected);
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
      super(ZipFilePathWriteTest.fac, src, dst, opts, expected);
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

/*
  @RunWith(Parameterized.class)
  public static class NewOutputStreamTest extends PathNewOutputStreamTest {
    public NewOutputStreamTest(String output, OpenOption[] opts,
                                                             Object expected) {
      super(ZipFilePathReadTest.fs, output, opts, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Output       Options                     Expected
        { "/fileInZip", new OpenOption[0],          testDir + "fileInZip" },
        { "/fileInZip", new OpenOption[]{ READ },   t(IllegalArgumentException) },
        { "/fileInZip", new OpenOption[]{ APPEND },  },
        { "/foo",       new OpenOption[0],          testDir + "foo"       },
        { "foo",        new OpenOption[0],          testDir + "foo"       },
      });
    }
  }
*/

  @RunWith(Parameterized.class)
  public static class SetAttributeTest extends PathSetAttributeTest {
    public SetAttributeTest(String path, String attr,
                            Object value, Object expected) {
      super(ZipFilePathWriteTest.fac, path, attr, value, expected);
    }

    @Parameters
    public static List<Object[]> cases() {
      return Arrays.asList(new Object[][] {
        // Path   Attribute   Value   Expected
        { "foo", "bogus",       null, t(UnsupportedOperationException.class) },
        { "foo", "basic:bogus", null, t(UnsupportedOperationException.class) },
        { "foo", "bogus:bogus", null, t(UnsupportedOperationException.class) },
        { "foo", "basic:size",  null, t(UnsupportedOperationException.class) },
        { "foo", "lastModifiedTime", FileTime.fromMillis(315532800000L), null }
      });
    }
  }
}
