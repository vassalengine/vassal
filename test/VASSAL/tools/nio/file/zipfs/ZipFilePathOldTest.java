package VASSAL.tools.nio.file.zipfs;

import static org.junit.Assert.*;
import static org.junit.Assume.*;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Iterator;
import java.util.Scanner;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import VASSAL.tools.io.IOUtils;
import VASSAL.tools.nio.channels.SeekableByteChannel;
import VASSAL.tools.nio.file.AccessDeniedException;
import VASSAL.tools.nio.file.AccessMode;
import VASSAL.tools.nio.file.FileSystems;
import VASSAL.tools.nio.file.LinkOption;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.Paths;
import VASSAL.tools.nio.file.NoSuchFileException;
import VASSAL.tools.nio.file.ReadOnlyFileSystemException;
import VASSAL.tools.nio.file.StandardOpenOption;
import VASSAL.tools.nio.file.StandardCopyOption;
import VASSAL.tools.nio.file.WatchEvent;
import VASSAL.tools.nio.file.attribute.BasicFileAttributeView;
import VASSAL.tools.nio.file.attribute.FileAttributeView;
import VASSAL.tools.nio.file.attribute.FileTime;

public class ZipFilePathOldTest {

  final String zipScheme = "zip";
  final String testZipFileName = "testZipFile.zip";
  final String pathToTestZipFileName = "test/VASSAL/tools/nio/file/zipfs/".replace("/", File.separator) + testZipFileName;

  Path testZipFilePath;

  ZipFileSystem fs;
  
  final String testFileCreatedName = "testFileInZip.txt";
  ZipFilePath pathTestFileCreated; 

  final String testingDirectoryName = "dirInZip";
  ZipFilePath pathTestingDirectory;
 
  final String testingDirectory2Name = "dirInZip/foo";
  ZipFilePath pathTestingDirectory2; 
 
  final String testFileOtherName = testingDirectoryName + "/testFileVolatile";
  ZipFilePath pathTestFileOther;
  
  final String testDirOtherName = "/testDirOther";
  ZipFilePath pathTestDirOther;
  
  Path externalPath;

  @Before
  public void setUp() throws Exception {
    testZipFilePath = Paths.get(pathToTestZipFileName).toAbsolutePath();
    externalPath = testZipFilePath.getParent().resolve("tmpFile");

    fs = (ZipFileSystem) FileSystems.newFileSystem(
      URI.create("zip://" + testZipFilePath), null);

    pathTestingDirectory = fs.getPath(testingDirectoryName);
    pathTestingDirectory2 = fs.getPath(testingDirectory2Name);
    pathTestDirOther = fs.getPath(testDirOtherName);
    pathTestFileCreated = fs.getPath(testFileCreatedName);
    pathTestFileOther = fs.getPath(testFileOtherName);
  }

/*
  @Test
  public void testGetFileStore() throws IOException {
// FIXME: File stores not guaranteed to be identical, and FileStore
// does not reimiplement equals().
    assertEquals(fs.getFileStores().iterator().next(),
                 pathTestingDirectory.getFileStore());
  }
*/

  // FIXME can check success only with lack of exception. 
  @Test
  public void testNewDirectoryStream() throws IOException {
    pathTestingDirectory.newDirectoryStream();
  }

  @Test
  public void testNewDirectoryStreamFilterOfQsuperPath() throws IOException {
    assertTrue(pathTestingDirectory.newDirectoryStream().iterator().hasNext());
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testNewDirectoryStreamString() throws IOException {
    pathTestingDirectory.newDirectoryStream("anyGlob");
  }

/* 
  @Test
  public void testGetFileAttributeViewBasicFileAttributeView() {
    assertNotNull(pathTestFileCreated.getFileAttributeView(BasicFileAttributeView.class));
  }

  @Test
  public void testGetFileAttributeViewFileAttributeView() {
    assertNull(pathTestFileCreated.getFileAttributeView(FileAttributeView.class));
  }

  @Test
  public void testGetFileAttributeViewLinkOptions() {
    assertNotNull(pathTestFileCreated.getFileAttributeView(BasicFileAttributeView.class, LinkOption.NOFOLLOW_LINKS));
  }
*/

/*
  @Test(expected = UnsupportedOperationException.class)
  public void testRegisterWatchServiceKindOfQArray() throws IOException {
    pathTestFileOther.register(null);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void testRegisterWatchServiceKindOfQArrayModifierArray() throws IOException {
    pathTestFileOther.register(null, (WatchEvent.Kind<?>[]) null);
  }
*/
}
