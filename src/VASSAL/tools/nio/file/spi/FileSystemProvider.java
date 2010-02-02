package VASSAL.tools.nio.file.spi;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.net.URI;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;

import VASSAL.tools.nio.channels.AsynchronousFileChannel;
import VASSAL.tools.nio.file.FileRef;
import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.OpenOption;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.attribute.FileAttribute;
import VASSAL.tools.nio.file.realfs.RealFileSystemProvider;
import VASSAL.tools.nio.file.zipfs.ZipFileSystemProvider;

public abstract class FileSystemProvider {
  protected FileSystemProvider() {}

  public abstract FileSystem getFileSystem(URI uri);

  public abstract Path getPath(URI uri);

  public abstract String getScheme();

  protected static List<FileSystemProvider> providers =
    Collections.unmodifiableList(
      Arrays.<FileSystemProvider>asList(
        new ZipFileSystemProvider(),
        new RealFileSystemProvider()
      )
    );

  public static List<FileSystemProvider> installedProviders() {
    return providers;    
  }

  public AsynchronousFileChannel newAsynchronousFileChannel(
    Path path,
    Set<? extends OpenOption> options,
    ExecutorService executor,
    FileAttribute<?>... attrs)
  {
    throw new UnsupportedOperationException();
  }

  public FileChannel newFileChannel(
    Path path,
    Set<? extends OpenOption> options,
    FileAttribute<?>... attrs) throws IOException
  {
    throw new UnsupportedOperationException();
  }

  public FileSystem newFileSystem(FileRef file, Map<String,?> env)
                                                           throws IOException {
    throw new UnsupportedOperationException();
  }

  public abstract FileSystem newFileSystem(URI uri, Map<String,?> env)
                                                            throws IOException;
}
