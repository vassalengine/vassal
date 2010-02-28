package VASSAL.tools.nio.file.realfs;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.net.URI;
import java.util.EnumSet;
import java.util.Map;
import java.util.Set;

import VASSAL.tools.nio.file.FileAlreadyExistsException;
import VASSAL.tools.nio.file.FileRef;
import VASSAL.tools.nio.file.FileSystem;
import VASSAL.tools.nio.file.FileSystemAlreadyExistsException;
import VASSAL.tools.nio.file.OpenOption;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.ProviderMismatchException;
import VASSAL.tools.nio.file.StandardOpenOption;
import VASSAL.tools.nio.file.attribute.FileAttribute;
import VASSAL.tools.nio.file.spi.FileSystemProvider;

import static VASSAL.tools.nio.file.StandardOpenOption.*;

public class RealFileSystemProvider extends FileSystemProvider {
  protected final RealFileSystem fs;

  public RealFileSystemProvider() {
    fs = new RealFileSystem(this);
  }

  public FileSystem getFileSystem(URI uri) {
    checkURI(uri);
    if (!"/".equals(uri.getPath())) throw new IllegalArgumentException();
    return fs;
  }

  public Path getPath(URI uri) {
    return fs.getPath(uriToPath(uri));
  }
  
  protected void checkURI(URI uri) {
    if (!getScheme().equalsIgnoreCase(uri.getScheme())) {
      throw new IllegalArgumentException(
        getScheme() + " != " + uri.getScheme());
    }

    if (!uri.isAbsolute()) throw new IllegalArgumentException();
    if (uri.isOpaque()) throw new IllegalArgumentException();
    if (uri.getAuthority() != null) throw new IllegalArgumentException();
    if (uri.getQuery() != null) throw new IllegalArgumentException();
    if (uri.getFragment() != null) throw new IllegalArgumentException();
    if (uri.getPath() == null) throw new IllegalArgumentException();
  }

  protected String uriToPath(URI uri) {
    checkURI(uri);

    String path = uri.getPath();
    if (path.endsWith(File.separator) && !path.equals(File.separator)) {
      path = path.substring(0, path.length()-1);
    }

    return path;
  }

  public String getScheme() {
    return "file";
  }

  protected static final Set<StandardOpenOption> supportedOpenOptions =
    EnumSet.of(
      APPEND,
      CREATE,
      CREATE_NEW,
      READ,
      TRUNCATE_EXISTING,
      WRITE
    );

  @Override 
  public FileChannel newFileChannel(
    Path path,
    Set<? extends OpenOption> options,
    FileAttribute<?>... attrs) throws IOException
  {
    if (!(path instanceof RealPath)) throw new ProviderMismatchException();
    final File file = ((RealPath) path).file;

    if (attrs.length > 0) throw new UnsupportedOperationException();

    if (!supportedOpenOptions.containsAll(options)) 
      throw new UnsupportedOperationException();

    if (options.contains(CREATE_NEW) && file.exists())
      throw new FileAlreadyExistsException(file.toString());

    if (options.contains(APPEND)) {
      if (options.contains(READ) ||
          options.contains(TRUNCATE_EXISTING)) {
        throw new IllegalArgumentException();
      }

      return new FileOutputStream(file, true).getChannel();
    }

    if (options.contains(WRITE)) {
      if (options.contains(READ)) {
        // read-write
        final FileChannel fc = new RandomAccessFile(file, "rw").getChannel();
        if (options.contains(TRUNCATE_EXISTING)) fc.truncate(0);
        return fc;
      }
      else {
        // write-only
        return new FileOutputStream(file, false).getChannel();
      }
    }
    else {
      // read-only
      return ((RealPath) path).newInputStream().getChannel();
    }
  }
 
  @Override 
  public FileSystem newFileSystem(FileRef file, Map<String,?> env)
                                                           throws IOException {
    throw new FileSystemAlreadyExistsException();
  }

  @Override
  public FileSystem newFileSystem(URI uri, Map<String,?> env)
                                                           throws IOException {
    checkURI(uri);
    throw new FileSystemAlreadyExistsException();
  }
}
