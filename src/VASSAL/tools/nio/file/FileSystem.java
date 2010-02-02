package VASSAL.tools.nio.file;

import java.io.IOException;
import java.util.Set;

import VASSAL.tools.nio.file.attribute.UserPrincipalLookupService;
import VASSAL.tools.nio.file.spi.FileSystemProvider;

public abstract class FileSystem {

  public abstract void close() throws IOException;

  public abstract Iterable<FileStore> getFileStores();

  public abstract Path getPath(String path);

  public abstract PathMatcher getPathMatcher(String syntaxAndPatttern);

  public abstract Iterable<Path> getRootDirectories();

  public abstract String getSeparator();

  public abstract UserPrincipalLookupService getUserPrincipalLookupService();

  public abstract boolean isOpen();

  public abstract boolean isReadOnly();

  public abstract WatchService newWatchService() throws IOException;

  public abstract FileSystemProvider provider();

  public abstract Set<String> supportedFileAttributeViews();
}
