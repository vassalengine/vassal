package VASSAL.tools.nio.file;

import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.util.Iterator;
import java.util.Set;

import VASSAL.tools.nio.channels.SeekableByteChannel;
import VASSAL.tools.nio.file.attribute.FileAttribute;

public abstract class Path implements FileRef,
                                      Comparable<Path>,
                                      Iterable<Path> {
  protected Path() {}

  public abstract void checkAccess(AccessMode... modes) throws IOException;
  
  public abstract int compareTo(Path owner);

  public abstract Path copyTo(Path target, CopyOption... options)
                                                            throws IOException;
  
  public abstract Path createDirectory(FileAttribute<?>... attrs)
                                                            throws IOException;

  public abstract Path createFile(FileAttribute<?>... attrs)  
                                                            throws IOException;

  public abstract Path createLink(Path existing) throws IOException;

  public abstract Path createSymbolicLink(Path target,
                                          FileAttribute<?>... attrs)
                                                            throws IOException;

  public abstract void delete() throws IOException;
  
  public abstract void deleteIfExists() throws IOException;
  
  public abstract boolean endsWith(Path other);

  public abstract boolean equals(Object o);

  public abstract boolean exists();

  public abstract FileStore getFileStore() throws IOException;

  public abstract FileSystem getFileSystem();

  public abstract Path getName();

  public abstract Path getName(int index);

  public abstract int getNameCount();

  public abstract Path getParent();

  public abstract Path getRoot();

  public abstract int hashCode();

  public abstract boolean isAbsolute();

  public abstract boolean isHidden() throws IOException;

  public abstract boolean isSameFile(Path other) throws IOException;

  public abstract Iterator<Path> iterator();

  public abstract Path moveTo(Path target, CopyOption... options) 
                                                            throws IOException;

  public abstract SeekableByteChannel newByteChannel(OpenOption... options)
                                                            throws IOException;

  public abstract SeekableByteChannel newByteChannel(
    Set<? extends OpenOption> options, FileAttribute<?>... attrs)
                                                            throws IOException;

  public abstract DirectoryStream<Path> newDirectoryStream() throws IOException;

  public abstract DirectoryStream<Path> newDirectoryStream(
    DirectoryStream.Filter<? super Path> filter) throws IOException;

  public abstract DirectoryStream<Path> newDirectoryStream(String glob)
                                                            throws IOException;

  public abstract Path normalize();

  public abstract boolean notExists();

  public abstract Path readSymbolicLink() throws IOException;

  public abstract WatchKey register(WatchService watcher,
                                    WatchEvent.Kind<?>... events)
                                                            throws IOException;

  public abstract WatchKey register(WatchService watcher,
                                    WatchEvent.Kind<?>[] events,
                                    WatchEvent.Modifier... modifiers)
                                                            throws IOException;

  public abstract Path relativize(Path other);

  public abstract Path resolve(Path other);

  public abstract Path resolve(String other);

  public abstract boolean startsWith(Path other);

  public abstract Path subpath(int beginIndex, int endIndex);

  public abstract Path toAbsolutePath();
  
  public abstract Path toRealPath(boolean resolveLinks) throws IOException;
  
  public abstract String toString();

  public abstract URI toUri();
}
