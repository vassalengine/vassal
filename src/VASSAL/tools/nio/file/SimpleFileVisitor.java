package VASSAL.tools.nio.file;

import java.io.IOException;

import VASSAL.tools.io.IOError;
import VASSAL.tools.nio.file.attribute.BasicFileAttributes;

public class SimpleFileVisitor<T> implements FileVisitor<T> {
  public FileVisitResult postVisitDirectory(T dir, IOException e) {
    if (dir == null) throw new NullPointerException();
    if (e != null) throw new IOError(e);
    return FileVisitResult.CONTINUE;
  }

  public FileVisitResult preVisitDirectory(T dir) {
    if (dir == null) throw new NullPointerException();
    return FileVisitResult.CONTINUE;
  }

  public FileVisitResult preVisitDirectoryFailed(T dir, IOException e) {
    if (dir == null) throw new NullPointerException();
    if (e == null) throw new NullPointerException();
    throw new IOError(e);
  }

  public FileVisitResult visitFile(T file, BasicFileAttributes attrs) {
    if (file == null) throw new NullPointerException();
    if (attrs == null) throw new NullPointerException();
    return FileVisitResult.CONTINUE;
  }

  public FileVisitResult visitFileFailed(T file, IOException e) {
    if (file == null) throw new NullPointerException();
    if (e == null) throw new NullPointerException();
    throw new IOError(e);
  }
}
