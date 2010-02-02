package VASSAL.tools.nio.file;

import java.io.IOException;

import VASSAL.tools.nio.file.attribute.BasicFileAttributes;

public interface FileVisitor<T> {
  public FileVisitResult postVisitDirectory(T dir, IOException exc);

  public FileVisitResult preVisitDirectory(T dir);

  public FileVisitResult preVisitDirectoryFailed(T dir, IOException exc);

  public FileVisitResult visitFile(T file, BasicFileAttributes attrs);

  public FileVisitResult visitFileFailed(T file, IOException exc);
}
