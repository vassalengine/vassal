package VASSAL.tools.nio.file.realfs;

import java.util.concurrent.TimeUnit;

import VASSAL.tools.nio.file.attribute.BasicFileAttributes;
import VASSAL.tools.nio.file.attribute.FileTime;

class RealFileAttributes implements BasicFileAttributes {
  protected final RealPath path;

  public RealFileAttributes(RealPath path) {
    this.path = path;
  } 

  public FileTime creationTime() {
    return null;
  }

  public Object fileKey() {
    return null; 
  }

  public boolean isDirectory() {
    return path.file.isDirectory();
  }

  public boolean isOther() {
    return !isRegularFile() && !isDirectory() && !isSymbolicLink();
  }

  public boolean isRegularFile() {
    return path.file.isFile();
  }

  public boolean isSymbolicLink() {
    return false;
  }

  public FileTime lastAccessTime() {
    return null;
  }
  
  public FileTime lastModifiedTime() {
    return FileTime.from(path.file.lastModified(), TimeUnit.MILLISECONDS);
  }

  public long size() {
    return path.file.length();
  }
}
