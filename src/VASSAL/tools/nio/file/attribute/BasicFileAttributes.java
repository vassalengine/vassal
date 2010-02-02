package VASSAL.tools.nio.file.attribute;

public interface BasicFileAttributes {
  public FileTime creationTime();

  public Object fileKey();

  public boolean isDirectory();

  public boolean isOther();

  public boolean isRegularFile();

  public boolean isSymbolicLink();

  public FileTime lastAccessTime();
  
  public FileTime lastModifiedTime();

  public long size();
}
