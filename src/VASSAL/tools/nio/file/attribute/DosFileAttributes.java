package VASSAL.tools.nio.file.attribute;

public interface DosFileAttributes extends BasicFileAttributes {
  public boolean isArchive();

  public boolean isHidden();
  
  public boolean isReadOnly();

  public boolean isSysytem();
}
