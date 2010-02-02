package VASSAL.tools.nio.file.attribute;

public interface FileStoreSpaceAttributes {
  public long totalSpace();

  public long unallocatedSpace();

  public long usableSpace(); 
}
