package VASSAL.tools.nio.file.realfs;

import VASSAL.tools.nio.file.attribute.FileStoreSpaceAttributes;

class DummyFileStoreAttributes implements FileStoreSpaceAttributes {
  public long totalSpace() {
    return 0L;
  }

  public long unallocatedSpace() {
    return 0L;
  }

  public long usableSpace() {
    return 0L;
  } 
}
