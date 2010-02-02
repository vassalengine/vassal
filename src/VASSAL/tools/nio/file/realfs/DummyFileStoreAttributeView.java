package VASSAL.tools.nio.file.realfs;

import java.io.IOException;

import VASSAL.tools.nio.file.attribute.FileStoreSpaceAttributes;
import VASSAL.tools.nio.file.attribute.FileStoreSpaceAttributeView;

class DummyFileStoreAttributeView implements FileStoreSpaceAttributeView {
  public String name() {
    return "space";
  }

  public FileStoreSpaceAttributes readAttributes() throws IOException {
    return new DummyFileStoreAttributes();
  }
}
