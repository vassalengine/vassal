package VASSAL.tools.nio.file.attribute;

import java.io.IOException;

public interface FileStoreSpaceAttributeView extends FileStoreAttributeView {
  public FileStoreSpaceAttributes readAttributes() throws IOException;
}
