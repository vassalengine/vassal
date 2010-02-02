package VASSAL.tools.nio.file.attribute;

import java.io.IOException;

public interface BasicFileAttributeView extends FileAttributeView {
  public BasicFileAttributes readAttributes() throws IOException;

  public void setTimes(FileTime mtime, FileTime atime, FileTime ctime)
                                                            throws IOException;
}
