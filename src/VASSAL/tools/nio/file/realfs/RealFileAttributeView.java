package VASSAL.tools.nio.file.realfs;

import java.io.IOException;

import VASSAL.tools.nio.file.FileSystemException;
import VASSAL.tools.nio.file.attribute.FileTime;
import VASSAL.tools.nio.file.attribute.BasicFileAttributeView;

public class RealFileAttributeView implements BasicFileAttributeView {
  protected final RealPath path;

  public RealFileAttributeView(RealPath path) {
    this.path = path;
  }
  
  public String name() {
    return "basic";
  }

  public RealFileAttributes readAttributes() throws IOException {
    return new RealFileAttributes(path);
  }

  public void setTimes(FileTime mtime, FileTime atime, FileTime ctime)
                                                           throws IOException {
    if (mtime != null && !path.file.setLastModified(mtime.toMillis())) {
      throw new FileSystemException(path.toString());
    }

    if (atime != null) throw new UnsupportedOperationException();
    if (ctime != null) throw new UnsupportedOperationException();
  }
}
