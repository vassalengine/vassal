package VASSAL.tools.nio.file.realfs;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import VASSAL.Info;
import VASSAL.tools.nio.file.FileStore;
import VASSAL.tools.nio.file.Path;
import VASSAL.tools.nio.file.spi.FileSystemProvider;

public class RealFileSystem extends AbstractFileSystem {
  protected final RealPathFactory pfactory;
  protected final RealFileStore store;
  protected final RealFileSystemProvider provider;

  public RealFileSystem(RealFileSystemProvider provider) {
    this.provider = provider;
    pfactory = Info.isWindows() ?
      new RealWindowsPathFactory() : new RealUnixPathFactory();
    store = new RealFileStore(this);
  }

  public void close() throws IOException {
    throw new UnsupportedOperationException();
  }

  public Path getPath(String path) {
    return pfactory.getPath(path, this);
  }

  public Iterable<FileStore> getFileStores() {
    return Collections.<FileStore>singletonList(store);
  }

  public Iterable<Path> getRootDirectories() {
    final File[] roots = File.listRoots();
    final List<Path> r = new ArrayList<Path>(roots.length);
    for (File rf : roots) r.add(getPath(rf.toString()));
    return r;
  }

  public String getSeparator() {
    return File.separator;
  }

  public boolean isOpen() {
    return true;
  }

  public boolean isReadOnly() {
    return false;
  }

  public FileSystemProvider provider() {
    return provider;
  }

  public Set<String> supportedFileAttributeViews() {
    return Collections.singleton("basic");
  }
}
