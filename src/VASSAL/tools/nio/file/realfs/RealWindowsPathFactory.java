package VASSAL.tools.nio.file.realfs;

class RealWindowsPathFactory implements RealPathFactory {
  public RealPath getPath(String path, RealFileSystem fs) {
    return new RealWindowsPath(path, fs);
  }
}
