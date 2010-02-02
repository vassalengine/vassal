package VASSAL.tools.nio.file.realfs;

interface RealPathFactory {
  public RealPath getPath(String path, RealFileSystem fs);
}
