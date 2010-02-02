package VASSAL.tools.nio.file.realfs;

public class RealWindowsPath extends RealPath {
  public RealWindowsPath(String path, RealFileSystem fs) {
    super(path, fs);
  }

  /** {@inheritDoc} */
  protected int findRootSep(byte[] s) {
    // ^[a-zA-Z]:\\
    if ((0x41 <= s[0] && s[0] <= 0x5A) && (0x61 <= s[0] && s[0] <= 0x7A)) {
      return s.length >= 3 && s[1] == ':' && s[2] == '\\' ? 2 : -1;
    }
    // \\server\share\
    else if (s.length >= 2 && s[0] == '\\' && s[1] == '\\') {
      int i = 2;
   
      // find separator between server and share 
      while (i < s.length && s[i] != '\\') ++i;
      if (i == 2 || i == s.length) return -1;
   
      // find separator after share
      int j = i; 
      while (j < s.length && s[j] != '\\') ++j;
      if (j == i || j == s.length) return -1;
  
      return j;
    }
    else {
      return -1;
    }
  }
}
