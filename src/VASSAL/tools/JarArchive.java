package VASSAL.tools;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;


public class JarArchive extends DataArchive {

  protected String prefix;

  public JarArchive() {
    this(null);
  }
  
  public JarArchive(String prefix) {
    super();
    this.prefix = prefix;
  }

  public URL getURL(String fileName) throws IOException {
    URL url = getClass().getResource(getAbsolutePath(fileName));
    if (url == null) {
      url = getURLFromExtension(fileName);
    }
    return url;
  }

  public InputStream getFileStream(String file) throws IOException {
    InputStream in = getClass().getResourceAsStream(getAbsolutePath(file));
  
    if (in == null) {
      in = getFileStreamFromExtension(file);
    }
    return in;
  }

  protected String getAbsolutePath(String file) {
    String resource = prefix != null ? "/" + prefix + "/" + file : "/" + file;
    return resource;
  }

  public String getName() {
    return prefix != null ? prefix : super.getName();
  }
}
