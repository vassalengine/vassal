package VASSAL.launch;

import java.nio.file.Path;

public class DummyConfigImpl implements Config {
  public String getVersion() {
    return "3.4.3";
  }

  public String getReportableVersion() {
    return "3.4";
  }

  public int getInstanceID() {
    return -1;
  }

  public Path getBaseDir() {
    return getTempDir(); 
  }

  public Path getDocDir() {
    return getTempDir(); 
  }

  public Path getConfDir() {
    return getTempDir(); 
  }

  public Path getTempDir() {
    return Path.of(System.getProperty("java.io.tmpdir"));
  }

  public Path getPrefsDir() {
    return getTempDir(); 
  }

  public Path getErrorLogPath() {
    return getTempDir(); 
  }

  public Path getJavaBinPath() {
    return Path.of(System.getProperty("java.home"), "bin", "java");
  }
}
