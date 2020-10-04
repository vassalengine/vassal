package VASSAL.launch;

import java.nio.file.Path;

public interface Config {
  String getVersion();

  String getReportableVersion();

  int getInstanceID();

  Path getBaseDir();

  Path getDocDir();

  Path getConfDir();

  Path getTempDir();

  Path getPrefsDir();

  Path getErrorLogPath();

  Path getJavaBinPath();
}
