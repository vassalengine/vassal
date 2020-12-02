package VASSAL.launch;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.Info;

public class CustomVmOptions {

  private static final Logger log = LoggerFactory.getLogger(CustomVmOptions.class);

  private static final String VM_OPTIONS_FILE_NAME = "vassal.vmoptions"; //NON-NLS

  public void ensureCustomVmOptionsFileExistsInConfDir() {
    final File confDir = Info.getConfDir();
    final File vmOptionsFile = new File(confDir, VM_OPTIONS_FILE_NAME);
    if (!vmOptionsFile.exists()) {
      createInitialVmOptionsFile(confDir);
    }
  }

  private void createInitialVmOptionsFile(File confDir) {
    final ClassLoader classLoader = getClass().getClassLoader();
    try (InputStream is = classLoader.getResourceAsStream(VM_OPTIONS_FILE_NAME)) {
      if (is == null) {
        log.error("Template for custom VM options " + VM_OPTIONS_FILE_NAME + " not found in the Vassal distribution"); //NON-NLS
        return;
      }
      Files.copy(is, Path.of(confDir.getAbsolutePath(), VM_OPTIONS_FILE_NAME));
    }
    catch (IOException e) {
      log.error("Unable to copy " + VM_OPTIONS_FILE_NAME + " to " + confDir.getAbsolutePath(), e); //NON-NLS
    }
  }

  public List<String> getCustomVmOptions() {
    try {
      final List<String> allLines =
        Files.readAllLines(Path.of(Info.getConfDir().getAbsolutePath(), VM_OPTIONS_FILE_NAME));

      return allLines
        .stream()
        .filter(s -> !s.startsWith("#"))
        .filter(StringUtils::isNotBlank)
        .map(String::trim)
        .collect(Collectors.toList());
    }
    catch (IOException e) {
      log.error("Unable to read " + VM_OPTIONS_FILE_NAME, e); //NON-NLS
      return Collections.emptyList();
    }
  }
}
