package VASSAL.build.module;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.tools.BugUtils;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class Console {

  private static final org.slf4j.Logger log =
    LoggerFactory.getLogger(Console.class);

  private static void show (String s) {
    GameModule.getGameModule().warn(s);
  }

  public static boolean exec(String s, String style, boolean html_allowed) {
    String cl = s.toLowerCase().trim();
    if (cl.charAt(0) != '/') {
      return false;
    }

    show(s);

    cl = cl.substring(1);
    String delims = "[ ]+";
    String[] tokens = cl.split(delims);

    if ("errorlog".equals(tokens[0])) {
      if (tokens.length > 1) {
        if ("show".equals(tokens[1])) {
          String errorLog = BugUtils.getErrorLog();
          String delims2 = "[\n]+";
          String[] lines = errorLog.split(delims2);

          if (lines.length > 0) {
            int start = 0;
            int end = lines.length - 1;
            if (tokens.length > 2) {
              try {
                start = Math.max(0, end - Integer.valueOf(tokens[2]) + 1);
              }
              catch (NumberFormatException e) {
                //
              }
            }

            for (int line = start; line <= end; line++) {
              show(lines[line]);
            }
          }
        }
        else if ("write".equals(tokens[1])) {
          int where = s.toLowerCase().indexOf("write");
          if (where > 0) {
            String writeString = s.substring(where + 6);
            log.info(writeString);
          }
        }
        else if ("wipe".equals(tokens[1])) {
          // truncate the errorLog
          final File errorLog = Info.getErrorLogPath();
          try {
            new FileOutputStream(errorLog).close();
            show("Wiped errorlog");
          }
          catch (IOException e) {
            show("Failed to wipe errorlog");
          }
        }
      }
    }

    return true;
  }
}
