package VASSAL.build.module;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.tools.BugUtils;
import VASSAL.tools.SequenceEncoder;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class Console {
  SequenceEncoder.Decoder decode;
  String commandLine;

  private static final org.slf4j.Logger log =
    LoggerFactory.getLogger(Console.class);

  private void show (String s) {
    GameModule.getGameModule().warn(s);
  }


  private Boolean matches (String s1, String s2) {
    return matches(s1, s2, 2);
  }

  private Boolean matches (String s1, String s2, int min) {
    if (s2.isEmpty() || (s2.length() > s1.length()) || (s2.length() < min)) {
      return false;
    }
    return s1.toLowerCase().substring(0, s2.length()).equals(s2.toLowerCase());
  }


  public Boolean doErrorLog () {
    String option = decode.nextToken("");
    if (matches("show", option)) {
      String errorLog = BugUtils.getErrorLog();
      String delims2 = "[\n]+";
      String[] lines = errorLog.split(delims2);

      if (lines.length > 0) {
        int end         = lines.length - 1;
        int linesToShow = decode.nextInt(0);
        int start       = (linesToShow <= 0) ? 0 : Math.max(0, end - linesToShow + 1);

        for (int line = start; line <= end; line++) {
          show(lines[line]);
        }
      }
    }
    else if (matches("write", option)) {
      int where = commandLine.toLowerCase().indexOf("write");
      if ((where > 0) && commandLine.length() > where + 6) {
        log.info(commandLine.substring(where + 6));
      }
    }
    else if (matches("wipe", option)) {
      final File errorLog = Info.getErrorLogPath();
      try {
        new FileOutputStream(errorLog).close();
        show("Wiped errorlog");
      }
      catch (IOException e) {
        show("Failed to wipe errorlog");
      }
    }
    else if (matches("?", option) || matches("help", option)) {
      show("Usage:");
      show("  /errorlog show [n]     - Show last n lines of errorlog");
      show("  /errorlog wipe         - Wipe the errorlog file");
      show("  /errorlog write [text] - Write text into the errorlog file");
    }

    return true;
  }


  public Boolean exec(String s, String style, boolean html_allowed) {
    if (s.charAt(0) != '/') {
      return false;
    }
    commandLine = s.substring(1);

    show(s);

    // First get rid of any extra spaces between things
    String delims = "[ ]+";
    String[] tokens = commandLine.split(delims);
    StringBuilder sb = new StringBuilder("");
    Boolean first = true;
    for (String t : tokens) {
      if (first) {
        first = false;
      }
      else {
        sb.append(" ");
      }
      sb.append(t);
    }
    String cl = sb.toString(); // Our command line with things separated by no more than one space

    decode = new SequenceEncoder.Decoder(cl, ' ');

    String command = decode.nextToken("");

    if (matches("errorlog", command)) {
      return doErrorLog();
    }

    return false;
  }
}
