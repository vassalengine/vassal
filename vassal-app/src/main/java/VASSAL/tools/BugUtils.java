package VASSAL.tools;

import VASSAL.build.GameModule;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

import VASSAL.Info;
import VASSAL.tools.io.IOUtils;
import java.util.stream.Stream;

public class BugUtils {

  public static void sendBugReport(String email,
                                   String description,
                                   String errorLog,
                                   Throwable t) throws IOException {
    final HTTPPostBuilder pb = new HTTPPostBuilder();

    final String url = "http://www.vassalengine.org/util/bug.php";
    pb.setParameter("version", Info.getReportableVersion());
    pb.setParameter("email", email);
    pb.setParameter("summary", getSummary(t));
    pb.setParameter("description", getDescription(description, errorLog));
    pb.setParameter("log", Info.getErrorLogPath().getName(), errorLog);

    try (InputStream in = pb.post(url)) {
      final String result = IOUtils.toString(in, StandardCharsets.UTF_8);

      // script should return zero on success, otherwise it failed
      try {
        if (Integer.parseInt(result) != 0) {
          throw new NumberFormatException("Bad result: " + result);
        }
      }
      catch (NumberFormatException e) {
        throw new IOException(e);
      }
    }
  }

  private static String getDescription(String description, String errorLog) {
    return
      description + "\n\n" +
      GameModule.getGameModule().getGameName() + " v" + GameModule.getGameModule().getGameVersion() + " " + Info.getVersion() + "\n\n" +
      getStackTraceSummary(errorLog);
  }

  private static String getStackTraceSummary(String errorLog) {
    final StringBuilder summary = new StringBuilder();
    final Stream<String> log = errorLog.substring(errorLog.lastIndexOf("ERROR VASSAL.tools.ErrorDialog")).lines();
    log.skip(1).limit(5).forEach(l -> summary.append(l.replace('\t', ' ')).append('\n'));
    return summary.toString();
  }

  private static String getSummary(Throwable t) {
    String summary = "[" + GameModule.getGameModule().getGameName() + "] ";
    if (t == null) {
      summary += "Automated Bug Report";
    }
    else {
      final String tc = t.getClass().getName();
      summary += tc.substring(tc.lastIndexOf('.') + 1);

      if (t.getMessage() != null) {
        summary += ": " + t.getMessage();
      }
/*
      for (StackTraceElement e : t.getStackTrace()) {
        if (e.getClassName().startsWith("VASSAL")) {
          summary += " at "+e.getClassName()+"."+e.getMethodName()+" (line "+e.getLineNumber()+")";
          break;
        }
      }
*/
    }
    return summary;
  }


// FIXME: move this somewhere else?
  public static String getErrorLog() {
    String log = null;
    final File f = Info.getErrorLogPath();
    try (FileReader r = new FileReader(f, Charset.defaultCharset())) {
      log = IOUtils.toString(r);
    }
    catch (IOException e) {
      // Don't bother logging this---if we can't read the errorLog,
      // then we probably can't write to it either.
    }

    return log;
  }
}
