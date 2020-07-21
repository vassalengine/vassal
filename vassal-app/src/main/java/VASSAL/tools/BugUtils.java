package VASSAL.tools;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import VASSAL.Info;
import VASSAL.tools.io.IOUtils;

public class BugUtils {

  public static void sendBugReport(String email,
                                   String description,
                                   String errorLog,
                                   Throwable t) throws IOException {
    final HTTPPostBuilder pb = new HTTPPostBuilder();

    final String url = "http://www.vassalengine.org/util/bug.php";
    pb.setParameter("version", Info.getVersion());
    pb.setParameter("email", email);
    pb.setParameter("summary", getSummary(t));
    pb.setParameter("description", description);
    pb.setParameter("log", "errorLog", errorLog);

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

  private static String getSummary(Throwable t) {
    String summary;
    if (t == null) {
      summary = "Automated Bug Report";
    }
    else {
      final String tc = t.getClass().getName();
      summary = tc.substring(tc.lastIndexOf('.') + 1);

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
    final File f = new File(Info.getConfDir(), "errorLog");
    try (FileReader r = new FileReader(f)) {
      log = IOUtils.toString(r);
    }
    catch (IOException e) {
      // Don't bother logging this---if we can't read the errorLog,
      // then we probably can't write to it either.
    }

    return log;
  }
}
