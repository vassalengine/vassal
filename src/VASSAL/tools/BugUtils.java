package VASSAL.tools;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;

import VASSAL.Info;
import VASSAL.tools.io.IOUtils;

public class BugUtils {

  public static void sendBugReport(String email,
                                   String description,
                                   String errorLog,
                                   Throwable t) throws IOException {
    final HTTPPostBuilder pb = new HTTPPostBuilder();

    InputStream in = null;
    try {
/*
      final URL url = new URL("http://sourceforge.net/tracker/index.php");
      pb.setParameter("group_id", "90612");
      pb.setParameter("atid", "594231");
      pb.setParameter("func", "postadd");
      pb.setParameter("category_id", "100");
      pb.setParameter("artifact_group_id", "100");
      pb.setParameter("summary", getSummary(t));
      pb.setParameter("details", email + "\n\n" + description);
      pb.setParameter("input_file", "errorLog", errorLog);
      pb.setParameter("file_description", "the errorLog");
      pb.setParameter("submit", "SUBMIT");
*/
      final String url = "http://www.vassalengine.org/util/bug.php";
      pb.setParameter("version", Info.getVersion());
      pb.setParameter("email", email);
      pb.setParameter("summary", getSummary(t));
      pb.setParameter("description", description);
      pb.setParameter("log", "errorLog", errorLog);

      in = pb.post(url);
      final String result = IOUtils.toString(in);

      // script should return zero on success, otherwise it failed
      try {
        if (Integer.parseInt(result) != 0) {
          throw new NumberFormatException("Bad result: " + result);
        }
      }
      catch (NumberFormatException e) {
        throw (IOException) new IOException().initCause(e);
      }

      in.close();
    }
    finally {
      IOUtils.closeQuietly(in);
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
    FileReader r = null;
    try {
      r = new FileReader(new File(Info.getConfDir(), "errorLog"));
      log = IOUtils.toString(r);
      r.close();
    }
    catch (IOException e) {
      // Don't bother logging this---if we can't read the errorLog,
      // then we probably can't write to it either.
      IOUtils.closeQuietly(r);
    }

    return log;
  }
}
