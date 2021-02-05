package VASSAL.tools;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;

import VASSAL.Info;
import VASSAL.build.GameModule;

public class BugUtils {

  public static void sendBugReport(String email,
                                   String description,
                                   String errorLog,
                                   Throwable t) throws IOException {
    final HTTPPostBuilder pb = new HTTPPostBuilder();

    final String url = "http://www.vassalengine.org/util/bug.php"; //NON-NLS
    pb.setParameter("version", Info.getReportableVersion()); //NON-NLS
    pb.setParameter("email", email); //NON-NLS
    pb.setParameter("summary", getSummary(t)); //NON-NLS
    pb.setParameter("description", getDescription(description, errorLog)); //NON-NLS
    pb.setParameter("log", Info.getErrorLogPath().getName(), errorLog); //NON-NLS

    try (InputStream in = pb.post(url)) {
      final String result = IOUtils.toString(in, StandardCharsets.UTF_8);

      // script should return zero on success, otherwise it failed
      try {
        if (Integer.parseInt(result) != 0) {
          throw new NumberFormatException("Bad result: " + result); //NON-NLS
        }
      }
      catch (NumberFormatException e) {
        throw new IOException(e);
      }
    }
  }

  private static String getDescription(String description, String errorLog) {
    final GameModule g = GameModule.getGameModule();
    return
      description + "\n\n" +
      (g == null ? "" : g.getGameName() + " v" + g.getGameModule().getGameVersion() + " ") +
      Info.getVersion() + "\n\n" + //NON-NLS
      getStackTraceSummary(errorLog);
  }

  private static String getStackTraceSummary(String errorLog) {
    final StringBuilder summary = new StringBuilder();
    final Stream<String> log = errorLog.substring(errorLog.lastIndexOf("ERROR VASSAL.tools.ErrorDialog")).lines(); //NON-NLS
    log.skip(1).limit(5).forEach(l -> summary.append(l.replace('\t', ' ')).append('\n'));
    return summary.toString();
  }

  private static String getSummary(Throwable t) {
    final GameModule g = GameModule.getGameModule();
    String summary = g == null ? "" : "[" + g.getGameName() + "] ";
    if (t == null) {
      summary += "Automated Bug Report"; //NON-NLS
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
    final File f = Info.getErrorLogPath();
    try {
      return Files.readString(f.toPath(),  Charset.defaultCharset());
    }
    catch (IOException e) {
      // Don't bother logging this---if we can't read the errorLog,
      // then we probably can't write to it either.
      return null;
    }
  }
}
