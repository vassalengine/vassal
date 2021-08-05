package VASSAL.tools;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.stream.Stream;

import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.entity.mime.MultipartEntityBuilder;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.CloseableHttpResponse;
import org.apache.hc.client5.http.impl.classic.HttpClients;
import org.apache.hc.core5.http.ContentType;
import org.apache.hc.core5.http.ParseException;
import org.apache.hc.core5.http.io.entity.EntityUtils;

import VASSAL.Info;
import VASSAL.build.GameModule;

public class BugUtils {
  private BugUtils() {}

  public static void sendBugReport(String email,
                                   String description,
                                   String errorLog,
                                   Throwable t) throws IOException {
    // build the POST body
    final MultipartEntityBuilder b = MultipartEntityBuilder.create();
    b.setCharset(StandardCharsets.UTF_8);

    b.addTextBody("version", Info.getReportableVersion()); //NON-NLS
    b.addTextBody("email", email); //NON-NLS
    b.addTextBody("summary", getSummary(t)); //NON-NLS
    b.addTextBody("description", getDescription(description, errorLog)); //NON-NLS
    b.addBinaryBody("log", errorLog.getBytes(StandardCharsets.UTF_8), ContentType.TEXT_PLAIN, Info.getErrorLogPath().getName()); //NON-NLS

    final String url = "http://www.vassalengine.org/util/abr";
    final HttpPost httpPost = new HttpPost(url);
    httpPost.setEntity(b.build());

    // send the POST
    try (CloseableHttpClient client = HttpClients.createDefault()) {
      try (CloseableHttpResponse response = client.execute(httpPost)) {
        if (response.getCode() != 201) {
          final String msg = "Bug report failed: " + response.getCode();

          String responseText = null;
          try {
            responseText = EntityUtils.toString(response.getEntity());
          }
          catch (ParseException e) {
            throw new IOException(msg, e);
          }

          throw new IOException(msg + ": " + responseText);
        }
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
    }
    return summary;
  }

// FIXME: move this somewhere else?
  public static String getErrorLog() {
    final File f = Info.getErrorLogPath();
    try {
      return Files.readString(f.toPath(),  StandardCharsets.UTF_8);
    }
    catch (IOException e) {
      // Don't bother logging this---if we can't read the errorLog,
      // then we probably can't write to it either.
      return null;
    }
  }
}
