/*
 * Based on http://burtbeckwith.com/blog/?p=52
 */

package VASSAL.test;

import java.io.File;
import java.io.UnsupportedEncodingException;

import java.net.URLDecoder;
import java.util.logging.Level;

import org.junit.platform.launcher.Launcher;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder;
import org.junit.platform.launcher.core.LauncherFactory;
import org.junit.platform.launcher.listeners.LoggingListener;
import org.junit.platform.launcher.listeners.SummaryGeneratingListener;
import org.junit.platform.launcher.listeners.TestExecutionSummary;

import static org.junit.platform.engine.discovery.DiscoverySelectors.selectDirectory;
import static org.junit.platform.engine.discovery.DiscoverySelectors.selectPackage;

/**
 * Discovers all JUnit tests and runs them.
 */
public final class AllTests {

  /**
   * Runs all tests
   */
  public static void main(String[] args) {
    LauncherDiscoveryRequest request = LauncherDiscoveryRequestBuilder.request()
        .selectors(
            selectDirectory(findClassesDir()),
            selectPackage("")
        )
        .build();

    Launcher launcher = LauncherFactory.create();

    SummaryGeneratingListener listener = new SummaryGeneratingListener();
    launcher.registerTestExecutionListeners(listener);
    launcher.registerTestExecutionListeners(LoggingListener.forJavaUtilLogging(Level.ALL));

    launcher.execute(request);

    TestExecutionSummary summary = listener.getSummary();
  }

  private static File findClassesDir() {
    try {
      String path = AllTests.class.getProtectionDomain()
          .getCodeSource().getLocation().getFile();
      return new File(URLDecoder.decode(path, "UTF-8"));
    } catch (UnsupportedEncodingException e) {
      throw new AssertionError(e);
    }
  }

}
