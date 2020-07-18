package depreport;

import java.io.IOException;
import java.util.List;

import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.BlockJUnit4ClassRunner;

@RunWith(BlockJUnit4ClassRunner.class)
public class DepreportRunner {

  private static final String DEPRECATED = "src/test/resources/depreport/deprecated.txt";
  private static final String DEPREPORT = "src/test/resources/depreport/depreport.txt";
  private static final String UNUSED_DEPENDENCIES = "src/test/resources/depreport/unused-dependencies.txt";

  /**
   * Meant to be used manually to rewrite the list of unused dependencies based on the list of deprecated class features
   * and the dependency report.
   *
   * Do not commit without active @Ignore annotation.
   */
  @Ignore
  @Test
  public void rewriteUnusedDependenciesFile() throws IOException {
    List<String> flatDepReport = new DepreportFlattener(DEPREPORT).getFlatReport();
    new UnusedDependenciesWriter(DEPRECATED, flatDepReport).writeTo(UNUSED_DEPENDENCIES);
  }
}
