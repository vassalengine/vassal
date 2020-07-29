package depreport;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class UnusedDependenciesWriter {

  private static final String HEADER = "# Generated file, do not change";

  private final List<String> deprecated;
  private final List<String> flatDepreport;
  private List<String> unusedDependencies;

  public UnusedDependenciesWriter(String deprecatedFile, List<String> flatDepreport) throws IOException {
    this.deprecated = Files.readAllLines(Path.of(deprecatedFile));
    this.flatDepreport = flatDepreport;
    determineUnusedDependencies();
  }

  private void determineUnusedDependencies() {
    Collections.sort(deprecated);
    Collections.sort(flatDepreport);
    unusedDependencies = new ArrayList<>(deprecated);
    unusedDependencies.removeAll(flatDepreport);
  }

  public void writeTo(String outputFile) throws IOException {
    unusedDependencies.add(0, HEADER);
    Files.write(
      Path.of(outputFile),
      unusedDependencies,
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE,
      StandardOpenOption.TRUNCATE_EXISTING);
  }
}
