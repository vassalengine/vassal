package depreport;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class DepreportFlattener {

  private final List<String> content;
  private final OutputAggregator outputAggregator = new OutputAggregator();

  public DepreportFlattener(String depreportFile) throws IOException {
    content = Files.readAllLines(Path.of(depreportFile));
    flattenContent();
  }

  private void flattenContent() {
    String packageName = null;
    String className = null;
    for (String line : content) {
      final boolean isPackage = line.matches("^\\S.*$");
      if (isPackage) {
        packageName = stripStarSuffix(line);
        continue;
      }

      final boolean isClassName = line.matches("^[ ]{4}\\S.*$");
      if (isClassName) {
        className = stripStarSuffix(line);
        continue;
      }

      final boolean isClassDependency = line.matches("^[ ]{8}<.*$");
      if (isClassDependency) {
        outputAggregator.addLine(String.format("%s.%s", packageName, className));
        continue;
      }

      final boolean isClassFeature = line.matches("^[ ]{8}\\S.*$");
      if (isClassFeature) {
        outputAggregator.addLine(String.format("%s.%s.%s", packageName, className, stripStarSuffix(line)));
      }
    }
  }

  private String stripStarSuffix(String stringWithStarSuffix) {
    return stringWithStarSuffix.replace('*', ' ').strip();
  }

  public List<String> getFlatReport() {
    return outputAggregator.getOutput();
  }

  private static class OutputAggregator {
    private final List<String> output = new ArrayList<>();
    private String lastLine;

    public void addLine(String line) {
      if (!line.equals(lastLine)) {
        output.add(line);
        lastLine = line;
      }
    }

    public List<String> getOutput() {
      return output;
    }
  }
}
