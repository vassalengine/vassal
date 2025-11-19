package VASSAL.build.module.map;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.BasicLogger;
import VASSAL.build.module.Map;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.build.module.map.Zoomer;
import VASSAL.command.Command;
import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/**
 * Prototype toolbar item that renders a vlog replay directly into an ffmpeg process.
 */
public class VideoExporter extends AbstractToolbarItem {
  private static final String DEFAULT_ICON = "/images/camera.gif";

  private Map map;

  public VideoExporter() {
    setNameKey("");
    setButtonTextKey("VideoExporter.button");
    setLaunchButton(makeLaunchButton(
      Resources.getString("VideoExporter.button"),
      "",
      DEFAULT_ICON,
      e -> SwingUtilities.invokeLater(this::startExport)
    ));
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public void addTo(Buildable parent) {
    map = (Map) parent;
    map.getToolBar().add(getLaunchButton());
  }

  @Override
  public void removeFrom(Buildable parent) {
    map.getToolBar().remove(getLaunchButton());
    map.getToolBar().revalidate();
  }

  public void startExport() {
    final GameModule gm = GameModule.getGameModule();
    final double originalZoom = maximizeZoom(map);
    final JFileChooser dirChooser = new JFileChooser();
    dirChooser.setDialogTitle(Resources.getString("VideoExporter.folder_dialog"));
    dirChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    if (dirChooser.showOpenDialog(map.getView()) != JFileChooser.APPROVE_OPTION) {
      return;
    }
    final File directory = dirChooser.getSelectedFile();
    if (directory == null || !directory.isDirectory()) {
      gm.warn(Resources.getString("VideoExporter.missing_log"));
      return;
    }

    final File[] logFiles = directory.listFiles((dir, name) -> name.toLowerCase().endsWith(".vlog"));
    if (logFiles == null || logFiles.length == 0) {
      gm.warn(Resources.getString("VideoExporter.no_logs"));
      return;
    }
    Arrays.sort(logFiles, Comparator.comparing(File::getName));
    final List<File> orderedLogs = Arrays.asList(logFiles);

    final JFileChooser videoChooser = new JFileChooser(directory);
    videoChooser.setDialogTitle(Resources.getString("VideoExporter.output_dialog"));
    videoChooser.setSelectedFile(new File(directory,
      directory.getName() + ".mp4"));
    if (videoChooser.showSaveDialog(map.getView()) != JFileChooser.APPROVE_OPTION) {
      return;
    }
    File videoFile = videoChooser.getSelectedFile();
    if (videoFile == null) {
      return;
    }
    if (!videoFile.getName().toLowerCase().endsWith(".mp4")) {
      videoFile = new File(videoFile.getParentFile(), videoFile.getName() + ".mp4");
    }

    final int fps = 5;
    final File finalVideo = videoFile;
    final double restoreZoom = originalZoom;
    new Thread(() -> {
      try {
        renderLogs(orderedLogs, finalVideo, fps);
      }
      finally {
        SwingUtilities.invokeLater(() -> restoreZoom(map, restoreZoom));
      }
    }, "VideoExporter").start();
  }

  private void renderLogs(List<File> logFiles, File videoFile, int fps) {
    final GameModule gm = GameModule.getGameModule();
    final BasicLogger logger = gm.getBasicLogger();
    final GameState gameState = gm.getGameState();
    final boolean wasSavePromptSuppressed = gameState.isSavePromptSuppressed();
    gameState.setSuppressSavePrompt(true);

    try {
      logger.setSuppressReplayPrompts(true);
      BufferedImage frame = null;
      Rectangle drawingRect = null;
      FfmpegWriter writer = null;
      long captureNanos = 0;
      long writeNanos = 0;
      long stepNanos = 0;
      long loadNanos = 0;
      long totalFrames = 0;
      long totalCommands = 0;
      try {
        for (final File logFile : logFiles) {
          final boolean[] loaded = new boolean[1];
          final long loadStart = System.nanoTime();
          SwingUtilities.invokeAndWait(() -> loaded[0] = gameState.loadGame(logFile, false, true));
          loadNanos += System.nanoTime() - loadStart;
          if (!loaded[0]) {
            gm.warn(Resources.getString("VideoExporter.load_failed"));
            continue;
          }

          if (writer == null) {
            final Rectangle[] captureRectHolder = new Rectangle[1];
            final int[] dimensions = new int[2];
            SwingUtilities.invokeAndWait(() -> {
              final Rectangle mapRect = new Rectangle(0, 0, map.mapSize().width, map.mapSize().height);
              final Rectangle drawing = map.mapToDrawing(mapRect, 1.0);
              captureRectHolder[0] = drawing;
              dimensions[0] = Math.max(1, drawing.width);
              dimensions[1] = Math.max(1, drawing.height);
            });

            if (captureRectHolder[0].width <= 0 || captureRectHolder[0].height <= 0) {
              gm.warn(Resources.getString("VideoExporter.no_view"));
              return;
            }

            drawingRect = new Rectangle(captureRectHolder[0]);
            final int width = dimensions[0];
            final int height = dimensions[1];
            final int videoWidth = (width % 2 == 0) ? width : width + 1;
            final int videoHeight = (height % 2 == 0) ? height : height + 1;
            frame = new BufferedImage(videoWidth, videoHeight, BufferedImage.TYPE_3BYTE_BGR);
            writer = new FfmpegWriter(videoWidth, videoHeight, fps, videoFile);
          }

          final long captureStart = System.nanoTime();
          captureFrame(frame, drawingRect);
          captureNanos += System.nanoTime() - captureStart;
          final long writeStart = System.nanoTime();
          writer.writeFrame(frame);
          writeNanos += System.nanoTime() - writeStart;
          totalFrames++;
          gm.warn(String.format("VideoExporter frame %d captured (load %.1f ms, capture %.1f ms, write %.1f ms)",
            totalFrames, loadNanos / 1_000_000.0, captureNanos / 1_000_000.0, writeNanos / 1_000_000.0));

          while (true) {
            final Command nextCommand = logger.peekNextCommand();
            final boolean captureAfterStep = commandLikelyChangesMap(nextCommand);
            final boolean[] stepped = new boolean[1];
            final long stepStart = System.nanoTime();
            SwingUtilities.invokeAndWait(() -> stepped[0] = logger.stepForward());
            stepNanos += System.nanoTime() - stepStart;
            if (!stepped[0]) {
              break;
            }
            if (captureAfterStep) {
              final long captureStartStep = System.nanoTime();
              captureFrame(frame, drawingRect);
              captureNanos += System.nanoTime() - captureStartStep;
              final long writeStartStep = System.nanoTime();
              writer.writeFrame(frame);
              writeNanos += System.nanoTime() - writeStartStep;
              totalFrames++;
              gm.warn(String.format("VideoExporter frame %d captured (load %.1f ms, capture %.1f ms, write %.1f ms)",
                totalFrames, loadNanos / 1_000_000.0, captureNanos / 1_000_000.0, writeNanos / 1_000_000.0));
            }
            totalCommands++;
          }
        }
        if (writer != null) {
          gm.warn(Resources.getString("VideoExporter.finished", videoFile.getAbsolutePath()));
          final double captureMs = captureNanos / 1_000_000.0;
          final double writeMs = writeNanos / 1_000_000.0;
          final double stepMs = stepNanos / 1_000_000.0;
          final double loadMs = loadNanos / 1_000_000.0;
          gm.warn(String.format(
            "VideoExporter profile: capture %.1f ms, write %.1f ms, step %.1f ms, load %.1f ms, frames %d, commands %d",
            captureMs, writeMs, stepMs, loadMs, totalFrames, totalCommands));
        }
      }
      finally {
        if (writer != null) {
          try {
            writer.close();
          }
          catch (IOException e) {
            gm.warn(Resources.getString("VideoExporter.failed", e.getMessage()));
          }
        }
      }
    }
    catch (Exception ex) {
      gm.warn(Resources.getString("VideoExporter.failed", ex.getMessage()));
    }
    finally {
      gameState.setSuppressSavePrompt(wasSavePromptSuppressed);
      logger.setSuppressReplayPrompts(false);
    }
  }

  public static void startGlobalExport() {
    final GameModule gm = GameModule.getGameModule();
    final java.util.List<Map> maps = Map.getMapList();
    if (maps.isEmpty()) {
      gm.warn(Resources.getString("VideoExporter.no_map"));
      return;
    }
    final Map target = maps.get(0);
    VideoExporter exporter = null;
    for (final VideoExporter ve : target.getComponentsOf(VideoExporter.class)) {
      exporter = ve;
      break;
    }
    if (exporter == null) {
      exporter = new VideoExporter();
      exporter.addTo(target);
    }
    exporter.startExport();
  }

  private double maximizeZoom(Map targetMap) {
    if (targetMap == null) {
      return 1.0;
    }
    final double original = targetMap.getZoom();
    final Zoomer zoomer = targetMap.getZoomer();
    if (zoomer == null) {
      return original;
    }
    double before;
    do {
      before = targetMap.getZoom();
      zoomer.zoomIn();
    }
    while (targetMap.getZoom() > before + 1e-6);
    return original;
  }

  private void restoreZoom(Map targetMap, double zoomFactor) {
    if (targetMap == null) {
      return;
    }
    final Zoomer zoomer = targetMap.getZoomer();
    if (zoomer == null) {
      return;
    }
    zoomer.setZoomFactor(zoomFactor);
  }

  private void captureFrame(BufferedImage frame, Rectangle drawingRect) {
    try {
      SwingUtilities.invokeAndWait(() -> {
        final var g2 = frame.createGraphics();
        g2.setColor(map.getView().getBackground());
        g2.fillRect(0, 0, frame.getWidth(), frame.getHeight());
        map.paintRegion(g2, drawingRect, map.getView());
        g2.dispose();
      });
    }
    catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private boolean commandLikelyChangesMap(Command command) {
    if (command == null) {
      return true;
    }
    if (command.isNull() || command.isNullOrContainsOnly(Chatter.DisplayText.class)) {
      return false;
    }
    return true;
  }

  private static class FfmpegWriter implements AutoCloseable {
    private final Process process;
    private final OutputStream output;

    FfmpegWriter(int width, int height, int fps, File outputFile) throws IOException {
      process = new ProcessBuilder(
        "ffmpeg",
        "-y",
        "-f", "rawvideo",
        "-pix_fmt", "bgr24",
        "-s", width + "x" + height,
        "-r", Integer.toString(fps),
        "-i", "-",
        "-an",
        "-c:v", "libx264",
        "-pix_fmt", "yuv420p",
        outputFile.getAbsolutePath()
      ).redirectError(ProcessBuilder.Redirect.INHERIT).start();
      output = process.getOutputStream();
    }

    void writeFrame(BufferedImage frame) throws IOException {
      final byte[] data = ((DataBufferByte) frame.getRaster().getDataBuffer()).getData();
      output.write(data);
    }

    @Override
    public void close() throws IOException {
      try {
        output.close();
        process.waitFor();
      }
      catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
    }
  }
}
