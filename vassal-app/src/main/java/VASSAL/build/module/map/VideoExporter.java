package VASSAL.build.module.map;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.BasicLogger;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.command.Command;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.FlowLayout;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

/**
 * Prototype toolbar item that renders a vlog replay directly into an ffmpeg process.
 */
public class VideoExporter extends AbstractToolbarItem {
  private static final String DEFAULT_ICON = "/images/camera.gif";
  private static final int MAX_VIDEO_WIDTH = Integer.getInteger("VideoExporter.maxWidth", 3840);
  private static final int MAX_VIDEO_HEIGHT = Integer.getInteger("VideoExporter.maxHeight", 2160);

  private Map map;
  private Rectangle cropSelection;

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
    SwingUtilities.invokeLater(this::showExportDialog);
  }

  private void showExportDialog() {
    final GameModule gm = GameModule.getGameModule();
    final JFrame frame = new JFrame(Resources.getString("VideoExporter.button"));
    frame.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);

    final JTextField logField = new JTextField();
    logField.setEditable(false);
    final JButton browseLogs = new JButton("Select Log Folder");
    final JTextField outField = new JTextField();
    outField.setEditable(false);
    final JButton browseOut = new JButton("Select Output");
    final JButton cropButton = new JButton("Select Crop Area");
    cropButton.setEnabled(false);
    final JButton fullMapButton = new JButton("Use Full Map");
    fullMapButton.setEnabled(false);
    final JButton startButton = new JButton("Start Rendering");
    startButton.setEnabled(false);

    final JPanel content = new JPanel();
    content.setLayout(new BoxLayout(content, BoxLayout.Y_AXIS));

    final JPanel logsPanel = new JPanel(new BorderLayout(5, 5));
    logsPanel.add(new JLabel("Log folder:"), BorderLayout.WEST);
    logsPanel.add(logField, BorderLayout.CENTER);
    logsPanel.add(browseLogs, BorderLayout.EAST);

    final JPanel outPanel = new JPanel(new BorderLayout(5, 5));
    outPanel.add(new JLabel("Output file:"), BorderLayout.WEST);
    outPanel.add(outField, BorderLayout.CENTER);
    outPanel.add(browseOut, BorderLayout.EAST);

    final JLabel cropStatus = new JLabel("Crop: full-map");
    cropStatus.setAlignmentX(Component.LEFT_ALIGNMENT);

    final JPanel buttons = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttons.add(cropButton);
    buttons.add(fullMapButton);
    buttons.add(startButton);

    final JLabel heading = new JLabel("Render Logs to Video");
    heading.setAlignmentX(Component.LEFT_ALIGNMENT);
    final JLabel subText = new JLabel("Select a log folder, optional crop area, and output mp4.");
    subText.setAlignmentX(Component.LEFT_ALIGNMENT);

    content.add(heading);
    content.add(subText);
    content.add(Box.createVerticalStrut(8));
    content.add(logsPanel);
    content.add(Box.createVerticalStrut(8));
    content.add(outPanel);
    content.add(Box.createVerticalStrut(8));
    final JPanel cropStatusPanel = new JPanel(new BorderLayout());
    cropStatusPanel.add(cropStatus, BorderLayout.CENTER);
    content.add(cropStatusPanel);
    content.add(Box.createVerticalStrut(8));
    content.add(buttons);

    frame.getContentPane().add(content);
    frame.pack();
    frame.setLocationRelativeTo(map.getView());
    frame.setVisible(true);

    final int fps = 5;

    final File[] selectedLogFolder = new File[1];
    final List<File>[] orderedLogsRef = new List[1];
    final File[] outputFileRef = new File[1];
    final boolean[] firstLogLoaded = new boolean[1];
    final boolean[] cropSelecting = new boolean[1];

    final Runnable updateControlStates = () -> {
      final boolean hasLogs = orderedLogsRef[0] != null && !orderedLogsRef[0].isEmpty();
      final boolean hasOutput = outputFileRef[0] != null;
      final boolean cropReady = hasLogs && firstLogLoaded[0];
      browseLogs.setEnabled(!cropSelecting[0]);
      browseOut.setEnabled(!cropSelecting[0]);
      cropButton.setEnabled(cropReady && !cropSelecting[0]);
      fullMapButton.setEnabled(hasLogs && !cropSelecting[0]);
      startButton.setEnabled(hasLogs && hasOutput && !cropSelecting[0]);
    };

    browseLogs.addActionListener(e -> {
      final JFileChooser dirChooser = new JFileChooser();
      dirChooser.setDialogTitle(Resources.getString("VideoExporter.folder_dialog"));
      dirChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
      if (dirChooser.showOpenDialog(frame) != JFileChooser.APPROVE_OPTION) {
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
      orderedLogsRef[0] = Arrays.asList(logFiles);
      selectedLogFolder[0] = directory;
      logField.setText(directory.getAbsolutePath());
      final File defaultOut = new File(directory, directory.getName() + ".mp4");
      outField.setText(defaultOut.getAbsolutePath());
      outputFileRef[0] = defaultOut;
      updateControlStates.run();
      // Load first log in background so crop can work.
      new Thread(() -> {
        try {
          SwingUtilities.invokeAndWait(() -> gm.getGameState().loadGame(logFiles[0], false, true));
          firstLogLoaded[0] = true;
          SwingUtilities.invokeLater(updateControlStates);
        }
        catch (Exception ex) {
          gm.warn(Resources.getString("VideoExporter.load_failed"));
        }
      }, "VideoExporter-Preload").start();
    });

    browseOut.addActionListener(e -> {
      final JFileChooser outChooser = new JFileChooser();
      outChooser.setDialogTitle(Resources.getString("VideoExporter.output_dialog"));
      if (selectedLogFolder[0] != null) {
        outChooser.setCurrentDirectory(selectedLogFolder[0]);
      }
      if (outChooser.showSaveDialog(frame) != JFileChooser.APPROVE_OPTION) {
        return;
      }
      File f = outChooser.getSelectedFile();
      if (f == null) {
        return;
      }
      if (!f.getName().toLowerCase().endsWith(".mp4")) {
        f = new File(f.getParentFile(), f.getName() + ".mp4");
      }
      outputFileRef[0] = f;
      outField.setText(f.getAbsolutePath());
      updateControlStates.run();
    });

    cropButton.addActionListener(e -> {
      if (orderedLogsRef[0] == null || orderedLogsRef[0].isEmpty()) {
        return;
      }
      cropButton.setText("Selecting... draw rectangle, Enter to confirm, Esc to cancel");
      cropSelecting[0] = true;
      updateControlStates.run();
      new Thread(() -> {
        // Ensure first log loaded (if preload failed)
        if (!firstLogLoaded[0]) {
          try {
            SwingUtilities.invokeAndWait(() -> gm.getGameState().loadGame(orderedLogsRef[0].get(0), false, true));
            firstLogLoaded[0] = true;
          }
          catch (Exception ex) {
            gm.warn(Resources.getString("VideoExporter.load_failed"));
            SwingUtilities.invokeLater(() -> {
              cropButton.setText("Select Crop Area");
              cropSelecting[0] = false;
              updateControlStates.run();
            });
            return;
          }
        }
        final Rectangle selected = RectangularSelector.select(map, cropSelection);
        if (selected != null) {
          cropSelection = selected;
          gm.warn("Crop area set to " + rectSummary(selected));
          SwingUtilities.invokeLater(() -> cropStatus.setText("Crop: " + rectSummary(selected)));
        }
        else {
          gm.warn("Crop selection cancelled; existing crop unchanged.");
        }
        SwingUtilities.invokeLater(() -> {
          cropButton.setText("Select Crop Area");
          cropSelecting[0] = false;
          updateControlStates.run();
        });
      }, "VideoExporter-Crop").start();
    });

    fullMapButton.addActionListener(e -> {
      cropSelection = null;
      cropStatus.setText("Crop: full-map");
      gm.warn("Crop selection cleared; using full map.");
    });

    startButton.addActionListener(e -> {
      if (orderedLogsRef[0] == null || outputFileRef[0] == null) {
        gm.warn("Please select logs and output file first.");
        return;
      }
      frame.dispose();
      final List<File> logs = orderedLogsRef[0];
      final File finalVideo = outputFileRef[0];
      final double restoreZoom = map.getZoom();
      new Thread(() -> {
        try {
          renderLogs(logs, finalVideo, fps);
        }
        finally {
          SwingUtilities.invokeLater(() -> restoreZoom(map, restoreZoom));
        }
      }, "VideoExporter").start();
    });
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
      final long frameIntervalNanos = fps > 0 ? 1_000_000_000L / fps : 0;
      long nextFrameDeadline = 0;
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
          final Rectangle cropArea = cropSelection != null ? new Rectangle(cropSelection) : fullMapRect(map);
          SwingUtilities.invokeAndWait(() -> limitZoomToFit(map, cropArea, MAX_VIDEO_WIDTH, MAX_VIDEO_HEIGHT, true));

          if (writer == null) {
            final Rectangle[] captureRectHolder = new Rectangle[1];
            final int[] dimensions = new int[2];
            SwingUtilities.invokeAndWait(() -> {
              final Rectangle mapRect = new Rectangle(cropArea);
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
          nextFrameDeadline = throttleFrameRate(frameIntervalNanos, nextFrameDeadline);
          gm.warn(String.format("VideoExporter frame %d captured (crop %s, load %.1f ms, capture %.1f ms, write %.1f ms)",
            totalFrames,
            cropSelection != null ? rectSummary(cropSelection) : "full-map",
            loadNanos / 1_000_000.0, captureNanos / 1_000_000.0, writeNanos / 1_000_000.0));

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
              nextFrameDeadline = throttleFrameRate(frameIntervalNanos, nextFrameDeadline);
              gm.warn(String.format("VideoExporter frame %d captured (crop %s, load %.1f ms, capture %.1f ms, write %.1f ms)",
                totalFrames,
                cropSelection != null ? rectSummary(cropSelection) : "full-map",
                loadNanos / 1_000_000.0, captureNanos / 1_000_000.0, writeNanos / 1_000_000.0));
            }
            totalCommands++;
          }
        }
        if (writer != null) {
          gm.warn(Resources.getString("VideoExporter.finished", videoFile.getAbsolutePath())
            + (cropSelection != null ? " (crop " + rectSummary(cropSelection) + ")" : " (full-map)"));
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

  public static void startCropSelectionCommand() {
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
    exporter.beginCropSelection();
  }

  private void beginCropSelection() {
    final GameModule gm = GameModule.getGameModule();
    gm.warn("Draw the crop box on the map, then press Enter to confirm or Esc to cancel.");
    new Thread(() -> {
      final Rectangle selected = RectangularSelector.select(map, cropSelection);
      if (selected != null) {
        cropSelection = selected;
        gm.warn("Crop area set to " + selected);
      }
      else {
        gm.warn("Crop selection cancelled; existing crop unchanged.");
      }
    }, "CropSelection").start();
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

  private void limitZoomToFit(Map targetMap, Rectangle cropArea, int maxWidth, int maxHeight, boolean allowZoomIn) {
    if (targetMap == null || maxWidth <= 0 || maxHeight <= 0) {
      return;
    }
    final Zoomer zoomer = targetMap.getZoomer();
    if (zoomer == null) {
      return;
    }
    final Rectangle mapRect = cropArea != null
      ? new Rectangle(cropArea)
      : new Rectangle(0, 0, targetMap.mapSize().width, targetMap.mapSize().height);
    final int width = mapRect.width;
    final int height = mapRect.height;
    if (width <= 0 || height <= 0) {
      return;
    }
    double targetZoom = Math.min((double) maxWidth / width, (double) maxHeight / height);
    if (!allowZoomIn && targetZoom > 1.0) {
      targetZoom = 1.0;
    }
    if (!Double.isFinite(targetZoom) || targetZoom <= 0) {
      return;
    }
    // Set absolute zoom so the selected area uses the largest resolution within limits.
    if (Math.abs(targetMap.getZoom() - targetZoom) > 1e-6) {
      zoomer.setZoomFactor(targetZoom);
    }
  }

  private Rectangle fullMapRect(Map targetMap) {
    return new Rectangle(0, 0, targetMap.mapSize().width, targetMap.mapSize().height);
  }

  private String rectSummary(Rectangle r) {
    return "x=" + r.x + ",y=" + r.y + ",w=" + r.width + ",h=" + r.height;
  }

  private void captureFrame(BufferedImage frame, Rectangle drawingRect) {
    try {
      SwingUtilities.invokeAndWait(() -> {
        final var g2 = frame.createGraphics();
        g2.setColor(map.getView().getBackground());
        g2.fillRect(0, 0, frame.getWidth(), frame.getHeight());
        g2.translate(-drawingRect.x, -drawingRect.y);
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

  private long throttleFrameRate(long frameIntervalNanos, long nextFrameDeadline) {
    if (frameIntervalNanos <= 0) {
      return nextFrameDeadline;
    }
    long target = nextFrameDeadline == 0 ? System.nanoTime() + frameIntervalNanos : nextFrameDeadline;
    final long sleepNanos = target - System.nanoTime();
    if (sleepNanos > 0) {
      try {
        TimeUnit.NANOSECONDS.sleep(sleepNanos);
      }
      catch (InterruptedException e) {
        Thread.currentThread().interrupt();
      }
    }
    return target + frameIntervalNanos;
  }

  private static class FfmpegWriter implements AutoCloseable {
    private final Process process;
    private final OutputStream output;

    FfmpegWriter(int width, int height, int fps, File outputFile) throws IOException {
      process = new ProcessBuilder(
        "ffmpeg",
        "-y",
        "-f", "rawvideo",
        "-v", "debug",
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
