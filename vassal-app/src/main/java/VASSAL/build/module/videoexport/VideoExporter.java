package VASSAL.build.module.videoexport;

import VASSAL.build.GameModule;
import VASSAL.build.module.BasicLogger;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.RectangularSelector;
import VASSAL.build.module.map.Zoomer;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;

import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;

/**
 * Menu-triggered video export utility.
 */
public class VideoExporter {
  private static final int MAX_VIDEO_WIDTH = Integer.getInteger("VideoExporter.maxWidth", 3840);
  private static final int MAX_VIDEO_HEIGHT = Integer.getInteger("VideoExporter.maxHeight", 2160);
  private static final int FPS = Integer.getInteger("VideoExporter.fps", 10);

  private final Map map;
  private Rectangle cropSelection;

  public VideoExporter(Map map) {
    this.map = map;
  }

  public void startExport() {
    SwingUtilities.invokeLater(this::showExportDialog);
  }

  private void showExportDialog() {
    final GameModule gm = GameModule.getGameModule();
    final File[] selectedLogFolder = new File[1];
    final List<File>[] orderedLogsRef = new List[1];
    final File[] outputFileRef = new File[1];
    final boolean[] firstLogLoaded = new boolean[1];
    final boolean[] cropSelecting = new boolean[1];

    final VideoExportDialog dialog = new VideoExportDialog(map);

    final Runnable updateControls = () -> {
      final boolean hasLogs = orderedLogsRef[0] != null && !orderedLogsRef[0].isEmpty();
      final boolean hasOutput = outputFileRef[0] != null;
      final boolean cropReady = hasLogs && firstLogLoaded[0];
      dialog.setEnabledStates(!cropSelecting[0], cropReady && !cropSelecting[0], hasLogs && !cropSelecting[0], hasLogs && hasOutput && !cropSelecting[0]);
    };

    dialog.getBrowseLogs().addActionListener(e -> {
      final JFileChooser dirChooser = dialog.newLogChooser();
      if (dirChooser.showOpenDialog(dialog.getFrame()) != JFileChooser.APPROVE_OPTION) {
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
      dialog.setLogPath(directory.getAbsolutePath());
      final File defaultOut = new File(directory, directory.getName() + ".mp4");
      dialog.setOutputPath(defaultOut.getAbsolutePath());
      outputFileRef[0] = defaultOut;
      updateControls.run();
      // preload first log so cropping works
      new Thread(() -> {
        try {
          SwingUtilities.invokeAndWait(() -> gm.getGameState().loadGame(logFiles[0], false, true));
          firstLogLoaded[0] = true;
          SwingUtilities.invokeLater(updateControls);
        }
        catch (Exception ex) {
          gm.warn(Resources.getString("VideoExporter.load_failed"));
        }
      }, "VideoExporter-Preload").start();
    });

    dialog.getBrowseOut().addActionListener(e -> {
      final JFileChooser outChooser = dialog.newOutputChooser();
      if (selectedLogFolder[0] != null) {
        outChooser.setCurrentDirectory(selectedLogFolder[0]);
      }
      if (outChooser.showSaveDialog(dialog.getFrame()) != JFileChooser.APPROVE_OPTION) {
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
      dialog.setOutputPath(f.getAbsolutePath());
      updateControls.run();
    });

    dialog.getCropButton().addActionListener(e -> {
      if (orderedLogsRef[0] == null || orderedLogsRef[0].isEmpty()) {
        gm.warn(Resources.getString("VideoExporter.no_logs"));
        return;
      }
      if (!firstLogLoaded[0]) {
        gm.warn(Resources.getString("VideoExporter.load_failed"));
        return;
      }
      dialog.setCropButtonText("Selecting... draw rectangle, Enter to confirm, Esc to cancel");
      cropSelecting[0] = true;
      updateControls.run();
      new Thread(() -> {
        final Rectangle selected = RectangularSelector.select(map, cropSelection);
        if (selected != null) {
          cropSelection = selected;
          gm.warn("Crop area set to " + rectSummary(selected));
          SwingUtilities.invokeLater(() -> dialog.setCropStatus("Crop: " + rectSummary(selected)));
        }
        else {
          gm.warn("Crop selection cancelled; existing crop unchanged.");
        }
        SwingUtilities.invokeLater(() -> {
          dialog.setCropButtonText("Select Crop Area");
          cropSelecting[0] = false;
          updateControls.run();
        });
      }, "VideoExporter-Crop").start();
    });

    dialog.getFullMapButton().addActionListener(e -> {
      cropSelection = null;
      dialog.setCropStatus("Crop: full-map");
      gm.warn("Crop selection cleared; using full map.");
      updateControls.run();
    });

    dialog.getStartButton().addActionListener(e -> {
      if (orderedLogsRef[0] == null || outputFileRef[0] == null) {
        gm.warn("Please select logs and output file first.");
        return;
      }
      dialog.dispose();
      final List<File> logs = orderedLogsRef[0];
      final File finalVideo = outputFileRef[0];
      final double restoreZoom = map.getZoom();
      new Thread(() -> {
        try {
          renderLogs(logs, finalVideo);
        }
        finally {
          SwingUtilities.invokeLater(() -> restoreZoom(map, restoreZoom));
        }
      }, "VideoExporter").start();
    });

    updateControls.run();
    dialog.show();
  }

  private void renderLogs(List<File> logFiles, File videoFile) {
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
          final Rectangle cropArea = cropSelection != null ? new Rectangle(cropSelection) : fullMapRect(map);
          SwingUtilities.invokeAndWait(() -> limitZoomToFit(map, cropArea, MAX_VIDEO_WIDTH, MAX_VIDEO_HEIGHT));

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
            writer = new FfmpegWriter(videoWidth, videoHeight, FPS, videoFile);
          }

          final long captureStart = System.nanoTime();
          captureFrame(frame, drawingRect);
          captureNanos += System.nanoTime() - captureStart;
          final long writeStart = System.nanoTime();
          writer.writeFrame(frame);
          writeNanos += System.nanoTime() - writeStart;
          totalFrames++;
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
    new VideoExporter(target).startExport();
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

  /**
   * Adjusts the map's zoom so the given rectangle fits within the specified max output dimensions.
   * Small crop areas will zoom in (up to the max bounds), large ones will zoom out.
   */
  private void limitZoomToFit(Map targetMap, Rectangle cropArea, int maxWidth, int maxHeight) {
    if (targetMap == null || maxWidth <= 0 || maxHeight <= 0) {
      return;
    }
    final Zoomer zoomer = targetMap.getZoomer();
    if (zoomer == null) {
      return;
    }
    final Rectangle targetRect = cropArea != null
      ? new Rectangle(cropArea)
      : new Rectangle(0, 0, targetMap.mapSize().width, targetMap.mapSize().height);
    final int targetWidth = targetRect.width;
    final int targetHeight = targetRect.height;
    if (targetWidth <= 0 || targetHeight <= 0) {
      return;
    }
    double fitZoom = Math.min((double) maxWidth / targetWidth, (double) maxHeight / targetHeight);
    if (!Double.isFinite(fitZoom) || fitZoom <= 0) {
      return;
    }
    if (Math.abs(targetMap.getZoom() - fitZoom) > 1e-6) {
      zoomer.setZoomFactor(fitZoom);
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
}
