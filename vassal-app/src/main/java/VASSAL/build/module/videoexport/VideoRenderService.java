package VASSAL.build.module.videoexport;

import VASSAL.build.GameModule;
import VASSAL.build.module.BasicLogger;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.Zoomer;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;

import javax.swing.SwingUtilities;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Encapsulates the rendering loop: replay logs, capture frames, and stream to ffmpeg.
 */
class VideoRenderService {
  private final int maxWidth;
  private final int maxHeight;
  private final int fps;

  VideoRenderService(int maxWidth, int maxHeight, int fps) {
    this.maxWidth = maxWidth;
    this.maxHeight = maxHeight;
    this.fps = fps;
  }

  void render(Map map, Rectangle cropSelection, List<File> logFiles, File videoFile) {
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
          SwingUtilities.invokeAndWait(() -> limitZoomToFit(map, cropArea));

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
          captureFrame(map, frame, drawingRect);
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
              captureFrame(map, frame, drawingRect);
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

  private void captureFrame(Map map, BufferedImage frame, Rectangle drawingRect) {
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

  private void limitZoomToFit(Map targetMap, Rectangle cropArea) {
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
