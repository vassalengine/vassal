package VASSAL.build.module.map;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.BasicLogger;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.LogFileFilter;

import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;

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
      e -> SwingUtilities.invokeLater(this::promptAndStart)
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

  private void promptAndStart() {
    final GameModule gm = GameModule.getGameModule();
    final FileChooser logChooser = gm.getFileChooser();
    logChooser.addChoosableFileFilter(new LogFileFilter());
    if (logChooser.showOpenDialog(map.getView()) != FileChooser.APPROVE_OPTION) {
      return;
    }
    final File logFile = logChooser.getSelectedFile();
    if (logFile == null || !logFile.exists()) {
      gm.warn(Resources.getString("VideoExporter.missing_log"));
      return;
    }

    final JFileChooser videoChooser = new JFileChooser(logFile.getParentFile());
    videoChooser.setDialogTitle(Resources.getString("VideoExporter.output_dialog"));
    videoChooser.setSelectedFile(new File(logFile.getParentFile(), logFile.getName().replaceAll("\\.vlog$", "") + ".mp4"));
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
    new Thread(() -> renderLog(logFile, finalVideo, fps), "VideoExporter").start();
  }

  private void renderLog(File logFile, File videoFile, int fps) {
    final GameModule gm = GameModule.getGameModule();
    final BasicLogger logger = gm.getBasicLogger();

    try {
      final boolean[] loaded = new boolean[1];
      SwingUtilities.invokeAndWait(() -> loaded[0] = gm.getGameState().loadGame(logFile, false, true));
      if (!loaded[0]) {
        gm.warn(Resources.getString("VideoExporter.load_failed"));
        return;
      }

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

      final Rectangle drawingRect = new Rectangle(captureRectHolder[0]);
      final int width = dimensions[0];
      final int height = dimensions[1];
      final int videoWidth = (width % 2 == 0) ? width : width + 1;
      final int videoHeight = (height % 2 == 0) ? height : height + 1;
      final BufferedImage frame = new BufferedImage(videoWidth, videoHeight, BufferedImage.TYPE_3BYTE_BGR);

      logger.setSuppressReplayPrompts(true);
      try (FfmpegWriter writer = new FfmpegWriter(videoWidth, videoHeight, fps, videoFile)) {
        captureFrame(frame, drawingRect);
        writer.writeFrame(frame);

        while (true) {
          final boolean[] stepped = new boolean[1];
          SwingUtilities.invokeAndWait(() -> stepped[0] = logger.stepForward());
          if (!stepped[0]) {
            break;
          }
          captureFrame(frame, drawingRect);
          writer.writeFrame(frame);
        }
      }
      gm.warn(Resources.getString("VideoExporter.finished", videoFile.getAbsolutePath()));
    }
    catch (Exception ex) {
      gm.warn(Resources.getString("VideoExporter.failed", ex.getMessage()));
    }
    finally {
      logger.setSuppressReplayPrompts(false);
    }
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
