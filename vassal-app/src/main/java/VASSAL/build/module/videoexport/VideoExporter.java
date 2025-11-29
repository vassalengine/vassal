package VASSAL.build.module.videoexport;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.RectangularSelector;
import VASSAL.build.module.map.Zoomer;
import VASSAL.i18n.Resources;

import java.awt.Rectangle;
import java.io.File;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import javax.swing.JFileChooser;
import javax.swing.SwingUtilities;

/**
 * Menu-triggered video export utility.
 */
public class VideoExporter {
  private static final int DEFAULT_MAX_VIDEO_WIDTH = 3840;
  private static final int DEFAULT_MAX_VIDEO_HEIGHT = 2160;
  private static final int DEFAULT_FPS = 10;

  private final Map map;
  private final VideoRenderService renderService;
  private Rectangle cropSelection;

  public VideoExporter(Map map) {
    this.map = map;
    this.renderService = new VideoRenderService();
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

    final VideoExportDialog dialog = new VideoExportDialog();
    dialog.setFps(DEFAULT_FPS);
    dialog.setResolution(DEFAULT_MAX_VIDEO_WIDTH, DEFAULT_MAX_VIDEO_HEIGHT);

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
      int fps;
      try {
        fps = Integer.parseInt(dialog.getFpsText().trim());
        if (fps <= 0) {
          throw new NumberFormatException("fps must be positive");
        }
      }
      catch (NumberFormatException ex) {
        gm.warn("Please enter a positive FPS value.");
        return;
      }
      int maxWidth;
      int maxHeight;
      try {
        maxWidth = Integer.parseInt(dialog.getWidthText().trim());
        maxHeight = Integer.parseInt(dialog.getHeightText().trim());
        if (maxWidth <= 0 || maxHeight <= 0) {
          throw new NumberFormatException("dimensions must be positive");
        }
      }
      catch (NumberFormatException ex) {
        gm.warn("Please enter a positive width and height.");
        return;
      }
      final int fpsToUse = fps;
      final int maxWidthToUse = maxWidth;
      final int maxHeightToUse = maxHeight;
      dialog.dispose();
      final List<File> logs = orderedLogsRef[0];
      final File finalVideo = outputFileRef[0];
      final double restoreZoom = map.getZoom();
      new Thread(() -> {
        try {
          renderService.render(map, cropSelection, logs, finalVideo, fpsToUse, maxWidthToUse, maxHeightToUse);
        }
        finally {
          SwingUtilities.invokeLater(() -> restoreZoom(map, restoreZoom));
        }
      }, "VideoExporter").start();
    });

    updateControls.run();
    dialog.show();
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

  private String rectSummary(Rectangle r) {
    return "x=" + r.x + ",y=" + r.y + ",w=" + r.width + ",h=" + r.height;
  }
}
