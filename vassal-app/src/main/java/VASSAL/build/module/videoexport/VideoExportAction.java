package VASSAL.build.module.videoexport;

import VASSAL.i18n.Resources;

import javax.swing.AbstractAction;
import java.awt.event.ActionEvent;

/**
 * Menu action that launches the video export workflow.
 */
public class VideoExportAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  public VideoExportAction() {
    super(Resources.getString("VideoExporter.menu"));
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    VideoExporter.startGlobalExport();
  }
}
