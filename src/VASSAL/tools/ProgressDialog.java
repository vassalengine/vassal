package VASSAL.tools;

import java.awt.Frame;

/** @deprecated Moved to {@link VASSAL.tools.swing.ProgressDialog}. */
@Deprecated
public class ProgressDialog extends VASSAL.tools.swing.ProgressDialog {
  private static final long serialVersionUID = 1L;

  public ProgressDialog(Frame parent, String title, String text) {
    super(parent, title, text);
  }
}
