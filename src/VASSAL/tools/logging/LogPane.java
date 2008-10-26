package VASSAL.tools.logging;

import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

public class LogPane extends JTextArea implements LogListener {
  private static final long serialVersionUID = 1L; 
  
  public LogPane() {
    setEditable(false);
    setLineWrap(true);
    setWrapStyleWord(true);
  }

  public void handle(LogEntry entry) {
    final String es = entry.toString();

    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        append(es);
        append("\n");
      }
    });
  }
}
