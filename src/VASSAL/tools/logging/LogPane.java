package VASSAL.tools.logging;

import java.io.File;
import javax.swing.JTextArea;
import javax.swing.event.AncestorEvent;
import javax.swing.event.AncestorListener;

import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.io.Tailer;

public class LogPane extends JTextArea {
  private static final long serialVersionUID = 1L;
  
  protected final Tailer tailer;

  public LogPane(File file) {
    setEditable(false);
    setLineWrap(true);
    setWrapStyleWord(true);
    setTabSize(2);

    tailer = new Tailer(file);

    tailer.addEventListener(new EventListener<String>() {
      public void receive(Object src, String s) {
        // NB: JTextArea.append() is thread-safe; it can be called off-EDT.
        append(s);
      }
    });

    // tail the file only when the pane is visible
    addAncestorListener(new AncestorListener() {
      public void ancestorRemoved(AncestorEvent e) {
        tailer.stop();
      }

      public void ancestorAdded(AncestorEvent e) {
        tailer.start();
      }

      public void ancestorMoved(AncestorEvent e) {} 
    });
  }
}
