package VASSAL.build.module;

import java.awt.Event;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.Logger;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.FileChooser;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.Obfuscator;

public class BasicLogger implements Logger, Buildable, GameComponent, CommandEncoder {
  public static final String BEGIN = "begin_log";
  public static final String END = "end_log";
  public static final String LOG = "LOG\t";
  public static final String PROMPT_NEW_LOG = "PromptNewLog";
  public static final String PROMPT_NEW_LOG_START = "PromptNewLogStart";
  public static final String PROMPT_NEW_LOG_END = "PromptNewLogEnd";
  protected static final String STEP_ICON = "/images/StepForward16.gif";
  protected static final String UNDO_ICON = "/images/Undo16.gif";
  protected List logInput;
  protected List logOutput;
  protected int nextInput = 0;
  protected int nextUndo = -1;
  protected Command beginningState;
  protected File outputFile;
  protected Action stepAction = new StepAction();

  public BasicLogger() {
    super();
    stepAction.setEnabled(false);
    undoAction.setEnabled(false);
    endLogAction.setEnabled(false);
    newLogAction.setEnabled(false);
    logInput = new ArrayList();
    logOutput = new ArrayList();
  }

  public void build(org.w3c.dom.Element e) {
  }

  /**
   * Expects to be added to a {@link GameModule}. Adds <code>Undo</code>, <code>Step</code>, and
   * <code>End Log</code> buttons to the the control window toolbar. Registers {@link KeyStrokeListener}s for hotkey
   * equivalents of each button
   */
  public void addTo(Buildable b) {
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().getFileMenu().add(newLogAction).setMnemonic('B');
    JButton button = GameModule.getGameModule().getToolBar().add(undoAction);
    button.setToolTipText("Undo last move");
    button.setAlignmentY((float) 0.0);
    button = GameModule.getGameModule().getToolBar().add(stepAction);
    button.setToolTipText("Step forward through logfile [Page Down]");
    button.setAlignmentY((float) 0.0);
    GameModule.getGameModule().getFileMenu().add(endLogAction).setMnemonic('E');
    final KeyStrokeListener stepKeyListener = new KeyStrokeListener(stepAction, KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, 0));
    GameModule.getGameModule().addKeyStrokeListener(stepKeyListener);
    KeyStrokeListener newLogKeyListener = new KeyStrokeListener(newLogAction, KeyStroke.getKeyStroke(KeyEvent.VK_W, Event.ALT_MASK));
    GameModule.getGameModule().addKeyStrokeListener(newLogKeyListener);
    GameModule.getGameModule().getFrame().addComponentListener(new ComponentAdapter() {
      public void componentShown(ComponentEvent e) {
        GameModule.getGameModule().getFrame().removeComponentListener(this);
        final IconConfigurer stepIconConfig = new IconConfigurer("stepIcon", "Step forward button icon", STEP_ICON);
        stepIconConfig.setValue(STEP_ICON);
        GlobalOptions.getInstance().addOption(stepIconConfig);
        stepIconConfig.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            stepAction.putValue(Action.SMALL_ICON, stepIconConfig.getIconValue());
          }
        });
        stepIconConfig.fireUpdate();
        final IconConfigurer undoIconConfig = new IconConfigurer("undoIcon", "Undo button icon", UNDO_ICON);
        undoIconConfig.setValue(UNDO_ICON);
        GlobalOptions.getInstance().addOption(undoIconConfig);
        undoIconConfig.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            undoAction.putValue(Action.SMALL_ICON, undoIconConfig.getIconValue());
          }
        });
        undoIconConfig.fireUpdate();
        final HotKeyConfigurer stepKeyConfig = new HotKeyConfigurer("stepHotKey", "Step forward hotkey", stepKeyListener.getKeyStroke());
        GlobalOptions.getInstance().addOption(stepKeyConfig);
        stepKeyConfig.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            stepKeyListener.setKeyStroke((KeyStroke) stepKeyConfig.getValue());
            stepAction.putValue(Action.SHORT_DESCRIPTION, "Step Forward through logfile [" + HotKeyConfigurer.getString(stepKeyListener.getKeyStroke()) + "]");
          }
        });
        stepKeyConfig.fireUpdate();
      }
    });
    BooleanConfigurer logOptionStart = new BooleanConfigurer(PROMPT_NEW_LOG_START, "Prompt to start new Log File before a Replay?", Boolean.FALSE);
    GameModule.getGameModule().getPrefs().addOption("General", logOptionStart);
    BooleanConfigurer logOptionEnd = new BooleanConfigurer(PROMPT_NEW_LOG_END, "Prompt to start new Log File after a Replay?", Boolean.TRUE);
    GameModule.getGameModule().getPrefs().addOption("General", logOptionEnd);  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  public void add(Buildable b) {
  }

  public void remove(Buildable b) {
  }

  public void setup(boolean show) {
    newLogAction.setEnabled(show);
    if (show) {
      logOutput.clear();
      nextInput = 0;
      nextUndo = -1;
      beginningState = GameModule.getGameModule().getGameState().getRestoreCommand();
      queryNewLogFile(true);
    }
    else {
      if (endLogAction.isEnabled()) {
        if (JOptionPane.showConfirmDialog(GameModule.getGameModule().getFrame(), "You are writing a logfile.\nSave now?", "Unsaved log",
            JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
          try {
            write();
          }
          catch (IOException ex) {
            String msg = "Unable to write to " + outputFile;
            if (ex.getMessage() != null) {
              msg += ".\n" + ex.getMessage();
            }
            JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), msg, "Save failed", JOptionPane.ERROR_MESSAGE);
          }
        }
      }
      logInput.clear();
      beginningState = null;
      undoAction.setEnabled(false);
      endLogAction.setEnabled(false);
      stepAction.setEnabled(false);
    }
  }

  public Command getRestoreCommand() {
    return null;
  }

  public void enableDrawing(boolean show) {
  }

  protected void step() {
    Command c = (Command) logInput.get(nextInput++);
    c.execute();
    GameModule.getGameModule().sendAndLog(c);
    stepAction.setEnabled(nextInput < logInput.size());
    if (!(nextInput < logInput.size())) {
      queryNewLogFile(false);
    }
  }

  /*
   * Check if user would like to create a new logfile
   */
  public void queryNewLogFile(boolean atStart) {
    String prefName;
    String prompt;
    if (atStart) {
      prefName = PROMPT_NEW_LOG_START;
      prompt = "Replay commencing";
    }
    else {
      prefName = PROMPT_NEW_LOG_END;
      prompt = "Reply completed";
    }
    if (((Boolean) GameModule.getGameModule().getPrefs().getValue(prefName)).booleanValue()) {
      Object[] options = {"Yes", "No", "Don't prompt again"};
      int result = JOptionPane.showOptionDialog(GameModule.getGameModule().getFrame(),
          prompt + ". Start new Log File?",
          "",
          JOptionPane.YES_NO_CANCEL_OPTION,
          JOptionPane.QUESTION_MESSAGE,
          null,
          options,
          options[0]);

      if (result == JOptionPane.YES_OPTION) {
         beginOutput();
      }
      else if (result == 2) { // Turn Preference Off
        GameModule.getGameModule().getPrefs().setValue(prefName, Boolean.FALSE);
      }
    }
  }

  /**
   * Write the logfile to a file. The file will have been selected when the logfile was begun.
   * 
   */
  public void write() throws IOException {
    if (logOutput.size() > 0) {
      Command log = beginningState;
      for (int i = 0; i < logOutput.size(); ++i) {
        log.append(new LogCommand((Command) logOutput.get(i), logInput, stepAction));
      }
      String s = GameModule.getGameModule().encode(log);
      ArchiveWriter saver = new ArchiveWriter(outputFile.getPath());
      byte[] contents = s.getBytes("UTF-8"); 
      ByteArrayOutputStream out = new ByteArrayOutputStream();
      new Obfuscator(contents).write(out);
      out.close();
      contents = out.toByteArray();
      saver.addFile("savedGame", new ByteArrayInputStream(contents));
      saver.write();
      GameModule.getGameModule().getGameState().setModified(false);
      undoAction.setEnabled(false);
      endLogAction.setEnabled(false);
    }
  }
  
  protected void beginOutput() {
    FileChooser fd = GameModule.getGameModule().getFileChooser();
    String name = fd.getSelectedFile().getName();
    if (name != null) {
      int index = name.lastIndexOf('.');
      if (index > 0) {
        name = name.substring(0, index) + ".log";
        fd.setSelectedFile(new File(fd.getSelectedFile().getParentFile(), name));
      }
    }
    if (fd.showSaveDialog() == FileChooser.APPROVE_OPTION) {
      outputFile = fd.getSelectedFile();
      logOutput.clear();
      beginningState = GameModule.getGameModule().getGameState().getRestoreCommand();
      undoAction.setEnabled(true);
      endLogAction.setEnabled(true);
      GameModule.getGameModule().appendToTitle(" - logging to " + outputFile.getName());
      newLogAction.setEnabled(false);
    }
  }

  protected void undo() {
    Command lastOutput = (Command) logOutput.get(nextUndo);
    Command lastInput = (nextInput > logInput.size() || nextInput < 1) ? null : (Command) logInput.get(nextInput - 1);
    if (lastInput == lastOutput) {
      while (nextInput-- > 0) {
        stepAction.setEnabled(true);
        if (((Command) logInput.get(nextInput)).getUndoCommand() != null) {
          break;
        }
      }
    }
    while (nextUndo-- > 0) {
      if (((Command) logOutput.get(nextUndo)).getUndoCommand() != null) {
        break;
      }
    }
    undoAction.setEnabled(nextUndo >= 0);
    Command undo = lastOutput.getUndoCommand();
    undo.execute();
    GameModule.getGameModule().getServer().sendToOthers(undo);
    logOutput.add(undo);
  }

  public void log(Command c) {
    if (c != null && c.isLoggable()) {
      logOutput.add(c);
      if (c.getUndoCommand() != null && !c.getUndoCommand().isNull()) {
        nextUndo = logOutput.size() - 1;
      }
    }
    undoAction.setEnabled(nextUndo >= 0);
  }
  
  /**
   * Are there Input Steps yet to be replayed?
   */
  public boolean hasMoreCommands() {
    return nextInput < logInput.size();
  }

  /**
   * Recognizes a logging command. The logging command is a wrapper around an ordinary {@link Command} indicating that
   * the wrapped command should be stored and executed in sequence (when the <code>Step</code> button is pressed)
   */
  public String encode(Command c) {
    if (c instanceof LogCommand) {
      return LOG + GameModule.getGameModule().encode(((LogCommand) c).getLoggedCommand());
    }
    else {
      return null;
    }
  }

  public Command decode(String command) {
    if (command.startsWith(LOG)) {
      Command logged = GameModule.getGameModule().decode(command.substring(LOG.length()));
      if (logged != null) {
        return new LogCommand(logged, logInput, stepAction);
      }
    }
    return null;
  }
  protected Action undoAction = new UndoAction();
  protected Action endLogAction = new AbstractAction("End Logfile") {
    private static final long serialVersionUID = 1L;

    public void actionPerformed(ActionEvent e) {
      try {
        write();
        GameModule.getGameModule().warn("Logfile written.");
        newLogAction.setEnabled(true);
        GameModule.getGameModule().appendToTitle(null);
      }
      catch (IOException ex) {
        String msg = "Unable to write to " + outputFile;
        if (ex.getMessage() != null) {
          msg += ".\n" + ex.getMessage();
        }
        JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), msg, "Save failed", JOptionPane.ERROR_MESSAGE);
      }
    }
  };
  protected Action newLogAction = new AbstractAction("Begin Logfile") {
    private static final long serialVersionUID = 1L;

    public void actionPerformed(ActionEvent e) {
      beginOutput();
    }
  };
  public static class LogCommand extends Command {
    protected Command logged;
    protected List logInput;
    protected Action stepAction;

    public LogCommand(Command c, List logInput, Action stepAction) {
      if (c instanceof LogCommand) {
        throw new RuntimeException("Can't log a LogCommand");
      }
      this.logInput = logInput;
      this.stepAction = stepAction;
      logged = c;
      Command sub[] = c.getSubCommands();
      for (int i = 0; i < sub.length; ++i) {
        append(new LogCommand(sub[i], logInput, stepAction));
      }
      logged.stripSubCommands();
    }

    protected void executeCommand() {
    }

    protected Command myUndoCommand() {
      return null;
    }

    public Command getLoggedCommand() {
      return logged;
    }

    public void execute() {
      Command c = assembleCommand();
      logInput.add(c);
      stepAction.setEnabled(true);
    }

    protected Command assembleCommand() {
      Command c = logged;
      Command sub[] = getSubCommands();
      for (int i = 0; i < sub.length; ++i) {
        c.append(((LogCommand) sub[i]).assembleCommand());
      }
      return c;
    }
  }
  public class StepAction extends AbstractAction {
    private static final long serialVersionUID = 1L;

    public StepAction() {
      URL iconURL = getClass().getResource(STEP_ICON);
      if (iconURL != null) {
        putValue(Action.SMALL_ICON, new ImageIcon(iconURL));
      }
      else {
        putValue(Action.NAME, "Step");
      }
    }

    public void actionPerformed(ActionEvent e) {
      step();
    }
  }
  public class UndoAction extends AbstractAction {
    private static final long serialVersionUID = 1L;

    public UndoAction() {
      URL iconURL = getClass().getResource(UNDO_ICON);
      if (iconURL != null) {
        putValue(Action.SMALL_ICON, new ImageIcon(iconURL));
      }
      else {
        putValue(Action.NAME, "Undo");
      }
    }

    public void actionPerformed(ActionEvent e) {
      undo();
    }
  }
}
