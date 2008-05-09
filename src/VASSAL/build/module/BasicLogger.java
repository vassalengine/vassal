/*
 * $Id$
 *
 * Copyright (c) 2000 by Rodney Kinney
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available 
 * at http://www.opensource.org.
 */

package VASSAL.build.module;

import java.awt.Event;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
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

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.Logger;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IconConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.launch.Launcher;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.BridgeStream;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.Obfuscator;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.menu.MenuManager;

public class BasicLogger implements Logger, Buildable, GameComponent, CommandEncoder {
  public static final String BEGIN = "begin_log";  //$NON-NLS-1$
  public static final String END = "end_log";  //$NON-NLS-1$
  public static final String LOG = "LOG\t";  //$NON-NLS-1$
  public static final String PROMPT_NEW_LOG = "PromptNewLog";  //$NON-NLS-1$
  public static final String PROMPT_NEW_LOG_START = "PromptNewLogStart";  //$NON-NLS-1$
  public static final String PROMPT_NEW_LOG_END = "PromptNewLogEnd";  //$NON-NLS-1$
  protected static final String STEP_ICON = "/images/StepForward16.gif";  //$NON-NLS-1$
  protected static final String UNDO_ICON = "/images/Undo16.gif";  //$NON-NLS-1$
  protected List<Command> logInput;
  protected List<Command> logOutput;
  protected int nextInput = 0;
  protected int nextUndo = -1;
  protected Command beginningState;
  protected File outputFile;
  protected Action stepAction = new StepAction();
  protected SaveMetaData metadata;

  public BasicLogger() {
    super();
    stepAction.setEnabled(false);
    undoAction.setEnabled(false);
    endLogAction.setEnabled(false);
    newLogAction.setEnabled(false);
    logInput = new ArrayList<Command>();
    logOutput = new ArrayList<Command>();
  }

  public void build(Element e) { }

  /**
   * Expects to be added to a {@link GameModule}. Adds <code>Undo</code>,
   * <code>Step</code>, and <code>End Log</code> buttons to the the control
   * window toolbar. Registers {@link KeyStrokeListener}s for hotkey
   * equivalents of each button.
   */
  public void addTo(Buildable b) {
    final GameModule mod = GameModule.getGameModule();

    mod.addCommandEncoder(this);
    mod.getGameState().addGameComponent(this);

    final MenuManager mm = MenuManager.getInstance();
    mm.addAction("BasicLogger.begin_logfile", newLogAction);
    mm.addAction("BasicLogger.end_logfile", endLogAction);

    JButton button = mod.getToolBar().add(undoAction);
    button.setToolTipText(Resources.getString("BasicLogger.undo_last_move"));  //$NON-NLS-1$
    button.setAlignmentY((float) 0.0);

    button = mod.getToolBar().add(stepAction);
    button.setToolTipText(Resources.getString("BasicLogger.step_forward_tooltip"));  //$NON-NLS-1$
    button.setAlignmentY((float) 0.0);

    final KeyStrokeListener stepKeyListener = new KeyStrokeListener(stepAction, KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, 0));
    mod.addKeyStrokeListener(stepKeyListener);

    final KeyStrokeListener newLogKeyListener = new KeyStrokeListener(newLogAction, KeyStroke.getKeyStroke(KeyEvent.VK_W, Event.ALT_MASK));
    mod.addKeyStrokeListener(newLogKeyListener);

    mod.getFrame().addComponentListener(new ComponentAdapter() {
      public void componentShown(ComponentEvent e) {
        GameModule.getGameModule().getFrame().removeComponentListener(this);
        final IconConfigurer stepIconConfig = new IconConfigurer("stepIcon", Resources.getString("BasicLogger.step_forward_button"), STEP_ICON); //$NON-NLS-1$ //$NON-NLS-2$
        stepIconConfig.setValue(STEP_ICON);
        GlobalOptions.getInstance().addOption(stepIconConfig);
        stepIconConfig.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            stepAction.putValue(Action.SMALL_ICON, stepIconConfig.getIconValue());
          }
        });
        stepIconConfig.fireUpdate();
        final IconConfigurer undoIconConfig = new IconConfigurer("undoIcon", Resources.getString("BasicLogger.undo_icon"), UNDO_ICON);  //$NON-NLS-1$ //$NON-NLS-2$
        undoIconConfig.setValue(UNDO_ICON);
        GlobalOptions.getInstance().addOption(undoIconConfig);
        undoIconConfig.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            undoAction.putValue(Action.SMALL_ICON, undoIconConfig.getIconValue());
          }
        });
        undoIconConfig.fireUpdate();
        final HotKeyConfigurer stepKeyConfig = new HotKeyConfigurer("stepHotKey", Resources.getString("BasicLogger.step_forward_hotkey"), stepKeyListener.getKeyStroke());  //$NON-NLS-1$ //$NON-NLS-2$
        GlobalOptions.getInstance().addOption(stepKeyConfig);
        stepKeyConfig.addPropertyChangeListener(new PropertyChangeListener() {
          public void propertyChange(PropertyChangeEvent evt) {
            stepKeyListener.setKeyStroke((KeyStroke) stepKeyConfig.getValue());
            stepAction.putValue(Action.SHORT_DESCRIPTION, Resources.getString("BasicLogger.step_forward_tooltip2", HotKeyConfigurer.getString(stepKeyListener.getKeyStroke())));  //$NON-NLS-1$
          }
        });
        stepKeyConfig.fireUpdate();
      }
    });

    BooleanConfigurer logOptionStart = new BooleanConfigurer(PROMPT_NEW_LOG_START, Resources.getString("BasicLogger.prompt_new_log_before"), Boolean.FALSE);  //$NON-NLS-1$
    mod.getPrefs().addOption(Resources.getString("Prefs.general_tab"), logOptionStart); //$NON-NLS-1$

    BooleanConfigurer logOptionEnd = new BooleanConfigurer(PROMPT_NEW_LOG_END, Resources.getString("BasicLogger.prompt_new_log_after"), Boolean.TRUE);  //$NON-NLS-1$
    mod.getPrefs().addOption(Resources.getString("Prefs.general_tab"), logOptionEnd); //$NON-NLS-1$
  }

  public Element getBuildElement(Document doc) {
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
    }
    else {
      if (endLogAction.isEnabled()) {
        if (JOptionPane.showConfirmDialog(GameModule.getGameModule().getFrame(), Resources.getString("BasicLogger.save_log"), Resources.getString("BasicLogger.unsaved_log"),  //$NON-NLS-1$ //$NON-NLS-2$
            JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION) {
          try {
            write();
          }
          catch (IOException ex) {
            String msg = Resources.getString("BasicLogger.unable_to_write", outputFile.toString());  //$NON-NLS-1$
            if (ex.getMessage() != null) {
              msg += ".\n" + ex.getMessage(); //$NON-NLS-1$
            }
            JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), msg, Resources.getString("BasicLogger.save_failed"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$
          }
        }
      }
      logInput.clear();
      beginningState = null;
      undoAction.setEnabled(false);
      endLogAction.setEnabled(false);
      stepAction.setEnabled(false);
      outputFile = null;
    }
  }

  public boolean isLogging() {
    return outputFile != null;
  }
  
  public Command getRestoreCommand() {
    return null;
  }

  public void enableDrawing(boolean show) {
  }

  protected void step() {
    Command c = logInput.get(nextInput++);
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
    if (isLogging()) {
      return;
    }

    if (atStart) {
      prefName = PROMPT_NEW_LOG_START;
      prompt = Resources.getString("BasicLogger.replay_commencing");  //$NON-NLS-1$
    }
    else {
      prefName = PROMPT_NEW_LOG_END;
      prompt = Resources.getString("BasicLogger.replay_completed");  //$NON-NLS-1$
    }
    if (((Boolean) GameModule.getGameModule().getPrefs().getValue(prefName)).booleanValue()) {
      Object[] options = {Resources.getString(Resources.YES), Resources.getString(Resources.NO), Resources.getString("BasicLogger.dont_prompt_again")};  //$NON-NLS-1$
      int result = JOptionPane.showOptionDialog(GameModule.getGameModule().getFrame(),
          Resources.getString("BasicLogger.start_new_log_file", prompt), //$NON-NLS-1$
          "",  //$NON-NLS-1$
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
    if (!logOutput.isEmpty()) {
      final Command log = beginningState;
      for (Command c : logOutput) {
        log.append(new LogCommand(c, logInput, stepAction));
      }

      final String s = GameModule.getGameModule().encode(log);
      final BridgeStream out = new BridgeStream();
      new Obfuscator(s.getBytes("UTF-8")).write(out); //$NON-NLS-1$    

      final ArchiveWriter saver = new ArchiveWriter(outputFile.getPath());
      saver.addFile(GameState.SAVEFILE_ZIP_ENTRY, out.toInputStream()); //$NON-NLS-1$
      metadata.save(saver);
      saver.write();
      Launcher.getInstance().sendSaveCmd(outputFile);

      GameModule.getGameModule().getGameState().setModified(false);
      undoAction.setEnabled(false);
      endLogAction.setEnabled(false);
    }
  }
  
  protected void beginOutput() {
    FileChooser fd = GameModule.getGameModule().getFileChooser();
    String name = fd.getSelectedFile() == null ? null : fd.getSelectedFile().getName();
    if (name != null) {
      int index = name.lastIndexOf('.');
      if (index > 0) {
        name = name.substring(0, index) + ".log";  //$NON-NLS-1$
        fd.setSelectedFile(new File(fd.getSelectedFile().getParentFile(), name));
      }
    }
    if (fd.showSaveDialog() == FileChooser.APPROVE_OPTION) {
      outputFile = fd.getSelectedFile();
      logOutput.clear();
      beginningState = GameModule.getGameModule().getGameState().getRestoreCommand();
      undoAction.setEnabled(false);
      endLogAction.setEnabled(true);
      GameModule.getGameModule().appendToTitle(Resources.getString("BasicLogger.logging_to", outputFile.getName()));  //$NON-NLS-1$
      newLogAction.setEnabled(false);
      metadata = new SaveMetaData();
    }
  }

  protected void undo() {
    Command lastOutput = logOutput.get(nextUndo);
    Command lastInput = (nextInput > logInput.size() || nextInput < 1) ?
      null : logInput.get(nextInput - 1);
    if (lastInput == lastOutput) {
      while (nextInput-- > 0) {
        stepAction.setEnabled(true);
        if (logInput.get(nextInput).getUndoCommand() != null) {
          break;
        }
      }
    }
    while (nextUndo-- > 0) {
      if (logOutput.get(nextUndo).getUndoCommand() != null) {
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

  protected Action endLogAction = new AbstractAction(Resources.getString("BasicLogger.end_logfile")) {  //$NON-NLS-1$
    private static final long serialVersionUID = 1L;

    public void actionPerformed(ActionEvent e) {
      try {
        write();
        GameModule.getGameModule().warn(Resources.getString("BasicLogger.logfile_written"));  //$NON-NLS-1$
        newLogAction.setEnabled(true);
        GameModule.getGameModule().appendToTitle(null);
        outputFile = null;
      }
      catch (IOException ex) {
        String msg = Resources.getString("BasicLogger.unable_to_write", outputFile.toString());  //$NON-NLS-1$
        if (ex.getMessage() != null) {
          msg += ".\n" + ex.getMessage(); //$NON-NLS-1$
        }
        JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), msg, Resources.getString("BasicLogger.save_failed"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$
      }
    }
  };

  protected Action newLogAction = new AbstractAction(Resources.getString("BasicLogger.begin_logfile")) {  //$NON-NLS-1$
    private static final long serialVersionUID = 1L;

    public void actionPerformed(ActionEvent e) {
      beginOutput();
    }
  };

  public static class LogCommand extends Command {
    protected Command logged;
    protected List<Command> logInput;
    protected Action stepAction;

    public LogCommand(Command c, List<Command> logInput, Action stepAction) {
      if (c instanceof LogCommand) {
        throw new RuntimeException(Resources.getString("BasicLogger.cant_log"));  //$NON-NLS-1$
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
        putValue(Action.NAME, Resources.getString("BasicLogger.step"));  //$NON-NLS-1$
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
        putValue(Action.NAME, Resources.getString("BasicLogger.undo"));  //$NON-NLS-1$
      }
    }

    public void actionPerformed(ActionEvent e) {
      undo();
    }
  }
}
