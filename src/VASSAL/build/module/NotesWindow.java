/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.noteswindow.PrivateNotesController;
import VASSAL.build.module.noteswindow.SecretNotesController;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;

/**
 * This is a {@link GameComponent} that allows players to type and
 * save text notes during a game.  There is one set of shared public
 * notes, and each player has a set of private notes visible only to
 * him
 */
public class NotesWindow extends AbstractConfigurable
    implements GameComponent, CommandEncoder {

  protected JDialog frame;
  protected LaunchButton launch;
  protected TextConfigurer scenarioNotes;
  protected TextConfigurer publicNotes;
  protected PrivateNotesController privateNotes;
  protected SecretNotesController secretNotes;
  protected static final String SCENARIO_NOTE_COMMAND_PREFIX = "NOTES\t"; //$NON-NLS-1$
  protected static final String PUBLIC_NOTE_COMMAND_PREFIX = "PNOTES\t"; //$NON-NLS-1$

  public static final String HOT_KEY = "hotkey"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "buttonText"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$

  protected String lastSavedScenarioNotes;
  protected String lastSavedPublicNotes;

  public NotesWindow() {
    privateNotes = new PrivateNotesController();
    secretNotes = new SecretNotesController();
    frame = new NotesDialog();
    frame.setTitle(Resources.getString("Notes.notes")); //$NON-NLS-1$
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        captureState();
        frame.setVisible(!frame.isShowing());
      }
    };
    launch = new LaunchButton(Resources.getString("Notes.notes"), TOOLTIP, BUTTON_TEXT, HOT_KEY, ICON, al); //$NON-NLS-1$
    launch.setAttribute(ICON, "/images/notes.gif"); //$NON-NLS-1$
    launch.setToolTipText(Resources.getString("Notes.notes")); //$NON-NLS-1$
    frame.pack();
    setup(false);
  }

  /**
   * Capture this object's state, to be restored if the user hits "Cancel"
   */
  protected void captureState() {
    lastSavedScenarioNotes = (String) scenarioNotes.getValue();
    lastSavedPublicNotes = (String) publicNotes.getValue();
    privateNotes.captureState();
    secretNotes.captureState();
  }

  public void cancel() {
    restoreState();
    privateNotes.restoreState();
    secretNotes.restoreState();
  }

  protected void restoreState() {
    scenarioNotes.setValue(lastSavedScenarioNotes);
    publicNotes.setValue(lastSavedPublicNotes);
  }

  protected void save() {
    Command c = new NullCommand();
    if (!lastSavedScenarioNotes.equals(scenarioNotes.getValue())) {
      c.append(new SetScenarioNote(scenarioNotes.getValueString()));
    }
    if (!lastSavedPublicNotes.equals(publicNotes.getValue())) {
      c.append(new SetPublicNote(publicNotes.getValueString()));
    }
    c.append(privateNotes.save());
    c.append(secretNotes.save());
    GameModule.getGameModule().sendAndLog(c);
  }

  protected class NotesDialog extends JDialog {

    private static final long serialVersionUID = 1L;

    protected NotesDialog() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
      setLocationRelativeTo(getOwner());
    }

    protected void initComponents() {
      setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          cancel();
          setVisible(false);
        }
      });

      scenarioNotes = new TextConfigurer(null, null);
      publicNotes = new TextConfigurer(null, null);
      JTabbedPane tab = new JTabbedPane();
      add(tab);

      Box b = Box.createVerticalBox();
      b.add(new JLabel(Resources.getString("Notes.visible_to_all"))); //$NON-NLS-1$
      b.add(scenarioNotes.getControls());
      tab.addTab(Resources.getString("Notes.scenario"), b); //$NON-NLS-1$

      b = Box.createVerticalBox();
      b.add(new JLabel(Resources.getString("Notes.visible_to_all"))); //$NON-NLS-1$
      b.add(publicNotes.getControls());
      tab.addTab(Resources.getString("Notes.public"), b); //$NON-NLS-1$

      tab.addTab(Resources.getString("Notes.private"), privateNotes.getControls()); //$NON-NLS-1$

      tab.addTab(Resources.getString("Notes.delayed"), secretNotes.getControls()); //$NON-NLS-1$

      JPanel p = new JPanel();
      JButton saveButton = new JButton(Resources.getString(Resources.SAVE));
      p.add(saveButton);
      saveButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          save();
          setVisible(false);
        }
      });
      JButton cancelButton = new JButton(Resources.getString(Resources.CANCEL));
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          cancel();
          setVisible(false);
        }
      });
      p.add(cancelButton);
      add(p);
    }
  }


  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm", "NotesWindow"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public String[] getAttributeNames() {
    return new String[] {BUTTON_TEXT, TOOLTIP, ICON, HOT_KEY};
  }

  public void setAttribute(String name, Object value) {
    launch.setAttribute(name, value);
  }

  public String getAttributeValueString(String name) {
    return launch.getAttributeValueString(name);
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetScenarioNote) {
      s = SCENARIO_NOTE_COMMAND_PREFIX + ((SetScenarioNote) c).msg;
    }
    else if (c instanceof SetPublicNote) {
      s = PUBLIC_NOTE_COMMAND_PREFIX + ((SetPublicNote) c).msg;
    }
    else {
      s = privateNotes.encode(c);
      if (s == null) {
        s = secretNotes.encode(c);
      }
    }
    return s;
  }

  public Command decode(String command) {
    Command comm;
    if (command.startsWith(SCENARIO_NOTE_COMMAND_PREFIX)) {
      comm = new SetScenarioNote(command.substring(SCENARIO_NOTE_COMMAND_PREFIX.length()));
    }
    else if (command.startsWith(PUBLIC_NOTE_COMMAND_PREFIX)) {
      comm = new SetPublicNote(command.substring(PUBLIC_NOTE_COMMAND_PREFIX.length()));
    }
    else {
      comm = privateNotes.decode(command);
      if (comm == null) {
        comm = secretNotes.decode(command);
      }
    }
    return comm;
  }

  public String[] getAttributeDescriptions() {
    return new String[] {
        Resources.getString(Resources.BUTTON_TEXT),
          Resources.getString(Resources.TOOLTIP_TEXT),
            Resources.getString(Resources.BUTTON_ICON),
            Resources.getString(Resources.HOTKEY_LABEL)
    };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((NotesWindow) c).launch.getAttributeValueString(ICON));
    }
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.NotesWindow.component_type"); //$NON-NLS-1$
  }

  /**
   * Expects to be added to a {@link VASSAL.build.GameModule}.  Adds a button to
   * the controls window toolbar to show the window containing the
   * notes */
  public void addTo(Buildable b) {
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(privateNotes);
    GameModule.getGameModule().getGameState().addGameComponent(privateNotes);
    GameModule.getGameModule().addCommandEncoder(secretNotes);
    GameModule.getGameModule().getGameState().addGameComponent(secretNotes);
  }

  public void removeFrom(Buildable b) {
    GameModule.getGameModule().getToolBar().remove(launch);
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().removeCommandEncoder(privateNotes);
    GameModule.getGameModule().getGameState().removeGameComponent(privateNotes);
    GameModule.getGameModule().removeCommandEncoder(secretNotes);
    GameModule.getGameModule().getGameState().removeGameComponent(secretNotes);
  }

  public void setup(boolean show) {
    launch.setEnabled(show);
    if (!show) {
      scenarioNotes.setValue(""); //$NON-NLS-1$
      publicNotes.setValue(""); //$NON-NLS-1$
    }
  }

  public Command getRestoreCommand() {
    Command c = new SetScenarioNote(scenarioNotes.getValueString());
    c.append(new SetPublicNote(publicNotes.getValueString()));
    c.append(privateNotes.getRestoreCommand());
    c.append(secretNotes.getRestoreCommand());
    return c;
  }

  protected class SetScenarioNote extends Command {
    protected String msg;

    protected SetScenarioNote(String s) {
      msg = s;
    }

    protected void executeCommand() {
      scenarioNotes.setValue(msg);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }

  protected class SetPublicNote extends Command {
    protected String msg;

    protected SetPublicNote(String s) {
      msg = s;
    }

    protected void executeCommand() {
      publicNotes.setValue(msg);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
