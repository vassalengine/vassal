/*
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

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import VASSAL.build.AbstractToolbarItem;
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

/**
 * This is a {@link GameComponent} that allows players to type and
 * save text notes during a game.  There is one set of shared public
 * notes, and each player has a set of private notes visible only to
 * him
 */
public class NotesWindow extends AbstractToolbarItem
    implements GameComponent, CommandEncoder {

  public static final String BUTTON_TEXT = "buttonText"; //NON-NLS // non-standard legacy difference from AbstractToolbarItem

  // These three identical to AbstractToolbarItem, and are only here for "clirr purposes"
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String HOT_KEY = "hotkey"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String ICON = "icon"; //$NON-NLS-1$
  @Deprecated (since = "2020-10-21", forRemoval = true) public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$

  protected JDialog frame;

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected TextConfigurer scenarioNotes;
  protected TextConfigurer publicNotes;
  protected PrivateNotesController privateNotes;
  protected SecretNotesController secretNotes;
  protected static final String SCENARIO_NOTE_COMMAND_PREFIX = "NOTES\t"; //$NON-NLS-1$
  protected static final String PUBLIC_NOTE_COMMAND_PREFIX = "PNOTES\t"; //$NON-NLS-1$

  protected String lastSavedScenarioNotes;
  protected String lastSavedPublicNotes;

  public NotesWindow() {
    privateNotes = new PrivateNotesController();
    secretNotes = new SecretNotesController();
    frame = new NotesDialog();
    frame.setTitle(Resources.getString("Notes.notes")); //$NON-NLS-1$
    setNameKey("");                // No description or name configured
    setButtonTextKey(BUTTON_TEXT); // Legacy different button text key

    setLaunchButton(makeLaunchButton(
      Resources.getString("Notes.notes"),
      Resources.getString("Notes.notes"),
      "/images/notes.gif", //NON-NLS
      e -> {
        captureState();
        frame.setVisible(!frame.isShowing());
      }
    ));
    launch = getLaunchButton();

    frame.pack();
    setup(false);
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((NotesWindow) c).launch.getAttributeValueString(ICON));
    }
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
    final Command c = new NullCommand();
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
      super(GameModule.getGameModule().getPlayerWindow());
      initComponents();
      setLocationRelativeTo(getOwner());
    }

    protected void initComponents() {
      setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          cancel();
          setVisible(false);
        }
      });

      scenarioNotes = new TextConfigurer(null, null);
      publicNotes = new TextConfigurer(null, null);
      final JTabbedPane tab = new JTabbedPane();
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

      final JPanel p = new JPanel();
      final JButton saveButton = new JButton(Resources.getString(Resources.SAVE));
      p.add(saveButton);
      saveButton.addActionListener(e -> {
        save();
        setVisible(false);
      });
      final JButton cancelButton = new JButton(Resources.getString(Resources.CANCEL));
      cancelButton.addActionListener(e -> {
        cancel();
        setVisible(false);
      });
      p.add(cancelButton);
      add(p);
    }
  }


  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.html", "NotesWindow"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void setAttribute(String name, Object value) {
    getLaunchButton().setAttribute(name, value);
  }

  @Override
  public String getAttributeValueString(String name) {
    return getLaunchButton().getAttributeValueString(name);
  }

  @Override
  public String encode(Command c) {
    if (c instanceof SetScenarioNote) {
      return SCENARIO_NOTE_COMMAND_PREFIX + ((SetScenarioNote) c).msg;
    }

    if (c instanceof SetPublicNote) {
      return PUBLIC_NOTE_COMMAND_PREFIX + ((SetPublicNote) c).msg;
    }

    String s = privateNotes.encode(c);
    if (s == null) {
      s = secretNotes.encode(c);
    }
    return s;
  }

  @Override
  public Command decode(String command) {
    if (command.startsWith(SCENARIO_NOTE_COMMAND_PREFIX)) {
      return new SetScenarioNote(command.substring(SCENARIO_NOTE_COMMAND_PREFIX.length()));
    }

    if (command.startsWith(PUBLIC_NOTE_COMMAND_PREFIX)) {
      return new SetPublicNote(command.substring(PUBLIC_NOTE_COMMAND_PREFIX.length()));
    }

    Command comm = privateNotes.decode(command);
    if (comm == null) {
      comm = secretNotes.decode(command);
    }
    return comm;
  }


  @Override
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  @Override
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
  @Override
  public void addTo(Buildable b) {
    super.addTo(b);
    getLaunchButton().setAlignmentY(0.0F);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(privateNotes);
    GameModule.getGameModule().getGameState().addGameComponent(privateNotes);
    GameModule.getGameModule().addCommandEncoder(secretNotes);
    GameModule.getGameModule().getGameState().addGameComponent(secretNotes);
  }

  @Override
  public void removeFrom(Buildable b) {
    super.removeFrom(b);
    GameModule.getGameModule().removeCommandEncoder(this);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    GameModule.getGameModule().removeCommandEncoder(privateNotes);
    GameModule.getGameModule().getGameState().removeGameComponent(privateNotes);
    GameModule.getGameModule().removeCommandEncoder(secretNotes);
    GameModule.getGameModule().getGameState().removeGameComponent(secretNotes);
  }

  @Override
  public void setup(boolean show) {
    getLaunchButton().setEnabled(show);
    if (!show) {
      scenarioNotes.setValue(""); //$NON-NLS-1$
      publicNotes.setValue(""); //$NON-NLS-1$
    }
  }

  @Override
  public Command getRestoreCommand() {
    final Command c = new SetScenarioNote(scenarioNotes.getValueString());
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

    @Override
    protected void executeCommand() {
      scenarioNotes.setValue(msg);
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }

  protected class SetPublicNote extends Command {
    protected String msg;

    protected SetPublicNote(String s) {
      msg = s;
    }

    @Override
    protected void executeCommand() {
      publicNotes.setValue(msg);
    }

    @Override
    protected Command myUndoCommand() {
      return null;
    }
  }
}
