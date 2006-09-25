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
import java.io.File;
import java.net.MalformedURLException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.KeyStroke;

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
import VASSAL.tools.LaunchButton;

/**
 * This is a {@link GameComponent} that allows players to type and
 * save text notes during a game.  There is one set of shared public
 * notes, and each player has a set of private notes visible only to
 * him
 */
public class NotesWindow extends AbstractConfigurable
    implements GameComponent, CommandEncoder {

  private JDialog frame;
  private LaunchButton launch;
  private TextConfigurer notes;
  private PrivateNotesController privateNotes;
  private SecretNotesController secretNotes;
  private static final String COMMAND_PREFIX = "NOTES\t";

  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";

  private String lastSavedNotes;

  public NotesWindow() {
    privateNotes = new PrivateNotesController();
    secretNotes = new SecretNotesController();
    frame = new NotesDialog();
    frame.setTitle("Notes");
    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        captureState();
        frame.setVisible(!frame.isShowing());
      }
    };
    launch = new LaunchButton("Notes", BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setAttribute(ICON, "/images/notes.gif");
    launch.setToolTipText("Notes");
    frame.pack();
    setup(false);
  }

  /**
   * Capture this object's state, to be restored if the user hits "Cancel"
   */
  protected void captureState() {
    lastSavedNotes = (String) notes.getValue();
    privateNotes.captureState();
    secretNotes.captureState();
  }

  public void cancel() {
    restoreState();
    privateNotes.restoreState();
    secretNotes.restoreState();
  }

  protected void restoreState() {
    notes.setValue(lastSavedNotes);
  }

  protected void save() {
    Command c = new NullCommand();
    if (!lastSavedNotes.equals(notes.getValue())) {
      c.append(new SetNote(notes.getValueString()));
    }
    c.append(privateNotes.save());
    c.append(secretNotes.save());
    GameModule.getGameModule().sendAndLog(c);
  }

  private class NotesDialog extends JDialog {
    private NotesDialog() {
      super(GameModule.getGameModule().getFrame());
      initComponents();
      setLocationRelativeTo(getOwner());
    }

    private void initComponents() {
      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          cancel();
          setVisible(false);
        }
      });

      notes = new TextConfigurer(null, null);
      JTabbedPane tab = new JTabbedPane();
      getContentPane().add(tab);

      Box b = Box.createVerticalBox();
      b.add(new JLabel("Visible to all"));
      b.add(notes.getControls());
      tab.addTab("Public", b);

      tab.addTab("Private", privateNotes.getControls());

      tab.addTab("Delayed", secretNotes.getControls());

      JPanel p = new JPanel();
      JButton saveButton = new JButton("Save");
      p.add(saveButton);
      saveButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          save();
          setVisible(false);
        }
      });
      JButton cancelButton = new JButton("Cancel");
      cancelButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          cancel();
          setVisible(false);
        }
      });
      p.add(cancelButton);
      getContentPane().add(p);
    }
  }


  public HelpFile getHelpFile() {
    File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
    dir = new File(dir, "ReferenceManual");
    try {
      return new HelpFile(null, new File(dir, "GameModule.htm"), "#NotesWindow");
    }
    catch (MalformedURLException ex) {
      return null;
    }
  }

  public String[] getAttributeNames() {
    return new String[] {BUTTON_TEXT, ICON, HOT_KEY};
  }

  public void setAttribute(String name, Object value) {
    launch.setAttribute(name, value);
  }

  public String getAttributeValueString(String name) {
    return launch.getAttributeValueString(name);
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof SetNote) {
      s = COMMAND_PREFIX + ((SetNote) c).msg;
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
    if (command.startsWith(COMMAND_PREFIX)) {
      comm = new SetNote(command.substring(COMMAND_PREFIX.length()));
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
    return new String[] {"Button text", "Button Icon", "Hotkey"};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, IconConfig.class, KeyStroke.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((NotesWindow) c).launch.getAttributeValueString(ICON));
    }
  }

  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public static String getConfigureTypeName() {
    return "Notes Window";
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
      notes.setValue("");
    }
  }

  public Command getRestoreCommand() {
    Command c = new SetNote(notes.getValueString());
    c.append(privateNotes.getRestoreCommand());
    c.append(secretNotes.getRestoreCommand());
    return c;
  }

  private class SetNote extends Command {
    private String msg;

    private SetNote(String s) {
      msg = s;
    }

    protected void executeCommand() {
      notes.setValue(msg);
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
