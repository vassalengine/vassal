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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.Enumeration;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;

import org.w3c.dom.Document;

import VASSAL.Info;
import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;

/**
 * An optional extension to a GameModule
 * Like a GameModule, it is built from scratch from a 'buildFile' in a DataArchive
 * The components described in the buildFile are appended to components in the base DataArchive
 */
public class ModuleExtension extends AbstractBuildable implements GameComponent {
  public static final String BASE_MODULE_NAME = "module";
  public static final String BASE_MODULE_VERSION = "moduleVersion";
  public static final String VERSION = "version";
  public static final String VASSAL_VERSION_CREATED = "vassalVersion";

  private DataArchive archive;
  private String version = "0.0";

  private String lastSave;
  private String vassalVersionCreated;
  private AbstractAction editAction;

  public ModuleExtension(DataArchive archive) {
    this.archive = archive;
  }

  public String getVersion() {
    return version;
  }

  public DataArchive getDataArchive() {
    return archive;
  }

  public void build() {
    String fileName = "buildFile";

    InputStream inStream = null;
    if (archive != null) {
      try {
        inStream = archive.getFileStream(fileName);
      }
      catch (IOException ex) {
      }
    }
    GameModule.getGameModule().getDataArchive().addExtension(archive);
    try {
      if (inStream == null) {
        build(null);
      }
      else {
        Document doc = Builder.createDocument(inStream);
        build(doc.getDocumentElement());
      }
    }
    catch (IOException ex) {
      throw new IllegalBuildException(ex.getMessage());
    }
    GameModule.getGameModule().add(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    if (archive instanceof ArchiveWriter) {
      lastSave = buildString();
    }
  }

  public Command getRestoreCommand() {
    return new RegCmd(getName(), version);
  }

  public void setup(boolean gameStarting) {
  }

  public String[] getAttributeNames() {
    return new String[]{VERSION, BASE_MODULE_NAME, BASE_MODULE_VERSION, VASSAL_VERSION_CREATED};
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
    throw new IllegalBuildException("Cannot remove Extension");
  }

  public boolean confirmExit() {
    boolean confirm = true;
    if (archive instanceof ArchiveWriter
        && !buildString().equals(lastSave)) {
      switch (JOptionPane.showConfirmDialog
          (GameModule.getGameModule().getFrame(), "Save Extension?",
           "", JOptionPane.YES_NO_CANCEL_OPTION)) {
        case JOptionPane.YES_OPTION:
          try {
            save();
          }
          catch (IOException e) {
            confirm = false;
          }
          break;
        case JOptionPane.CANCEL_OPTION:
          confirm = false;
      }

    }
    return confirm;
  }

  public String getAttributeValueString(String key) {
    String s = null;
    if (BASE_MODULE_NAME.equals(key)) {
      s = GameModule.getGameModule().getGameName();
    }
    else if (BASE_MODULE_VERSION.equals(key)) {
      s = GameModule.getGameModule().getGameVersion();
    }
    else if (VERSION.equals(key)) {
      s = version;
    }
    else if (VASSAL_VERSION_CREATED.equals(key)) {
      s = vassalVersionCreated;
    }
    return s;
  }

  public void setAttribute(String key, Object value) {
    if (BASE_MODULE_NAME.equals(key)) {
      if (!GameModule.getGameModule().getGameName().equals(value)) {
        throw new IllegalBuildException("Extension \'" + getName() + "\' was built for module \'" + value + "\'");
      }
    }
    else if (BASE_MODULE_VERSION.equals(key)) {
      String version = (String) value;
      if (Info.compareVersions(GameModule.getGameModule().getGameVersion(), version) < 0) {
        GameModule.getGameModule().warn("Extension \'" + getName() + "\' was built for module version "
                                      + version + ".  You are running version " + GameModule.getGameModule().getGameVersion()
                                      + ".  It's recommended you upgrade to the latest version of " + GameModule.getGameModule().getGameName());
      }
    }
    else if (VASSAL_VERSION_CREATED.equals(key)) {
      vassalVersionCreated = (String) value;
      String runningVersion = Info.getVersion();
      if (Info.compareVersions(vassalVersionCreated, runningVersion) > 0) {
        GameModule.getGameModule().warn("Extension "+getName()+" was created using version " + value
             + " of the VASSAL engine\nYou are using version "
             + runningVersion + ".  It's recommended you upgrade to the latest version of the VASSAL engine.");
      }
    }
    else if (VERSION.equals(key)) {
      version = (String) value;
    }
  }

  public void addTo(Buildable parent) {
  }

  public String getName() {
    String name = "Extension";
    if (archive != null) {
      name = archive.getName();
      int index = name.lastIndexOf(File.separatorChar);
      if (index < name.length()) {
        name = name.substring(index + 1);
      }
      index = name.lastIndexOf('.');
      if (index > 0) {
        name = name.substring(0, index);
      }
    }
    return name;
  }

  public String buildString() {
    org.w3c.dom.Document doc = Builder.createNewDocument();
    doc.appendChild(getBuildElement(doc));
    return Builder.toString(doc);
  }

  public void save() throws IOException {
    vassalVersionCreated = Info.getVersion();
    if (archive instanceof ArchiveWriter) {
      String save = buildString();
      ((ArchiveWriter) archive).addFile
          ("buildFile",
           new java.io.ByteArrayInputStream(save.getBytes("UTF-8")));
      ((ArchiveWriter) archive).write();
      lastSave = save;
    }
    else {
      throw new IOException("Read-only extension");
    }
  }

  public void saveAs() throws IOException {
    vassalVersionCreated = Info.getVersion();
    if (archive instanceof ArchiveWriter) {
      String save = buildString();
      ((ArchiveWriter) archive).addFile
          ("buildFile",
           new java.io.ByteArrayInputStream(save.getBytes("UTF-8")));
      ((ArchiveWriter) archive).saveAs();
      lastSave = save;
    }
    else {
      throw new IOException("Read-only extension");
    }
  }

  public void remove(ExtensionElement el) {
    buildComponents.remove(el);
  }

  public Action getEditAction(final JDialog d) {
    if (editAction == null) {
      d.setName(getName());
      final StringConfigurer config = new StringConfigurer(VERSION, "Version", version);
      d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
      d.getContentPane().add(config.getControls());
      Box b = Box.createHorizontalBox();
      JButton ok = new JButton("Save");
      ok.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setAttribute(VERSION, config.getValue());
          d.dispose();
        }
      });
      b.add(ok);
      JButton cancel = new JButton("Cancel");
      cancel.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          d.dispose();
        }
      });
      b.add(cancel);
      d.getContentPane().add(b);
      d.pack();
      d.setLocationRelativeTo(d.getParent());
      editAction = new AbstractAction() {
        public void actionPerformed(ActionEvent e) {
          d.setVisible(true);
        }
      };
      URL iconURL = getClass().getResource("/images/Edit16.gif");
      if (iconURL != null) {
        editAction.putValue(Action.SMALL_ICON, new ImageIcon(iconURL));
      }
      else {
        editAction.putValue(Action.NAME, "Edit");
      }
      editAction.putValue(Action.SHORT_DESCRIPTION, "Extension Properties");
    }
    return editAction;
  }

  /**
   * A command that verifies that a certain extension has been loaded
   */
  public static class RegCmd extends Command {
    private String name;
    private String version;

    public RegCmd(String name, String version) {
      this.name = name;
      this.version = version;
    }

    public String getName() {
      return name;
    }

    public String getVersion() {
      return version;
    }

    protected void executeCommand() {
      boolean containsExtension = false;
      String msg = null;
      Enumeration e = GameModule.getGameModule().getComponents(ModuleExtension.class);
      while (e.hasMoreElements()) {
        final ModuleExtension ext = (ModuleExtension) e.nextElement();
        if (ext.getName().equals(name)) {
          containsExtension = true;
          if (Info.compareVersions(ext.getVersion(), version) < 0) {
            msg = "Game saved with version " + version + " of extension \'" + name
                         + "\', you are running version " + ext.getVersion() +
                         ". Please upgrade to the latest version of this extension.";
            GameModule.getGameModule().warn(msg);
          }
          break;
        }
      }
      if (!containsExtension) {
        msg = "This game was saved with extension \'" + name + "\' loaded. You do not have this extension loaded."
                                             + "Place the file into the \'" + ExtensionsLoader.getExtensionDirectory() + "\' folder to load it.";
        GameModule.getGameModule().warn(msg);
      }
    }

    protected Command myUndoCommand() {
      return null;
    }
  }
}
