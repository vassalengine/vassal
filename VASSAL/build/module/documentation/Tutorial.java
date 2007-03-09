/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.build.module.documentation;

import java.awt.event.ActionEvent;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;

/**
 * Provides tutorial functionality by reading in a logfile
 */
public class Tutorial extends AbstractConfigurable {
  public static final String FILE_NAME = "logfile"; //$NON-NLS-1$
  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String LAUNCH_ON_STARTUP = "launchOnStartup"; //$NON-NLS-1$
  public static final String PROMPT_MESSAGE = "promptMessage"; //$NON-NLS-1$
  public static final String WELCOME_MESSAGE = "welcomeMessage"; //$NON-NLS-1$
  private String fileName;
  private Action launch;
  private JMenuItem item;
  private boolean launchOnFirstStartup;
  private String welcomeMessage = Resources.getString("Tutorial.instructions"); //$NON-NLS-1$
  private String promptMessage = Resources.getString("Tutorial.load_tutorial"); //$NON-NLS-1$

  public Tutorial() {
    launch = new AbstractAction(Resources.getString("Tutorial.tutorial")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        launch();
      };
    };
  }

  private void launch() {
    try {
      int index = fileName.indexOf("."); //$NON-NLS-1$
      String prefix = index > 3 ? fileName.substring(0, index) : "VSL"; //$NON-NLS-1$
      String suffix = index >= 0 ? fileName.substring(index) : ".log"; //$NON-NLS-1$
      File tmp = File.createTempFile(prefix, suffix);
      BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(tmp));
      BufferedInputStream in = new BufferedInputStream(GameModule.getGameModule().getDataArchive().getFileStream(fileName));
      int len = 0;
      byte[] b = new byte[in.available()];
      while ((len = in.read(b)) > 0) {
        out.write(b, 0, len);
      }
      in.close();
      out.close();
      File renamed = new File(tmp.getParent(), (String) launch.getValue(Action.NAME));
      if (tmp.renameTo(renamed)) {
        tmp = renamed;
      }
      if (welcomeMessage != null
          && welcomeMessage.length() > 0) {
        GameModule.getGameModule().warn(welcomeMessage);
      }
      GameModule.getGameModule().getGameState().loadGame(tmp);
    }
    catch (IOException e1) {
      e1.printStackTrace();
      String msg = Resources.getString("Tutorial.unable_to_launch", name); //$NON-NLS-1$
      if (e1.getMessage() != null) {
        msg += ":  " + e1.getMessage(); //$NON-NLS-1$
      }
      GameModule.getGameModule().warn(msg);
    }
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Menu Text", "Logfile", "Launch automatically on first startup", "Auto-launch confirm message", "Welcome message"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, File.class, Boolean.class, String.class, String.class};
  }

  public String[] getAttributeNames() {
    return new String[]{NAME, FILE_NAME, LAUNCH_ON_STARTUP, PROMPT_MESSAGE, WELCOME_MESSAGE};
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (name.equals(PROMPT_MESSAGE)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return launchOnFirstStartup;
        }
      };
    }
    return null;
  }

  public String getAttributeValueString(String key) {
    if (FILE_NAME.equals(key)) {
      return fileName;
    }
    else if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (LAUNCH_ON_STARTUP.equals(key)) {
      return "" + launchOnFirstStartup; //$NON-NLS-1$
    }
    else if (PROMPT_MESSAGE.equals(key)) {
      return promptMessage;
    }
    else if (WELCOME_MESSAGE.equals(key)) {
      return welcomeMessage;
    }
    else {
      return null;
    }
  }

  public void setAttribute(String key, Object value) {
    if (FILE_NAME.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      fileName = (String) value;
    }
    else if (NAME.equals(key)) {
      launch.putValue(Action.NAME, value);
      setConfigureName((String) value);
    }
    else if (LAUNCH_ON_STARTUP.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      launchOnFirstStartup = ((Boolean) value).booleanValue();
    }
    else if (PROMPT_MESSAGE.equals(key)) {
      promptMessage = (String) value;
    }
    else if (WELCOME_MESSAGE.equals(key)) {
      welcomeMessage = (String) value;
    }
  }

  public void addTo(Buildable parent) {
    item = ((Documentation) parent).getHelpMenu().add(launch);
    final String key = "viewedTutorial" + getConfigureName(); //$NON-NLS-1$
    GameModule.getGameModule().getPrefs().addOption(null, new BooleanConfigurer(key, null, Boolean.FALSE));
    if (launchOnFirstStartup
        && !Boolean.TRUE.equals(GameModule.getGameModule().getPrefs().getValue(key))) {
      Runnable runnable = new Runnable() {
        public void run() {
          String[] options = new String[]{Resources.getString(Resources.YES),Resources.getString(Resources.NO), Resources.getString("Tutorial.dont_ask")}; //$NON-NLS-1$
          switch (JOptionPane.showOptionDialog(GameModule.getGameModule().getFrame(), promptMessage, getConfigureName(),
              JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE, null, options, options[0])) {
            case 0: // Yes
              launch();
            case 2: // Don't ask again
              GameModule.getGameModule().getPrefs().setValue(key, Boolean.TRUE);
              break;
          }
        }
      };
      SwingUtilities.invokeLater(runnable);
    }
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("HelpMenu.htm", "Tutorial"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void removeFrom(Buildable parent) {
    if (item != null) {
      ((Documentation) parent).getHelpMenu().remove(item);
    }
  }
}
