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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JMenuItem;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.BackgroundTask;

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
  protected BooleanConfigurer hasViewedTutorial;

  public Tutorial() {
    launch = new AbstractAction(Resources.getString("Tutorial.tutorial")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        launch();
      };
    };
  }

  public void launch() {
    GameModule.getGameModule().warn(Resources.getString("Tutorial.Tutorial.loading")); //$NON-NLS-1$
    new BackgroundTask() {
      private Command saveCommand;
      private String error;
      public void doFirst() {
        try {
          saveCommand = getTutorialCommand();
        }
        catch (IOException e) {
          e.printStackTrace();
          String msg = Resources.getString("Tutorial.unable_to_launch", name); //$NON-NLS-1$
          if (e.getMessage() != null) {
            msg += ":  " + e.getMessage(); //$NON-NLS-1$
          }
          error = msg;
        }
      }

      @Override
      public void doLater() {
        if (saveCommand != null) {
          saveCommand.execute();
          if (welcomeMessage != null
              && welcomeMessage.length() > 0) {
            GameModule.getGameModule().warn(welcomeMessage);
          }
        }
        else {
          GameModule.getGameModule().warn(error);
        }
      }
      
    }.start();
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Menu Text", "Logfile", "Launch automatically on first startup", "Auto-launch confirm message", "Welcome message"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
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
    hasViewedTutorial = new BooleanConfigurer(key, null, Boolean.FALSE);
    GameModule.getGameModule().getPrefs().addOption(null, hasViewedTutorial);
    GameModule.getGameModule().getWizardSupport().setTutorial(this);
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

  /**
   * Get the Command representing this tutorial logfile.  Executing the command loads the tutorial
   * @return
   * @throws IOException
   */
  public Command getTutorialCommand() throws IOException {
    return GameModule.getGameModule().getGameState().decodeSavedGame(getTutorialContents());
  }

  public InputStream getTutorialContents() throws IOException {
    return GameModule.getGameModule().getDataArchive().getFileStream(fileName);
  }

  public boolean isFirstRun() {
    return launchOnFirstStartup
        && !hasViewedTutorial.booleanValue();
  }
  
  /**
   * Mark this tutorial as having been viewed 
   */
  public void markAsViewed() {
    hasViewedTutorial.setValue(Boolean.TRUE);
  }

  public String getWelcomeMessage() {
    return welcomeMessage;
  }
}
