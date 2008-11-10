/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools;

import java.awt.Component;
import java.awt.Frame;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.logging.Logger;
import VASSAL.tools.swing.DetailsDialog;
import VASSAL.tools.swing.Dialogs;

public class ErrorDialog {
  private ErrorDialog() {}

  private static final Set<String> reportedDataErrors =
    Collections.synchronizedSet(new HashSet<String>());

// FIXME: make method which takes Throwable but doesn't use it for details

  public static void bug(Throwable thrown) {
    // determine whether an OutOfMemoryError is in our causal chain
    final OutOfMemoryError oom =
      ErrorUtils.getAncestorOfClass(OutOfMemoryError.class, thrown);
    if (oom != null) {
      Logger.log(thrown);
      error("Error.out_of_memory");
    }
    // show a bug report dialog if one has not been shown before
    else if (!DialogUtils.setDisabled(BugDialog.class, true)) {
      Logger.logAndWait(thrown, Logger.BUG);
      BugDialog.reportABug(thrown);
    }
  }

  public static void error(String type) {
    error(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message")
    );
  }

  public static void error(String type, Object... args) {
    error(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message", args)
    );
  }

  public static void error(
    String type,
    Throwable thrown,
    Object key,
    Object... args)
  {
    error(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message", args),
      thrown,
      key
    );
  }

  public static void error(String type, Throwable thrown) {
    error(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message"),
      thrown 
    );
  }

  public static void error(String type, Object key) {
    error(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message"),
      key
    );
  }

  public static void error(
    String title,
    String header,
    String message)
  {
    error(title, header, message, (Object) null); 
  }

  public static void error(
    String title,
    String header,
    String message,
    Object key)
  {
    show(getFrame(), title, header, message, JOptionPane.ERROR_MESSAGE, key); 
  }

  public static void error(
    String title,
    String header,
    String message,
    Throwable thrown)
  {
    error(getFrame(), title, header, message, thrown, null); 
  }

  public static void error(
    String title,
    String header,
    String message,
    Throwable thrown,
    Object key)
  {
    error(getFrame(), title, header, message, thrown, key); 
  }

  public static void error(
    Component parent,
    String title,
    String header,
    String message,
    Throwable thrown,
    Object key)
  {
    if (thrown != null) Logger.log(thrown);
    show(parent, title, header, message,
         BugUtils.getStackTrace(thrown), JOptionPane.ERROR_MESSAGE, key);
  }

  public static void warning(String type) {
    warning(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message")
    );
  }

  public static void warning(String type, Object... args) {
    warning(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message", args)
    );
  }

  public static void warning(String type, Throwable thrown) {
    warning(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message"),
      thrown 
    );
  }

  public static void warning(String type, Object key) {
    warning(
      Resources.getString(type + "_title"),
      Resources.getString(type + "_heading"),
      Resources.getString(type + "_message"),
      key
    );
  }

  public static void warning(
    String title,
    String header,
    String message)
  {
    warning(title, header, message, (Object) null); 
  }

  public static void warning(
    String title,
    String header,
    String message,
    Object key)
  {
    show(getFrame(), title, header, message, JOptionPane.WARNING_MESSAGE, key);
  }

  public static void warning(
    String title,
    String header,
    String message,
    Throwable thrown)
  {
    warning(getFrame(), title, header, message, thrown, null); 
  }

  public static void warning(
    String title,
    String header,
    String message,
    Throwable thrown,
    Object key)
  {
    warning(getFrame(), title, header, message, thrown, key); 
  }

  public static void warning(
    Component parent,
    String title,
    String header,
    String message,
    Throwable thrown,
    Object key)
  {
    if (thrown != null) Logger.log(thrown);
    show(parent, title, header, message,
         BugUtils.getStackTrace(thrown), JOptionPane.WARNING_MESSAGE, key);
  }
  
  public static void dataError(BadDataReport e) {
    Logger.log(e.getMessage() + ": " + e.getData(), Logger.WARNING);
    if (e.getCause() != null) Logger.log(e.getCause());

    if (!reportedDataErrors.contains(e.getData())) {
      reportedDataErrors.add(e.getData());
      // When playing a module, send a warning to the controls window
      if (GameModule.getGameModule().getArchiveWriter() == null) {
        GameModule.getGameModule().warn(Resources.getString("Error.data_error_message")+":  "+e.getData());        
      }
      // If editing, show a warning dialog
      else {
        warning(
          Resources.getString("Error.data_error"),
          Resources.getString("Error.data_error"),
          Resources.getString("Error.data_error_message") + "\n\n" + e.getMessage()+":  "+e.getData(),
          (Object) e.getData()
        );
      }
    }
  }

  public static void show(
    final Component parent,
    final String title,
    final String header,
    final String message,
    final int messageType,
    final Object key)
  {
    showDialog(
      parent,
      title,
      header,
      message,
      messageType,
      key
    );
  }

  public static void show(
    final Component parent,
    final String title,
    final String header,
    final String message,
    final String details,
    final int messageType,
    final Object key)
  {
    if (DialogUtils.isDisabled(key)) return;

    DialogUtils.enqueue(new Runnable() {
      public void run() {
        try {
          SwingUtilities.invokeAndWait(new Runnable() {
            public void run() {
              DetailsDialog.showDialog(
                parent,
                title,
                header,
                message,
                details,
                messageType,
                key
              );
            }
          });
        }
        catch (InterruptedException e) {
          Logger.log(e);
        }
        catch (InvocationTargetException e) {
          Logger.log(e);
        }
      }
    });
  }
  
  private static void showDialog(
    final Component parent,
    final String title,
    final String header,
    final String message,
    final int messageType,
    final Object key)
  {
    if (DialogUtils.isDisabled(key)) return;

    DialogUtils.enqueue(new Runnable() {
      public void run() {
        try {
          SwingUtilities.invokeAndWait(new Runnable() {
            public void run() {
              Dialogs.showMessageDialog(
                parent,
                title,
                header,
                message,
                messageType,
                key
              );
            }
          });
        }
        catch (InterruptedException e) {
          Logger.log(e);
        }
        catch (InvocationTargetException e) {
          Logger.log(e);
        }
      }
    });
  }

  private static Frame getFrame() {
    return GameModule.getGameModule() == null
      ? null : GameModule.getGameModule().getFrame();
  }

  public static void main(String[] args) {
    final String loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n\nLorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

    ErrorDialog.warning("Oh Shit!", "Oh Shit!", loremIpsum);
    ErrorDialog.error("Oh Shit!", "Oh Shit!", loremIpsum);

    ErrorDialog.warning("Oh Shit!", "Oh Shit!", loremIpsum, true);
    ErrorDialog.error("Oh Shit!", "Oh Shit!", loremIpsum, true);

    new Thread(new Runnable() {
      public void run() {
        while (!DialogUtils.isDisabled(0)) {
          ErrorDialog.warning("Oh Shit!", "Oh Shit!", loremIpsum, 0);
        }
      }
    }).start();

    new Thread(new Runnable() {
      public void run() {
        while (!DialogUtils.isDisabled(1)) {
          ErrorDialog.error("Oh Shit!", "Oh Shit!", loremIpsum, 1);
        }
      }
    }).start();

//    System.exit(0); 
  }
}
