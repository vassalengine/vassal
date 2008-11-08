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
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.io.PrintWriter;
import java.io.StringWriter;
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

  private static class Bug {
    public final Throwable t;

    public Bug(Throwable t) {
      this.t = t;
    }
  }
  
  private static class Message {
    public final Component parent;
    public final String title;
    public final String header;
    public final String[] message;
    public final String details;
    public final int messageType;
    public final Object key;

    public Message(
      Component parent,
      String title,
      String header,
      String[] message,
      String details,
      int messageType,
      Object key)
    {
      this.parent = parent;
      this.title = title;
      this.header = header;
      this.message = message;
      this.details = details;
      this.messageType = messageType;
      this.key = key;
    }
  }

// FIXME: make method which takes Throwable but doesn't use it for details

  public static void bug(Throwable thrown) {
    // determine whether an OutOfMemoryError is in our causal chain
    final OutOfMemoryError oom =
      ErrorUtils.getAncestorOfClass(OutOfMemoryError.class, thrown);
    if (oom != null) {
      Logger.log(thrown);

      error(
        Resources.getString("Error.out_of_memory"),
        Resources.getString("Error.out_of_memory"),
        Resources.getString("Error.out_of_memory_message")
      );
    }
    // show a bug report dialog if one has not been shown before
    else if (!DialogUtils.setDisabled(BugDialog.class, true)) {
      Logger.logAndWait(thrown, Logger.BUG);
      BugDialog.reportABug(thrown);
    }
  }

  public static void error(
    String title,
    String header,
    String... message)
  {
    error(title, header, (Object) null, message); 
  }

  public static void error(
    String title,
    String header,
    Object key,
    String... message)
  {
    show(getFrame(), title, header, message, JOptionPane.ERROR_MESSAGE, key); 
  }

  public static void error(
    String title,
    String header,
    Throwable thrown,
    String... message)
  {
    error(getFrame(), title, header, thrown, null, message); 
  }

  public static void error(
    String title,
    String header,
    Throwable thrown,
    Object key,
    String... message)
  {
    error(getFrame(), title, header, thrown, key, message); 
  }

  public static void error(
    Component parent,
    String title,
    String header,
    Throwable thrown,
    Object key,
    String... message)
  {
    if (thrown != null) Logger.log(thrown);
    show(parent, title, header, message,
         getStackTrace(thrown), JOptionPane.ERROR_MESSAGE, key);
  }

  public static void warning(
    String title,
    String header,
    String... message)
  {
    warning(title, header, (Object) null, message); 
  }

  public static void warning(
    String title,
    String header,
    Object key,
    String... message)
  {
    show(getFrame(), title, header, message, JOptionPane.WARNING_MESSAGE, key);
  }

  public static void warning(
    String title,
    String header,
    Throwable thrown,
    String... message)
  {
    warning(getFrame(), title, header, thrown, null, message); 
  }

  public static void warning(
    String title,
    String header,
    Throwable thrown,
    Object key,
    String... message)
  {
    warning(getFrame(), title, header, thrown, key, message); 
  }

  public static void warning(
    Component parent,
    String title,
    String header,
    Throwable thrown,
    Object key,
    String... message)
  {
    if (thrown != null) Logger.log(thrown);
    show(parent, title, header, message,
         getStackTrace(thrown), JOptionPane.WARNING_MESSAGE, key);
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
        warning(Resources.getString("Error.data_error"), Resources.getString("Error.data_error_message"), (Object)e.getData(),e.getMessage()+":  "+e.getData());
      }
    }
  }

  public static void show(
    final Component parent,
    final String title,
    final String header,
    final String[] message,
    final int messageType,
    final Object key)
  {
    showDialog(
      parent,
      title,
      header,
      StringUtils.join(message, "\n\n"),
      messageType,
      key
    );
  }

  public static void show(
    final Component parent,
    final String title,
    final String header,
    final String[] message,
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
                StringUtils.join(message, "\n\n"),
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

  private static String getStackTrace(Throwable t) {
    final StringWriter sw = new StringWriter();
    final PrintWriter pw = new PrintWriter(sw);
    t.printStackTrace(pw);
    pw.close();
    return sw.toString();
  }

  public static void main(String[] args) {
    final String loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

    ErrorDialog.warning("Oh Shit!", "Oh Shit!", loremIpsum, loremIpsum);
    ErrorDialog.error("Oh Shit!", "Oh Shit!", loremIpsum, loremIpsum);

    ErrorDialog.warning("Oh Shit!", "Oh Shit!", true, loremIpsum, loremIpsum);
    ErrorDialog.error("Oh Shit!", "Oh Shit!", true, loremIpsum, loremIpsum);

    new Thread(new Runnable() {
      public void run() {
        while (!DialogUtils.isDisabled(0)) {
          ErrorDialog.warning("Oh Shit!", "Oh Shit!", 0,
                              loremIpsum, loremIpsum);
        }
      }
    }).start();

    new Thread(new Runnable() {
      public void run() {
        while (!DialogUtils.isDisabled(1)) {
          ErrorDialog.error("Oh Shit!", "Oh Shit!", 1,
                              loremIpsum, loremIpsum);
        }
      }
    }).start();

//    System.exit(0); 
  }
}
