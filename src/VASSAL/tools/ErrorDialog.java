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
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JOptionPane;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.logging.Logger;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ErrorDialog {
  private ErrorDialog() {}

// FIXME: make method which takes Throwable but doesn't use it for details

  public static void bug(Throwable thrown) {
    // determine whether an OutOfMemoryError is in our causal chain
    final OutOfMemoryError oom =
      ThrowableUtils.getAncestorOfClass(OutOfMemoryError.class, thrown);
    if (oom != null) {
      Logger.log(thrown);
      show("Error.out_of_memory");
    }
    // show a bug report dialog if one has not been shown before
    else if (!DialogUtils.setDisabled(BugDialog.class, true)) {
      Logger.logAndWait(thrown, Logger.BUG);
      BugDialog.reportABug(thrown);
    }
  }

  public static void show(
    String messageKey,
    Object... args)
  {
    ProblemDialog.show(JOptionPane.ERROR_MESSAGE, messageKey, args);
  }

  public static void show(
    Component parent,
    String messageKey,
    Object... args)
  {
    ProblemDialog.show(JOptionPane.ERROR_MESSAGE, parent, messageKey, args);
  }

  public static void show(
    Throwable thrown,
    String messageKey,
    Object... args)
  {
    ProblemDialog.show(JOptionPane.ERROR_MESSAGE, thrown, messageKey, args);
  }

  public static void show(
    final Component parent,
    final Throwable thrown,
    final String messageKey,
    final Object... args)
  {
    ProblemDialog.show(JOptionPane.ERROR_MESSAGE, parent,
                       thrown, messageKey, args);
  }

  public static void show(
    final Component parent,
    final Throwable thrown,
    final String title,
    final String heading,
    final String message)
  {
    ProblemDialog.show(JOptionPane.ERROR_MESSAGE, parent,
                       thrown, title, heading, message);
  }

  public static void showDisableable(
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDisableable(JOptionPane.ERROR_MESSAGE,
                                  key, messageKey, args);
  }

  public static void showDisableable(
    Component parent,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDisableable(JOptionPane.ERROR_MESSAGE,
                                  parent, key, messageKey, args);
  }

  public static void showDisableable(
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDisableable(JOptionPane.ERROR_MESSAGE,
                                  thrown, key, messageKey, args);
  }

  public static void showDisableable(
    Component parent,
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDisableable(JOptionPane.ERROR_MESSAGE, parent,
                                  thrown, key, messageKey, args);
  }

  public static void showDisableable(
    Component parent,
    Throwable thrown,
    Object key,
    String title,
    String heading,
    String message)
  {
    ProblemDialog.showDisableable(JOptionPane.ERROR_MESSAGE, parent,
                                  thrown, key, title, heading, message);
  }

  public static void showDetails(
    String details,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetails(JOptionPane.ERROR_MESSAGE,
                              details, messageKey, args);
  }

  public static void showDetails(
    Component parent,
    String details,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetails(JOptionPane.ERROR_MESSAGE,
                              parent, details, messageKey, args);
  }
  
  public static void showDetails(
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetails(JOptionPane.ERROR_MESSAGE,
                              thrown, details, messageKey, args);
  }

  public static void showDetails(
    Component parent,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetails(JOptionPane.ERROR_MESSAGE,
                              parent, thrown, details, messageKey, args);
  }

  public static void showDetails(
    Component parent,
    Throwable thrown,
    String details,
    String title,
    String heading,
    String message)
  {
    ProblemDialog.showDetails(JOptionPane.ERROR_MESSAGE,
                              parent, thrown, details, title, heading, message);
  }

  public static void showDetailsDisableable(
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.ERROR_MESSAGE,
                                         details, key, messageKey, args);
  }

  public static void showDetailsDisableable(
    Component parent,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.ERROR_MESSAGE,
      parent, details, key, messageKey, args);
  }
  
  public static void showDetailsDisableable(
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.ERROR_MESSAGE,
      thrown, details, key, messageKey, args);
  }

  public static void showDetailsDisableable(
    Component parent,
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.ERROR_MESSAGE,
      parent, thrown, details, key, messageKey, args);
  }

  public static void showDetailsDisableable(
    Component parent,
    Throwable thrown,
    String details,
    Object key,
    String title,
    String heading,
    String message)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.ERROR_MESSAGE,
      parent, thrown, details, key, title, heading, message);
  }

////////////////


  private static final Set<String> reportedDataErrors =
    Collections.synchronizedSet(new HashSet<String>());

  public static void dataError(BadDataReport e) {
    Logger.log(e.getMessage() + ": " + e.getData(), Logger.WARNING);
    if (e.getCause() != null) Logger.log(e.getCause());

    if (!reportedDataErrors.contains(e.getData())) {
      reportedDataErrors.add(e.getData());

      // send a warning to the controls window
      GameModule.getGameModule().warn(Resources.getString(
        "Error.data_error_message", e.getMessage(), e.getData()
      ));
    }
  }

///////////////////


  public static void main(String[] args) throws Exception {
    final String loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n\nLorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

    while (!DialogUtils.isDisabled(0)) {
      showDisableable(null, null, 0, "Oh Shit!", "Oh Shit!", loremIpsum);
      Thread.sleep(1000);
    }

    System.exit(0); 
  }
}
