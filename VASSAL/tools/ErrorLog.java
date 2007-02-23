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
package VASSAL.tools;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import VASSAL.build.GameModule;

/**
 * Warns the user when an uncaught Exception occurs
 * Use this by calling System.setProperty("sun.awt.exception.handler","VASSAL.tools.ErrorLog");
 * See Java code in  EventDispatchThread.handleException()
 */
public class ErrorLog {
  private static boolean disabled = false;

  public void handle(Throwable t) {
    String logFile = System.getProperty("stderr");
    if (!disabled && logFile != null) {
      String type = t.getClass().getName().substring(t.getClass().getName().lastIndexOf(".") + 1);
      String msg = t.getMessage();
      if (msg == null
          || msg.length() == 0) {
        msg = type;
      }
      else {
        msg = type + "\n" + msg;
      }
      JButton okButton = new JButton("Ok");
      JButton disableButton = new JButton("Don't show this dialog again");
      String text = "An untrapped error has occurred.\n"
          + msg + "\n"
          + "Please send a report to support@vassalengine.org and attach the log file.\n" + logFile;
      if (t instanceof OutOfMemoryError) {
        String s = t.getMessage();
        text = "The application has run out of memory.\n";
        if (s != null) {
          text += s+"\n";
        }
        text += "To decrease memory usage, try reducing the number of colors in your display.";
      }
      final JOptionPane pane = new JOptionPane
          (text,
           JOptionPane.DEFAULT_OPTION,
           JOptionPane.ERROR_MESSAGE,
           UIManager.getIcon("OptionPane.errorIcon"),
           new Object[]{okButton, disableButton},
           okButton);
      Component comp = GameModule.getGameModule() == null ? null : GameModule.getGameModule().getFrame();
      final JDialog dialog = pane.createDialog(comp, "Error");
      okButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          pane.setValue(Boolean.FALSE);
          dialog.dispose();
        }
      });
      disableButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent evt) {
          pane.setValue(Boolean.TRUE);
          dialog.dispose();
        }
      });
      Runnable runnable = new Runnable() {
        public void run() {
          dialog.setVisible(true);
          disabled = Boolean.TRUE.equals(pane.getValue());
        }
      };
      SwingUtilities.invokeLater(runnable);
    }
    t.printStackTrace();
  }

  public static void main(String[] args) {
    ErrorLog log = new ErrorLog();
    while (!disabled) {
      log.handle(new RuntimeException("Warning!!!"));
    }
  }

  public static class Group extends ThreadGroup {
    private ErrorLog handler = new ErrorLog();

    public Group() {
      super("Main Thread");
    }

    public void uncaughtException(Thread t, Throwable e) {
      handler.handle(e);
    }
  }
}
