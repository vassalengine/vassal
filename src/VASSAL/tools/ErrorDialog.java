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
import java.awt.event.ActionEvent;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.AbstractAction;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

import org.jdesktop.layout.GroupLayout;
import org.jdesktop.layout.LayoutStyle;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.FlowLabel;

public class ErrorDialog {
  private ErrorDialog() {}

  private static final Set<Object> disabled =
    Collections.synchronizedSet(new HashSet<Object>());
  
  private static final Set<String> reportedDataErrors = Collections.synchronizedSet(new HashSet<String>());

  public static boolean isDisabled(Object key) {
    return disabled.contains(key);
  }

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

  private static final BlockingQueue<Object> queue =
    new LinkedBlockingQueue<Object>();

  // This thread consumes error messages so as not to block the threads
  // which are generating them or the EDT.
  static {
    new Thread() {
      @Override
      public void run() {
        try {
          while (true) {
            final Object o = queue.take();
          
            if (o instanceof Message) {
              final Message m = (Message) o;                

              if (isDisabled(m.key)) return;

              try {
                SwingUtilities.invokeAndWait(new Runnable() {
                  public void run() {
                    showDialog(m.parent, m.title, m.header,
                               m.message, m.messageType, m.key);
                  }
                });
              }
              catch (InterruptedException e) {
                ErrorLog.log(e);
              }
              catch (InvocationTargetException e) {
                ErrorLog.log(e);
              }
            }
            else if (o instanceof Bug) {
              try {
                SwingUtilities.invokeAndWait(new Runnable() {
                  public void run() {
                    BugDialog.reportABug();
                  }
                });
              }
              catch (InterruptedException e) {
                ErrorLog.log(e);
              }
              catch (InvocationTargetException e) {
                ErrorLog.log(e);
              }
            }
          }
        }
        catch (InterruptedException e) {
          ErrorLog.log(e);
        }
      }
    }.start();
  }

// FIXME: make method which takes Throwable but doesn't use it for details

  public static void bug(Throwable t) {
    ErrorLog.log(t);

    // determine whether an OutOfMemoryError is in our causal chain
    Throwable cause = t;
    for ( ; cause != null; cause = cause.getCause()) {
      if (t instanceof OutOfMemoryError) {
        ErrorDialog.error(
          Resources.getString("Error.out_of_memory"),
          Resources.getString("Error.out_of_memory"),
          Resources.getString("Error.out_of_memory_message")
        );
        return;
      }
    }
    
    queue.add(new Bug(t));

/*
    // otherwise determine (i.e., guess) who to blame for the bug
    for (StackTraceElement trace : t.getStackTrace()) {
      final String cn = trace.getClassName();
      final String loc = ErrorDialog.class.getResource(cn);
      
      if (loc.contains("/rt.jar!/") ||    // Java runtime
          loc.contains("/jce.jar!/") ||   // javax.crypto
      

        // are we a JRE class?

      }
    }
 

// FIXME: not right. We need to start at the deepest point and work
// our way up until we find a non-JDK class. We can find where a class
// came from using  getResource("classname").getFile().

    if (trace.length == 0 || trace[0].getClassName().startsWith("VASSAL.")) {
      // it's our fault
      BugDialog.reportABug();
    }
    else {
      // it's the module's fault
      BugDialog.reportABug();
    }
*/

/*
    // determine whether an OutOfMemoryError is in our causal chain
    for (Throwable cause = t; cause != null; cause = cause.getCause()) {
      if (t instanceof OutOfMemoryError) {
        ErrorDialog.error(
          Resources.getString("Error.out_of_memory"),
          Resources.getString("Error.out_of_memory"),
          Resources.getString("Error.out_of_memory_message")
        );
        return;
      }
    }

    BugDialog.reportABug();
*/
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
    ErrorLog.log(thrown);
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
    ErrorLog.log(thrown);
    show(parent, title, header, message,
         getStackTrace(thrown), JOptionPane.WARNING_MESSAGE, key);
  }
  
  public static void dataError(BadDataReport e) {
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
    if (disabled.contains(key)) return;
  
    final Message m =
      new Message(parent, title, header, message, null, messageType, key);

    queue.add(m);
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
    if (disabled.contains(key)) return;

    final Message m =
      new Message(parent, title, header, message, details, messageType, key);

    queue.add(m);
  }
  
  private static void showDialog(
    final Component parent,
    final String title,
    final String header,
    final String[] message,
    final int messageType,
    final Object key)
  {
    final JPanel panel = new JPanel();

    // set a slightly larger, bold font for the header
    final JLabel headerLabel = new JLabel(header);
    final Font f = headerLabel.getFont();
    headerLabel.setFont(f.deriveFont(Font.BOLD, f.getSize()*1.2f));

    // put together the paragraphs of the message
    final FlowLabel messageLabel =
      new FlowLabel(StringUtils.join(message, "\n\n"));

    final JCheckBox disableCheck;

    // layout the panel  
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);

    if (key != null) {
      disableCheck = new JCheckBox("Do not show this dialog again");

      layout.setHorizontalGroup(
        layout.createParallelGroup(GroupLayout.LEADING, true)
          .add(headerLabel)
          .add(messageLabel)
          .add(disableCheck));
  
      layout.setVerticalGroup(
        layout.createSequentialGroup()
          .add(headerLabel)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(messageLabel, GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(disableCheck));
    }
    else {
      disableCheck = null;

      layout.setHorizontalGroup(
        layout.createParallelGroup(GroupLayout.LEADING, true)
          .add(headerLabel)
          .add(messageLabel));
  
      layout.setVerticalGroup(
        layout.createSequentialGroup()
          .add(headerLabel)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(messageLabel, GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE));
    }

    final JDialog dialog = new JOptionPane(
      panel,
      messageType,
      JOptionPane.DEFAULT_OPTION
    ).createDialog(parent, title);
  
 // FIXME: setModal() is obsolete. Use setModalityType() in 1.6+.
//    d.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
    dialog.setModal(true);
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.setResizable(true);
    dialog.pack();
    dialog.setVisible(true);

    if (disableCheck != null && disableCheck.isSelected()) disabled.add(key);
  }

  private static void showDialog(
    final Component parent,
    final String title,
    final String header,
    final String[] message,
    final String details,
    final int messageType,
    final Object key)
  {
    final JPanel panel = new JPanel();

    // set a slightly larger, bold font for the header
    final JLabel headerLabel = new JLabel(header);
    final Font f = headerLabel.getFont();
    headerLabel.setFont(f.deriveFont(Font.BOLD, f.getSize()*1.2f));

    // put together the paragraphs of the message
   final FlowLabel messageLabel =
      new FlowLabel(StringUtils.join(message, "\n\n"));

    final JCheckBox disableCheck;

    // set up the details view
    final JTextArea detailsArea = new JTextArea(details, 10, 36);
    detailsArea.setEditable(false);

    final JScrollPane detailsScroll = new JScrollPane(detailsArea);
    detailsScroll.setVisible(false);

    // layout the panel  
    final GroupLayout layout = new GroupLayout(panel);
    panel.setLayout(layout);

    layout.setAutocreateGaps(true);
    layout.setAutocreateContainerGaps(true);

    if (key != null) {
      disableCheck = new JCheckBox("Do not show this dialog again");

      layout.setHorizontalGroup(
        layout.createParallelGroup(GroupLayout.LEADING, true)
          .add(headerLabel)
          .add(messageLabel)
          .add(detailsScroll)
          .add(disableCheck));
  
      layout.setVerticalGroup(
        layout.createSequentialGroup()
          .add(headerLabel)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(messageLabel, GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(detailsScroll, 0,
                              GroupLayout.PREFERRED_SIZE,
                              Integer.MAX_VALUE)
          .add(disableCheck));
    }
    else {
      disableCheck = null;

      layout.setHorizontalGroup(
        layout.createParallelGroup(GroupLayout.LEADING, true)
          .add(headerLabel)
          .add(messageLabel)
          .add(detailsScroll));
  
      layout.setVerticalGroup(
        layout.createSequentialGroup()
          .add(headerLabel)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(messageLabel, GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE,
                             GroupLayout.PREFERRED_SIZE)
          .addPreferredGap(LayoutStyle.UNRELATED)
          .add(detailsScroll, 0,
                              GroupLayout.PREFERRED_SIZE,
                              Integer.MAX_VALUE));
    }

    final JButton okButton = new JButton();
    final JButton detailsButton = new JButton();

    final JDialog dialog = new JOptionPane(
      panel,
      messageType,
      JOptionPane.DEFAULT_OPTION,
      null,
      new JButton[] { okButton, detailsButton }
    ).createDialog(parent, title);

    okButton.setAction(new AbstractAction("Ok") {
      private static final long serialVersionUID = 1L;

      public void actionPerformed(ActionEvent e) {
        dialog.dispose();
      }
    });

    detailsButton.setAction(new AbstractAction("<html>Details &raquo;</html>") {
      private static final long serialVersionUID = 1L;
    
      public void actionPerformed(ActionEvent e) {
        detailsScroll.setVisible(!detailsScroll.isVisible());
        putValue(NAME, detailsScroll.isVisible() ?
          "<html>Details &laquo;</html>" : "<html>Details &raquo;</html>");

        // ensure that neither expansion nor collapse changes the dialog width
        final Dimension d = messageLabel.getSize();
        d.height = Integer.MAX_VALUE;
        detailsScroll.setMaximumSize(d);
        messageLabel.setMaximumSize(d);

        dialog.pack();

        detailsScroll.setMaximumSize(null);
        messageLabel.setMaximumSize(null);
      }
    });
 
// FIXME: setModal() is obsolete. Use setModalityType() in 1.6+.
//    d.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
    dialog.setModal(true);
    dialog.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    dialog.setResizable(true);
    dialog.pack();
    dialog.setVisible(true);

    if (disableCheck != null && disableCheck.isSelected()) disabled.add(key);
  }

  private static Component getFrame() {
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
        while (!isDisabled(0)) {
          ErrorDialog.warning("Oh Shit!", "Oh Shit!", 0,
                              loremIpsum, loremIpsum);
        }
      }
    }).start();

    new Thread(new Runnable() {
      public void run() {
        while (!isDisabled(1)) {
          ErrorDialog.error("Oh Shit!", "Oh Shit!", 1,
                              loremIpsum, loremIpsum);
        }
      }
    }).start();

//    System.exit(0); 
  }
}
