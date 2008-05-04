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
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import VASSAL.build.GameModule;

public class ErrorDialog {
  private static JOptionPane pane;

  private static JDialog createDialog(String message, int severity) {
    final JButton okButton = new JButton("Ok");

    final JCheckBox disableCheck =
      new JCheckBox("Do not show this dialog again");

    // prevent exception from null messages  
    if (message == null) message = ""; 

    final JLabel msgLabel = new JLabel(
      "<html>" + message.replace("\n", "<p>") + "</html>");

    final Box msgBox = new Box(BoxLayout.Y_AXIS);
    msgBox.add(msgLabel);
    msgBox.add(Box.createVerticalStrut(
      msgLabel.getFontMetrics(msgLabel.getFont()).getHeight()));
    msgBox.add(disableCheck);
    
    pane = new JOptionPane(
      msgBox,
      JOptionPane.DEFAULT_OPTION,
      JOptionPane.ERROR_MESSAGE,
      UIManager.getIcon("OptionPane.errorIcon"),
      new Object[]{okButton},
      okButton
    );

    final Component comp = GameModule.getGameModule() == null
                         ? null : GameModule.getGameModule().getFrame();

    final JDialog d = pane.createDialog(comp, "Error");
    
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        pane.setValue(!disableCheck.isSelected());
      }
    });

    d.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent evt) {
        pane.setValue(!disableCheck.isSelected());
      }
    });

    d.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
// FIXME: setModal() is obsolete. Use setModalityType() in 1.6+.
//    d.setModalityType(JDialog.ModalityType.APPLICATION_MODAL);
    d.setModal(true);
    d.pack();
    
    return d;
  }

  private static class Message {
    public final String text;
    public final Class<? extends Throwable> tclass;
    public final int severity;

    public Message(Class<? extends Throwable> tclass,
                   String text, int severity) {
      this.tclass = tclass;
      this.text = text;
      this.severity = severity;
    }
  }

  private static final BlockingQueue<Message> queue =
    new LinkedBlockingQueue<Message>();

  private static final Set<Class<? extends Throwable>> disabled =
    Collections.synchronizedSet(new HashSet<Class<? extends Throwable>>());

  public static final int ERROR = 0;
  public static final int WARNING = 1;

  public static void error(Throwable t, String message) {
    raise(t, message, ERROR);
  }

  public static void warning(Throwable t, String message) {
    raise(t, message, WARNING);
  }

  public static void raise(Throwable t, String message, int severity) {
    final Class<? extends Throwable> throwableClass = t.getClass();
    if (ErrorDialog.isDisabled(throwableClass)) return;
    queue.add(new Message(throwableClass, message, severity));
  }

  // We start a Thread to consume Messages as they queue up. This
  // thread blocks on closing of the dialog to prevent the user
  // from being presented with more than one error dialog at a time.
  static {
    new Thread() {
      @Override
      public void run() {
        try {
          while (true) {
            final Message m = queue.take();

            if (ErrorDialog.isDisabled(m.tclass)) return;
            final JDialog dialog = createDialog(m.text, m.severity); 
  
            SwingUtilities.invokeAndWait(new Runnable() {
              public void run() {
                dialog.setVisible(true);
                if (Boolean.FALSE.equals(pane.getValue())) {
                  disabled.add(m.tclass);
                }
                dialog.dispose();
              }
            });
          }
        }
        catch (InterruptedException e) {
          e.printStackTrace();
        }
        catch (InvocationTargetException e) {
          e.printStackTrace();
        }
      }
    }.start();
  }

  public static boolean isDisabled(Class<? extends Throwable> c) {
    return disabled.contains(c);
  }
}
