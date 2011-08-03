/*
 * $Id$
 *
 * Copyright (c) 2008-2010 by Joel Uckelman
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

package VASSAL.tools.swing;

import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.event.EventListenerList;

import net.miginfocom.swing.MigLayout;
import VASSAL.i18n.Resources;

/**
 * A cancellable progress dialog.
 *
 * @since 3.1.0
 * @author Joel Uckelman
 */
public class ProgressDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  protected final JLabel label;
  protected final JProgressBar progbar;
  protected final JButton cancel;

  protected final EventListenerList listeners = new EventListenerList();

  /**
   * Creates a progress dialog.
   *
   * @param parent the parent frame
   * @param title the dialog title
   * @param text the text beneath the progress bar
   */
  public ProgressDialog(Frame parent, String title, String text) {
    super(parent, title, true);

    // set up the components
    label = new JLabel(text);

//    Font f = label.getFont();
//    label.setFont(f.deriveFont(f.getSize2D()*4f/3f));
//    label.setFont(f.deriveFont(14f));

    progbar = new JProgressBar(0, 100);
    progbar.setStringPainted(true);
    progbar.setValue(0);

// AUGH! This is retarded that we can't set a default font size...
//    f = progbar.getFont();
//    progbar.setFont(f.deriveFont(14f));

    cancel = new JButton(Resources.getString(Resources.CANCEL));

//    f = cancel.getFont();
//    cancel.setFont(f.deriveFont(14f));

    // forward clicks on the close decoration to cancellation listeners
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        fireCancelledEvent(new ActionEvent(
          ProgressDialog.this, ActionEvent.ACTION_PERFORMED, "cancel"
        ));
      }
    });

    // forward clicks on the close button to the cancellation listeners
    cancel.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        fireCancelledEvent(e);
      }
    });

    // create the layout
    final JPanel panel = new JPanel(new MigLayout(
      "insets dialog, fill", "", "unrelated:push[]related[]unrelated:push[]"));

    // NB: It's necessary to set the minimum width for the label,
    // otherwise if the label text is set to a string which is too long,
    // the label will overflow the container instead of showing ellipses.
    panel.add(progbar, "growx, wrap");
    panel.add(label,   "wmin 0, pad 0 0 2pt 0, wrap unrel:push");
    panel.add(cancel,  "tag cancel");

    add(panel);

    // pack to find the minimum height
    pack();

    // set minimum size
    setMinimumSize(new Dimension(300, getHeight()));

    // pack again to ensure that we respect the minimum size
    pack();
  }

  protected void fireCancelledEvent(ActionEvent e) {
    final Object[] larr = listeners.getListenerList();

    // Process the listeners last to first, notifying
    // those that are interested in this event.
    for (int i = larr.length-2; i >= 0; i -= 2) {
      if (larr[i] == ActionListener.class) {
        ((ActionListener) larr[i+1]).actionPerformed(e);
      }
    }
  }

  /**
   * Gets the text label shown beneath the progress bar.
   *
   * @return the text label
   */
  public String getLabel() {
    return label.getText();
  }

  /**
   * Sets the text label shown beneath the progress bar.
   *
   * @param text the text label
   */
  public void setLabel(String text) {
    label.setText(text);
  }

  /**
   * Gets whether the progress bar is indeterminate.
   *
   * @return whether the progress bar is indeterminate
   */
  public boolean isIndeterminate() {
    return progbar.isIndeterminate();
  }

  /**
   * Sets whether the progress bar should be indeterminate.
   *
   * @param indet whether the progress bar should beindeterminate
   */
  public void setIndeterminate(boolean indet) {
    progbar.setIndeterminate(indet);
  }

  /**
   * Gets the percentage displayed by the progress bar.
   *
   * @return the percentage completed
   */
  public int getProgress() {
    return progbar.getValue();
  }

  /**
   * Sets the percentage for the progress bar.
   *
   * @param percent the percentage completed
   */
  public void setProgress(int percent) {
    progbar.setValue(percent);
  }

  /**
   * Gets whether the progress bar contains a progress string.
   *
   * @return whether the progress bar contains a progress string
   */
  public boolean isStringPainted() {
    return progbar.isStringPainted();
  }

  /**
   * Sets whether the progress bar should contain a progress string.
   *
   * @param painted whether the progress bar should contain a progress string
   */
  public void setStringPainted(boolean painted) {
    progbar.setStringPainted(painted);
  }

  /**
   * Adds a cancellation listener.
   *
   * @param l the action listener
   */
  public void addActionListener(ActionListener l) {
    listeners.add(ActionListener.class, l);
  }

  /**
   * Removes cancellation listener.
   *
   * @param l the action listener
   */
  public void removeActionListener(ActionListener l) {
    listeners.remove(ActionListener.class, l);
  }

  /**
   * Gets the list of cancellation listeners.
   *
   * @return the action listeners
   */
  public ActionListener[] getActionListeners() {
    return listeners.getListeners(ActionListener.class);
  }

  /**
   * Creates a progress dialog on the EDT.
   *
   * This is a convenience method to be used when a non-EDT thread needs to
   * create a progress dialog in order to attach listeners to it.
   *
   * @param parent the parent frame
   * @param title the dialog title
   * @param text the text beneath the progress bar
   */
  public static ProgressDialog createOnEDT(final Frame parent,
                                           final String title,
                                           final String text) {
    final Future<ProgressDialog> f = EDT.submit(new Callable<ProgressDialog>() {
      public ProgressDialog call() {
        return new ProgressDialog(parent, title, text);
      }
    });

    try {
      return f.get();
    }
    catch (CancellationException e) {
      // this should never happen
      throw new IllegalStateException(e);
    }
    catch (InterruptedException e) {
      // this should never happen
      throw new IllegalStateException(e);
    }
    catch (ExecutionException e) {
      throw new RuntimeException(e);
    }
  }
}
