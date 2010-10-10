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
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;

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

    progbar = new JProgressBar(0, 100);
    progbar.setStringPainted(true);
    progbar.setValue(0);
  
    cancel = new JButton(Resources.getString(Resources.CANCEL));

    // create the layout
    final JPanel panel = new JPanel(new MigLayout(
      "insets dialog", "", "unrelated:push[]related[]unrelated:push[]"));

    panel.add(progbar, "pushx, growx, spanx, wrap");
    panel.add(label,   "pushx, growx, spanx, wrap unrel:push");
    panel.add(cancel,  "tag cancel");

    add(panel);
    pack();

    // set minimum size
    setMinimumSize(new Dimension(300, getHeight()));
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
   * Adds a listener for the cancel button.
   * 
   * @param l the action listener
   */
  public void addActionListener(ActionListener l) {
    cancel.addActionListener(l);
  }

  /**
   * Removes a listener from the cancel button.
   *
   * @param l the action listener
   */
  public void removeActionListener(ActionListener l) {
    cancel.removeActionListener(l);
  }
  
  /**
   * Gets the listeners for the cancel button.
   *
   * @return the action listeners
   */
  public ActionListener[] getActionListeners() {
    return cancel.getActionListeners();
  }
}
