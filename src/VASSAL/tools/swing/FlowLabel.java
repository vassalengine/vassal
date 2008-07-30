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

package VASSAL.tools.swing;

import java.awt.Dimension;
import java.awt.Frame;

import javax.swing.JDialog;
import javax.swing.JTextPane;
import javax.swing.UIManager;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;

/**
 * A label which word-wraps and fully justifies its text, and which
 * reflows the text when resized.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class FlowLabel extends JTextPane {
  private static final long serialVersionUID = 1L;

  private final int width;

  /**
   * Creates a <code>FlowLabel</code> with the desired message and
   * an initial width of 40em.
   *
   * @param message the text for the label
   */
  public FlowLabel(String message) {
    this(message, 40);
  }

  /**
   * Creates a <code>FlowLabel</code> with the desired message
   * and width.
   *
   * @param message the text for the label
   * @param width the initial width of the label in ems.
   */
  public FlowLabel(String message, int width) {
    super();
    this.width = width;

    setText(message);
     
    final SimpleAttributeSet sa = new SimpleAttributeSet();
    StyleConstants.setAlignment(sa, StyleConstants.ALIGN_JUSTIFIED);
    getStyledDocument().setParagraphAttributes(0, message.length(), sa, false);

    // set mesasge area to have the background a JLabel would have
    setBackground(UIManager.getColor("Label.background"));
  }

  private boolean firstTimeLayout;

  /** {@inheritDoc} */
  @Override
  public void addNotify() {
    super.addNotify();

    // The purpose of this is to find the correct height for the
    // pane given the initial width, prior to the first layout.
    // Once the pane has been laid out once, we clear the maximum
    // size, as it's only needed for fixing the initial width.

    // fix the initial width in em
    final int w = width * getFontMetrics(getFont()).stringWidth("M");
      
    // determine the preferred height at the initial width
    final View v = getUI().getRootView(this);
    v.setSize(w, Integer.MAX_VALUE);
    final int h = (int) v.getPreferredSpan(View.Y_AXIS) + 1;

    // set the maximum size to the initial width and height
    final Dimension d = new Dimension(w,h);
    setSize(d);
    setMaximumSize(d);

    // signal to doLayout() to clear the maximum size
    firstTimeLayout = true;
  }

  /** {@inheritDoc} */
  @Override
  public void doLayout() {
    super.doLayout(); 

    if (firstTimeLayout) {
      // clear the maximum size now that initial layout is done
      setMaximumSize(null);
      firstTimeLayout = false;
    }
  }

  public static void main(String[] args) {
    final String loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

    final JDialog d = new JDialog((Frame) null, "Flow Label Test");
    d.add(new FlowLabel(loremIpsum + "\n\n" + loremIpsum));
    d.pack();
    d.setVisible(true);
  }
}
