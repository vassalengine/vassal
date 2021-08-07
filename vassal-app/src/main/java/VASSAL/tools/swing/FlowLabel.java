/*
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
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.JDialog;
import javax.swing.JTextPane;
import javax.swing.UIManager;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyledDocument;

/**
 * A label which word-wraps and fully justifies its text, and which
 * reflows the text when resized.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class FlowLabel extends JTextPane {
  private static final long serialVersionUID = 1L;

  private static final int DEFAULT_WIDTH = 40;

  /**
   * Creates a <code>FlowLabel</code> with the desired text and
   * an initial width of 40em.
   *
   * @param text the text for the label
   */
  public FlowLabel(String text) {
    this(text, DEFAULT_WIDTH);
  }

  /**
   * Creates a <code>FlowLabel</code> with the desired text
   * and width.
   *
   * @param text the text for the label
   * @param width the initial width of the label in em
   */
  public FlowLabel(String text, int width) {
    super();
    setEditable(false);
    setText(text);

    // FIXME: This is a workaround for Redhat Bugzilla Bug #459967:
    // JTextPane.setBackground() fails when using GTK LookAndFeel. Once this
    // bug is resolved, there is no need to make this component nonopaque.
    setOpaque(false);

    // set the colors and font a JLabel would have
    putClientProperty(HONOR_DISPLAY_PROPERTIES, Boolean.TRUE);
    setFont(UIManager.getFont("Label.font"));
    setForeground(UIManager.getColor("Label.foreground"));
    setBackground(UIManager.getColor("Label.background"));

    // set full justification for the text
    final StyledDocument doc = getStyledDocument();
    final SimpleAttributeSet sa = new SimpleAttributeSet();
    StyleConstants.setAlignment(sa, StyleConstants.ALIGN_JUSTIFIED);
    doc.setParagraphAttributes(0, doc.getLength(), sa, false);

    //
    // This is a kludge to get around the fact that Swing layouts don't
    // support methods like getHeightForWidth(int) and so have no sensible
    // way of sizing widgets whose height and width are interdependent.
    //

    // convert the initial width from em to pixels
    final int w = width * getFont().getSize();

    // determine the preferred height at the initial width
    final Dimension d = new Dimension(w, Integer.MAX_VALUE);
    setSize(d);
    d.height = getPreferredSize().height;
    setPreferredSize(d);

    // unset the preferred size once we are laid out the first time
    addComponentListener(new ComponentAdapter() {
      @Override
      public void componentResized(ComponentEvent e) {
        setPreferredSize(null);
        removeComponentListener(this);
      }
    });

    //
    // end of preferred size kludge
    //
  }

  /** {@inheritDoc} */
  @Override
  public void setText(String text) {
    // check for HTML
    if (BasicHTML.isHTMLString(text)) setContentType("text/html");
    super.setText(text);
  }

  public static void main(String[] args) {
    final String loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."; //NON-NLS

    final JDialog d = new JDialog();
    d.setTitle("Flow Label Test"); //NON-NLS
    d.setModal(true);
    d.setResizable(true);
    d.setLocationRelativeTo(null);
    d.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
    d.add(new FlowLabel(loremIpsum + "\n\n" + loremIpsum));
    d.pack();
    d.setVisible(true);
  }
}
