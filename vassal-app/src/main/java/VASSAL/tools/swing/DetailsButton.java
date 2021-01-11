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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.swing;

import VASSAL.build.GameModule;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

import net.miginfocom.swing.MigLayout;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class DetailsButton extends JButton {
  private static final long serialVersionUID = 1L;

  protected static final Icon collapsedIcon =
    UIManager.getIcon("Tree.collapsedIcon");
  protected static final Icon expandedIcon =
    UIManager.getIcon("Tree.expandedIcon");

  protected String showText;
  protected String hideText;

  protected Component expander;
  protected Component buddy;

  protected static int eh = 300;

  public DetailsButton(String showText, String hideText) {
    this(showText, hideText, null, null);
  }

  public DetailsButton(String showText, String hideText, Component expander) {
    this(showText, hideText, expander, null);
  }

  public DetailsButton(String showText, String hideText,
                       Component expander, Component buddy) {
    this.showText = showText;
    this.hideText = hideText;

    if (expander != null) setExpander(expander);

    setAction(new AbstractAction(showText, collapsedIcon) {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        setExpanded(!DetailsButton.this.expander.isVisible());
      }
    });

    if (buddy != null) setBuddy(buddy);

    setBorderPainted(false);
    setContentAreaFilled(false);
  }

  public void setExpander(Component comp) {
    if (expander == null) comp.setVisible(false);
    expander = comp;
  }

  public void setButtonShowText(String text) {
    showText = text;
    if (!expander.isVisible()) setText(showText);
  }

  public void setButtonHideText(String text) {
    hideText = text;
    if (expander.isVisible()) setText(hideText);
  }

  /**
   * Sets the buddy component for the expanding component.
   * The width of the expanding component is adjusted to match the width of
   * the buddy component when the expanding component is invisible.
   *
   * @param comp the buddy component
   */
  public void setBuddy(Component comp) {
    buddy = comp;

    buddy.addComponentListener(new ComponentAdapter() {
      @Override
      public void componentResized(ComponentEvent e) {
        if (!expander.isVisible()) {
          expander.setSize(buddy.getWidth(), expander.getHeight());
        }
      }
    });
  }

  public void setExpanded(boolean expanded) {
    final Window w = SwingUtilities.getWindowAncestor(expander);
    final Dimension ws = w.getSize();

    if (!expander.isVisible()) {
      setText(hideText);
      setIcon(expandedIcon);
      expander.setVisible(true);

      ws.height += eh;

      if (!expander.isPreferredSizeSet()) {
        expander.setSize(buddy.getWidth(), 300);
      }
    }
    else {
      setText(showText);
      setIcon(collapsedIcon);
      eh = expander.getHeight();
      expander.setVisible(false);
      ws.height -= eh;
    }

    fixSize(w);
    w.setSize(ws);
    w.doLayout();
  }

  protected void fixSize(Container c) {
    for (final Component comp : c.getComponents()) {
      if (comp != expander && comp instanceof Container) {
        final Container con = (Container) comp;

        if (!con.isAncestorOf(expander)) {
          final Dimension d = con.getSize();
          con.setPreferredSize(d);
        }

        if (!(con instanceof JScrollPane)) fixSize(con);
      }
      else {
        final Dimension d = comp.getSize();
        comp.setPreferredSize(d);
      }
    }
  }

  public static void main(String[] args) {
    final String loremIpsum = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.\n\nLorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."; //NON-NLS

    SwingUtilities.invokeLater(() -> {
      final JTextArea a = new JTextArea(loremIpsum, 25, 80);
      a.setLineWrap(true);
      a.setWrapStyleWord(true);

      final JScrollPane sp1 = new JScrollPane(a);

      final JTextArea b = new JTextArea(loremIpsum, 25, 80);
      b.setLineWrap(true);
      b.setWrapStyleWord(true);

      final JScrollPane sp2 = new JScrollPane(b);

      final DetailsButton db = new DetailsButton("Show", "Hide", sp2); //NON-NLS
      db.setBuddy(sp1);

      final JPanel contents = new JPanel();
      contents.setLayout(new MigLayout("hidemode 3", "", "[]unrel[]rel[]")); //NON-NLS
      contents.add(sp1, "cell 0 0, grow, push"); //NON-NLS
      contents.add(db, "cell 0 1"); //NON-NLS
      contents.add(sp2, "cell 0 2, grow, push"); //NON-NLS

      final JDialog d = new JDialog(GameModule.getGameModule().getPlayerWindow());
      d.add(contents);
      d.setResizable(true);
      d.setLocationRelativeTo(null);
      d.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
      d.pack();
      d.setVisible(true);
    });

    SwingUtilities.invokeLater(() -> {
      final JLabel a = new JLabel("This is an expanding pane."); //NON-NLS

      final JTextArea b = new JTextArea(loremIpsum, 25, 80);
      b.setLineWrap(true);
      b.setWrapStyleWord(true);

      final JScrollPane sp = new JScrollPane(b);

      final DetailsButton db = new DetailsButton("Show", "Hide", sp); //NON-NLS
      db.setBuddy(a);

      final JPanel contents = new JPanel();
      contents.setLayout(
        new MigLayout("hidemode 3", "", "[]unrel[]rel[]unrel[]")); //NON-NLS
      contents.add(a, "cell 0 0, growx, pushx"); //NON-NLS
      contents.add(db, "cell 0 1"); //NON-NLS
      contents.add(sp, "cell 0 2, grow, push"); //NON-NLS
      contents.add(new JCheckBox("Disable?"), "cell 0 3"); //NON-NLS

      final JDialog d = new JOptionPane(
        contents,
        JOptionPane.ERROR_MESSAGE,
        JOptionPane.DEFAULT_OPTION
      ).createDialog(null, "Test"); //NON-NLS

      d.setModal(true);
      d.setResizable(true);
      d.setLocationRelativeTo(null);
      d.setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
      d.pack();
      d.setVisible(true);
    });
  }
}
