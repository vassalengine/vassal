/*
 *
 * Copyright (c) 2003 by Rodney Kinney
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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;

/**
 * Provides support for hidden panels. Use the split methods to create an
 * instance of {@link SplitPane}, which can then be manipulated to show and
 * hide the panel
 */
public class ComponentSplitter {
  /**
   * Create a new hideable panel to the right of the base component. The base component is replaced by a
   * {@link SplitPane}
   *
   * @param base
   *          the base component
   * @param hideableComponent
   *          the hideable component
   * @param resizeOnVisibilityChange
   *          If true, the containing window will expand or shrink to an appropriate size when the hideable component is
   *          shown or hidden
   * @return the {@link SplitPane} containing the two components
   * @deprecated Use {@link #split(Component, Component, int, boolean)} instead.
   */
  @Deprecated
  public SplitPane splitRight(Component base, Component hideableComponent, boolean resizeOnVisibilityChange) {
    return split(base, hideableComponent, SplitPane.HIDE_RIGHT, resizeOnVisibilityChange);
  }

  /**
   * Create a new hideable panel to the left of the base component. The base component is replaced by a
   * {@link SplitPane}
   *
   * @param base
   *          the base component
   * @param hideableComponent
   *          the hideable component
   * @param resizeOnVisibilityChange
   *          If true, the containing window will expand or shrink to an appropriate size when the hideable component is
   *          shown or hidden
   * @return the {@link SplitPane} containing the two components
   * @deprecated Use {@link #split(Component, Component, int, boolean)} instead.
   */
  @Deprecated
  public SplitPane splitLeft(Component base, Component hideableComponent, boolean resizeOnVisibilityChange) {
    return split(base, hideableComponent, SplitPane.HIDE_LEFT, resizeOnVisibilityChange);
  }

  /**
   * Create a new hideable panel to the bottom of the base component. The base component is replaced by a
   * {@link SplitPane}
   *
   * @param base
   *          the base component
   * @param hideableComponent
   *          the hideable component
   * @param resizeOnVisibilityChange
   *          If true, the containing window will expand or shrink to an appropriate size when the hideable component is
   *          shown or hidden
   * @return the {@link SplitPane} containing the two components
   * @deprecated Use {@link #split(Component, Component, int, boolean)} instead.
   */
  @Deprecated
  public SplitPane splitBottom(Component base, Component hideableComponent, boolean resizeOnVisibilityChange) {
    return split(base, hideableComponent, SplitPane.HIDE_BOTTOM, resizeOnVisibilityChange);
  }

  /**
   * Create a new hideable panel to the top of the base component. The base component is replaced by a {@link SplitPane}
   *
   * @param base
   *          the base component
   * @param hideableComponent
   *          the hideable component
   * @param resizeOnVisibilityChange
   *          If true, the containing window will expand or shrink to an appropriate size when the hideable component is
   *          shown or hidden
   * @return the {@link SplitPane} containing the two components
   * @deprecated Use {@link #split(Component, Component, int, boolean)} instead.
   */
  @Deprecated
  public SplitPane splitTop(Component base, Component hideableComponent, boolean resizeOnVisibilityChange) {
    return split(base, hideableComponent, SplitPane.HIDE_TOP, resizeOnVisibilityChange);
  }

  /**
   * Search the containment hierarchy for the index-th {@link SplitPane}
   * ancestor of a target component
   *
   * @param c
   *          the target component
   * @param index
   *          If -1, return the last {@link SplitPane} ancestor
   * @return the {@link SplitPane} ancestor, or the original component if none is found
   */
  @Deprecated
  public Component getSplitAncestor(Component c, int index) {
    return splitAncestorOf(c, index);
  }

  /**
   * Search the containment hierarchy for the ith {@link SplitPane}
   * ancestor of a target component
   *
   * @param c the target component
   * @param index If -1, return the last {@link SplitPane} ancestor
   * @return the {@link SplitPane} ancestor, or the original component if
   * none is found
   *
   * @deprecated deprecated without replacement
   */
  @Deprecated
  public static Component splitAncestorOf(Component c, int index) {
    Component next = SwingUtilities.getAncestorOfClass(SplitPane.class, c);
    int count = -1;
    while (next != null && (index < 0 || count++ < index)) {
      c = next;
      next = SwingUtilities.getAncestorOfClass(SplitPane.class, c);
    }
    return c;
  }

  /**
   * Create a new hideable panel beside the base component. The base component
   * is replaced by a {@link SplitPane}
   *
   * @param base the base component
   * @param newComponent the hideable component, one of
   *   {@link SplitPane#HIDE_TOP}, {@link SplitPane#HIDE_RIGHT},
   *   {@link SplitPane#HIDE_BOTTOM}, {@link SplitPane#HIDE_LEFT}
   * @param hideablePosition the position of the hideable component
   * @param resize If true, the containing window will
   *   expand or shrink to an appropriate size when the hideable component is
   *   shown or hidden
   * @return the {@link SplitPane} containing the two components
   *
   * @deprecated deprecated without replacement, modules should subclass/change the {@link Component}s instead
   */
  @Deprecated
  public static SplitPane split(
    Component base,
    Component newComponent,
    int hideablePosition,
    boolean resize) {

    int index = -1;
    Container parent = base.getParent();
    if (base.getParent() != null) {
      for (int i = 0, n = base.getParent().getComponentCount(); i < n; ++i) {
        if (base == base.getParent().getComponent(i)) {
          index = i;
          break;
        }
      }
    }

    final SplitPane split = new SplitPane(newComponent, base, hideablePosition, resize);
    if (index >= 0) {
      parent.add(split, index);
    }
    return split;
  }

  /**
   * For internal use only, modules should subclass/change the {@link Component}s instead
   */
  public static VASSAL.tools.swing.SplitPane splitAsNewSplitPane(
      Component base, Component newComponent, int hideablePosition, boolean resize) {

    int index = -1;
    Container parent = base.getParent();
    if (base.getParent() != null) {
      for (int i = 0, n = base.getParent().getComponentCount(); i < n; ++i) {
        if (base == base.getParent().getComponent(i)) {
          index = i;
          break;
        }
      }
    }

    final VASSAL.tools.swing.SplitPane split = new VASSAL.tools.swing.SplitPane(newComponent, base, hideablePosition, resize);
    if (index >= 0) {
      parent.add(split, index);
    }
    return split;
  }

  /**
   * @deprecated use {@link VASSAL.tools.swing.SplitPane}
   */
  @Deprecated
  public static class SplitPane extends VASSAL.tools.swing.InternalSplitPane {

    /**
     * Initialize the SplitPane with the two component
     *
     * @param hideableComponent
     * @param baseComponent
     * @param hideablePosition         one of {@link #HIDE_TOP}, {@link #HIDE_BOTTOM}, {@link #HIDE_LEFT} or {@link #HIDE_RIGHT}
     * @param resizeOnVisibilityChange
     *
     * @deprecated use {@link VASSAL.tools.swing.SplitPane}
     */
    @Deprecated
    public SplitPane(Component hideableComponent, Component baseComponent, int hideablePosition,
                     boolean resizeOnVisibilityChange) {
      super(hideableComponent, baseComponent, hideablePosition, resizeOnVisibilityChange);
    }
  }

  public static void main(String[] args) {
    JFrame f = new JFrame();
    f.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
    f.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        System.exit(0);
      }
    });
    JTextField status = new JTextField("status");
    status.setEditable(false);
    f.setLayout(new BorderLayout());
    Box box = Box.createVerticalBox();
    box.add(status);
    JPanel main = new JPanel(new BorderLayout());
    f.add(main, BorderLayout.CENTER);
    JToolBar toolbar = new JToolBar();
    toolbar.setFloatable(false);
    toolbar.setAlignmentX(0.0F);
    box.add(toolbar);
    f.add(box, BorderLayout.NORTH);
    final JLabel smallLeft = new JLabel(new ImageIcon("small.gif"));
    final JLabel smallRight = new JLabel(new ImageIcon("smallRight.gif"));
    final JLabel large = new JLabel(new ImageIcon("large.jpg"));
    JPanel text = new JPanel();
    text.setLayout(new BoxLayout(text, BoxLayout.Y_AXIS));
    text.add(new ScrollPane(new JTextArea(15, 60)));
    JTextField input = new JTextField(60);
    input.setMaximumSize(new Dimension(input.getMaximumSize().width, input.getPreferredSize().height));
    text.add(input);
    final SplitPane splitRight = ComponentSplitter.split(main, smallRight, VASSAL.tools.swing.SplitPane.HIDE_RIGHT, false);
    final SplitPane splitLeft = ComponentSplitter.split(main, smallLeft, VASSAL.tools.swing.SplitPane.HIDE_LEFT, false);
    final SplitPane splitBottom = ComponentSplitter.split(ComponentSplitter.splitAncestorOf(main, -1), new ScrollPane(large), VASSAL.tools.swing.SplitPane.HIDE_BOTTOM, true);
    splitBottom.setResizeWeight(0.0);
    main.add(text, BorderLayout.CENTER);
    toolbar.add(new AbstractAction("Left") {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        splitLeft.toggleVisibility();
      }
    });
    toolbar.add(new AbstractAction("Right") {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        splitRight.toggleVisibility();
      }
    });
    toolbar.add(new AbstractAction("Bottom") {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        splitBottom.toggleVisibility();
      }
    });
    f.pack();
    f.setVisible(true);
  }
}
