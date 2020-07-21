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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import VASSAL.Info;

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
   */
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
   * @param hideableComponent the hideable component, one of
   *   {@link SplitPane.HIDE_TOP}, {@link SplitPane.HIDE_RIGHT},
   *   {@link SplitPane.HIDE_BOTTOM}, {@link SplitPane.HIDE_LEFT}
   * @param hideablePosition the position of the hideable component
   * @param resizeOnVisibilityChange If true, the containing window will
   *   expand or shrink to an appropriate size when the hideable component is
   *   shown or hidden
   * @return the {@link SplitPane} containing the two components
   */
  public static SplitPane split(
    Component base,
    Component newComponent,
    int hideablePosition,
    boolean resize)
  {
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
   * Contains methods to automatically show/hide one of its components (the "hideable" component) while the other (the
   * "base" component) remains always visible. Can optionally change the size of its top level ancestorExtension when
   * the component is shown/hidden. The hideable component is initially hidden
   */
  public static class SplitPane extends JSplitPane {
    private static final long serialVersionUID = 1L;

    private boolean resizeOnVisibilityChange;
    private int hideablePosition;
    public static final int HIDE_TOP = 0;
    public static final int HIDE_BOTTOM = 1;
    public static final int HIDE_LEFT = 2;
    public static final int HIDE_RIGHT = 3;
    private List<SplitPane> showingTransverseComponents =
      new ArrayList<>();
    private int transverseHiddenSize;

    /**
     * Initialize the SplitPane with the two component
     *
     * @param hideableComponent
     * @param baseComponent
     * @param hideablePosition
     *          one of {@link #HIDE_TOP}, {@link #HIDE_BOTTOM}, {@link #HIDE_LEFT} or {@link #HIDE_RIGHT}
     * @param resizeOnVisibilityChange
     *          If true, resize the top-level ancestor when the hideable component is shown/hidden
     */
    public SplitPane(Component hideableComponent, Component baseComponent, int hideablePosition, boolean resizeOnVisibilityChange) {
      super(HIDE_TOP == hideablePosition || HIDE_BOTTOM == hideablePosition ? VERTICAL_SPLIT : HORIZONTAL_SPLIT);
      this.resizeOnVisibilityChange = resizeOnVisibilityChange;
      this.hideablePosition = hideablePosition;
      if (hideableComponent instanceof JComponent) {
        hideableComponent.setMinimumSize(new Dimension(0, 0));
      }
      switch (hideablePosition) {
      case HIDE_LEFT:
        setLeftComponent(hideableComponent);
        setRightComponent(baseComponent);
        break;
      case HIDE_RIGHT:
        setRightComponent(hideableComponent);
        setLeftComponent(baseComponent);
        break;
      case HIDE_TOP:
        setTopComponent(hideableComponent);
        setBottomComponent(baseComponent);
        break;
      case HIDE_BOTTOM:
        setBottomComponent(hideableComponent);
        setTopComponent(baseComponent);
      }
      setBorder(null);
      setResizeWeight(HIDE_LEFT == hideablePosition || HIDE_TOP == hideablePosition ? 0.0 : 1.0);
      hideComponent();
    }

    /** Toggle the visibility of the hideable component */
    public void toggleVisibility() {
      if (getHideableComponent().isVisible()) {
        hideComponent();
      }
      else {
        showComponent();
      }
    }

    /**
     * @return the Component that can be shown/hidden
     */
    public Component getHideableComponent() {
      Component c = null;
      switch (hideablePosition) {
      case HIDE_LEFT:
        c = getLeftComponent();
        break;
      case HIDE_RIGHT:
        c = getRightComponent();
        break;
      case HIDE_TOP:
        c = getTopComponent();
        break;
      case HIDE_BOTTOM:
        c = getBottomComponent();
      }
      return c;
    }

    /**
     * @return the Component that remains always visible
     */
    public Component getBaseComponent() {
      Component c = null;
      switch (hideablePosition) {
      case HIDE_LEFT:
        c = getRightComponent();
        break;
      case HIDE_RIGHT:
        c = getLeftComponent();
        break;
      case HIDE_TOP:
        c = getBottomComponent();
        break;
      case HIDE_BOTTOM:
        c = getTopComponent();
      }
      return c;
    }

    /**
     * @return the size of the base component along the axis of orientation
     */
    protected int getBaseComponentSize() {
      int size = -1;
      switch (getOrientation()) {
      case VERTICAL_SPLIT:
        size = getBaseComponent().getSize().height;
        break;
      case HORIZONTAL_SPLIT:
        size = getBaseComponent().getSize().width;
      }
      return size;
    }

    /**
     *
     * @return the size of the hideable component along the axis of orientation
     */
    protected int getHideableComponentSize() {
      int size = -1;
      switch (getOrientation()) {
      case VERTICAL_SPLIT:
        size = getHideableComponent().getSize().height;
        break;
      case HORIZONTAL_SPLIT:
        size = getHideableComponent().getSize().width;
      }
      return size;
    }

    /** Hide the hideable component */
    public void hideComponent() {
      if (getHideableComponent().isVisible()) {
        if (resizeOnVisibilityChange) {
          Container ancestor = getTopLevelAncestor();
          if (ancestor != null) {
            switch (hideablePosition) {
            case HIDE_LEFT:
            case HIDE_RIGHT:
              ancestor.setSize(new Dimension(ancestor.getSize().width - getHideableComponent().getSize().width, ancestor.getSize().height - getDividerSize()));
              break;
            case HIDE_TOP:
            case HIDE_BOTTOM:
              ancestor.setSize(new Dimension(ancestor.getSize().width, ancestor.getSize().height - getHideableComponent().getSize().height - getDividerSize()));
              break;
            }
            ancestor.validate();
          }
        }
        // TODO delete commented code or activate it again
        // Running later causes race conditions in the Module Manager
        //Runnable runnable = new Runnable() {
        //  public void run() {
        ((BasicSplitPaneUI) getUI()).getDivider().setVisible(false);
        getHideableComponent().setVisible(false);
        switch (hideablePosition) {
        case HIDE_LEFT:
        case HIDE_TOP:
          setDividerLocation(0.0);
          break;
        case HIDE_RIGHT:
        case HIDE_BOTTOM:
          setDividerLocation(1.0);
        }
        //  }
        //};
        //SwingUtilities.invokeLater(runnable);
        SplitPane split = getTransverseSplit();
        if (split != null) {
          split.hideTransverseComponent(this);
        }
      }
    }

    /**
     * Set the divider location and/or the top-level ancestor size to be large enough to display the argument
     * {@link SplitPane}'s hideable component
     *
     * @param split
     */
    protected void showTransverseComponent(SplitPane split) {
      if (showingTransverseComponents.isEmpty()) {
        transverseHiddenSize = getBaseComponentSize();
      }
      showingTransverseComponents.add(split);
      resizeBaseComponent();
    }

    /**
     * Set the base component size to be large enough to accomodate all descendant SplitPane's showing components
     */
    protected void resizeBaseComponent() {
      if (getHideableComponent().isVisible()) {
        switch (hideablePosition) {
        case HIDE_BOTTOM:
        case HIDE_RIGHT:
          setDividerLocation(getPreferredBaseComponentSize());
          break;
        case HIDE_TOP:
          setDividerLocation(getSize().height - getPreferredBaseComponentSize());
          break;
        case HIDE_LEFT:
          setDividerLocation(getSize().width - getPreferredBaseComponentSize());
          break;
        }
      }
      else if (resizeOnVisibilityChange && getTopLevelAncestor() != null) {
        getTopLevelAncestor().setSize(getTransverseSize());
        getTopLevelAncestor().validate();
      }
    }

    /**
     * @return the preferred size of the base component along the orientation axis
     */
    protected int getPreferredBaseComponentSize() {
      int size = transverseHiddenSize;
      for (SplitPane split : showingTransverseComponents) {
        switch (getOrientation()) {
        case VERTICAL_SPLIT:
          size = Math.max(size,
            split.getHideableComponent().getPreferredSize().height);
          break;
        case HORIZONTAL_SPLIT:
          size = Math.max(size,
            split.getHideableComponent().getPreferredSize().width);
        }
      }
      return size;
    }

    /**
     * Set the divider location and/or the top-level ancestor size to
     * the preferred transverse size.
     *
     * @param split
     */
    protected void hideTransverseComponent(SplitPane split) {
      showingTransverseComponents.remove(split);
      resizeBaseComponent();
    }

    /**
     * Return the preferred size of the top-level container in the
     * direction transverse to this SplitPane's orientation.
     * Depends on which ancestors have been shown using
     * {@link #showTransverseComponent}.
     */
    protected Dimension getTransverseSize() {
      Dimension newSize = getTopLevelAncestor().getSize();
      switch (getOrientation()) {
      case VERTICAL_SPLIT:
        newSize.height += getPreferredBaseComponentSize()
                        - getBaseComponentSize();
        break;
      case HORIZONTAL_SPLIT:
        newSize.width += getPreferredBaseComponentSize()
                       - getBaseComponentSize();
      }
      return newSize;
    }

    /**
     * Show the hideable component
     */
    public void showComponent() {
      if (getHideableComponent().isVisible()) {
        return;
      }

      if (resizeOnVisibilityChange) {
        final Container ancestor = getTopLevelAncestor();
        if (ancestor == null) {
          return;
        }

        final Rectangle screenBounds = Info.getScreenBounds(ancestor);
        final Point ancestorPos = ancestor.getLocation();
        final Dimension ancestorSize = ancestor.getSize();
        final Dimension prefHSize = getHideableComponent().getPreferredSize();
        final Dimension prefBSize = getBaseComponent().getPreferredSize();

        double div = 0.0;
        int w = 0, h = 0;
        switch (getOrientation()) {
        case JSplitPane.HORIZONTAL_SPLIT:
          w = Math.min(
            ancestorSize.width + prefHSize.width,
            screenBounds.width - ancestorPos.x
          );
          h = ancestorSize.height;
          div = prefBSize.width/(double)(prefBSize.width + prefHSize.width);
          break;
        case JSplitPane.VERTICAL_SPLIT:
          w = ancestorSize.width;
          h = Math.min(
            ancestorSize.height + prefHSize.height,
            screenBounds.height - ancestorPos.y
          );
          div = prefBSize.height/(double)(prefBSize.height + prefHSize.height);
          break;
        }

        ancestor.setSize(w, h);
        ancestor.validate();
        getHideableComponent().setVisible(true);
        ((BasicSplitPaneUI) getUI()).getDivider().setVisible(true);

        final double divPos = div;
        SwingUtilities.invokeLater(new Runnable() {
          @Override
          public void run() {
            setDividerLocation(divPos);
          }
        });
      }
      else {
        getHideableComponent().setVisible(true);
        ((BasicSplitPaneUI) getUI()).getDivider().setVisible(true);

        SwingUtilities.invokeLater(new Runnable() {
          @Override
          public void run() {
            setDividerLocation(getPreferredDividerLocation());
          }
        });

        final SplitPane split = getTransverseSplit();
        if (split != null) {
          split.showTransverseComponent(ComponentSplitter.SplitPane.this);
        }
      }
    }

    /**
     * @return the preferred location of the divider when the hideable component is visible
     */
    protected int getPreferredDividerLocation() {
      int loc = 0;
      switch (hideablePosition) {
      case HIDE_LEFT:
        loc = getInsets().left + getLeftComponent().getPreferredSize().width;
        break;
      case HIDE_RIGHT:
        loc = getSize().width - getInsets().right - getDividerSize() - getRightComponent().getPreferredSize().width;
        break;
      case HIDE_TOP:
        loc = getInsets().top + getLeftComponent().getPreferredSize().height;
        break;
      case HIDE_BOTTOM:
        loc = getSize().height - getInsets().bottom - getDividerSize() - getRightComponent().getPreferredSize().height;
      }
      return loc;
    }

    /**
     * Return the first SplitPane ancestor with a different orientation from this SplitPane
     *
     * @return
     */
    public SplitPane getTransverseSplit() {
      SplitPane split = null;
      for (Component c = getParent(); c != null; c = c.getParent()) {
        if (c instanceof SplitPane) {
          SplitPane p = (SplitPane) c;
          if (p.getOrientation() != getOrientation() && SwingUtilities.isDescendingFrom(this, p.getBaseComponent())) {
            split = p;
            break;
          }
        }
      }
      return split;
    }

    /**
     * If the hideable component is not visible, use the base component's preferred size
     */
    @Override
    public Dimension getPreferredSize() {
      Dimension d = null;
      if (getHideableComponent() == null || getHideableComponent().isVisible()) {
        d = super.getPreferredSize();
      }
      else {
        switch (hideablePosition) {
        case HIDE_LEFT:
          d = getRightComponent().getPreferredSize();
          break;
        case HIDE_RIGHT:
          d = getLeftComponent().getPreferredSize();
          break;
        case HIDE_TOP:
          d = getBottomComponent().getPreferredSize();
          break;
        case HIDE_BOTTOM:
          d = getTopComponent().getPreferredSize();
        }
      }
      return d;
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
    final SplitPane splitRight = ComponentSplitter.split(main, smallRight, SplitPane.HIDE_RIGHT, false);
    final SplitPane splitLeft = ComponentSplitter.split(main, smallLeft, SplitPane.HIDE_LEFT, false);
    final SplitPane splitBottom = ComponentSplitter.split(ComponentSplitter.splitAncestorOf(main, -1), new ScrollPane(large), SplitPane.HIDE_BOTTOM, true);
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
