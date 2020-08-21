/*
 *
 * Copyright (c) 2020 by Vassal developers
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

import javax.swing.JComponent;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;

import VASSAL.Info;
import VASSAL.tools.ComponentSplitter;

/**
 * !! This class is for internal use only, modules should use {@link SplitPane} !!
 *
 * <p>Contains methods to automatically show/hide one of its components (the "hideable" component)
 * while the other (the "base" component) remains always visible. Can optionally change the size of
 * its top level ancestorExtension when the component is shown/hidden. The hideable component is
 * initially hidden
 *
 * @deprecated remove this class and {@link ComponentSplitter.SplitPane}, move the code into {@link SplitPane}
 * after modules stop using them
 */
@Deprecated
public class InternalSplitPane extends JSplitPane {
  private static final long serialVersionUID = 1L;

  private boolean resizeOnVisibilityChange;
  private int hideablePosition;
  public static final int HIDE_TOP = 0;
  public static final int HIDE_BOTTOM = 1;
  public static final int HIDE_LEFT = 2;
  public static final int HIDE_RIGHT = 3;
  private List<InternalSplitPane> showingTransverseComponents =
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
  public InternalSplitPane(Component hideableComponent, Component baseComponent, int hideablePosition, boolean resizeOnVisibilityChange) {
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
      InternalSplitPane split = getTransverseSplit();
      if (split != null) {
        split.hideTransverseComponent(this);
      }
    }
  }

  /**
   * Set the divider location and/or the top-level ancestor size to be large enough to display the argument
   * {@link InternalSplitPane}'s hideable component
   *
   * @param split
   */
  protected void showTransverseComponent(InternalSplitPane split) {
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
    for (InternalSplitPane split : showingTransverseComponents) {
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
  protected void hideTransverseComponent(InternalSplitPane split) {
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
        div = prefBSize.width / (double)(prefBSize.width + prefHSize.width);
        break;
      case JSplitPane.VERTICAL_SPLIT:
        w = ancestorSize.width;
        h = Math.min(
          ancestorSize.height + prefHSize.height,
          screenBounds.height - ancestorPos.y
        );
        div = prefBSize.height / (double)(prefBSize.height + prefHSize.height);
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

      final InternalSplitPane split = getTransverseSplit();
      if (split != null) {
        split.showTransverseComponent(InternalSplitPane.this);
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
  public InternalSplitPane getTransverseSplit() {
    InternalSplitPane split = null;
    for (Component c = getParent(); c != null; c = c.getParent()) {
      if (c instanceof InternalSplitPane) {
        InternalSplitPane p = (InternalSplitPane) c;
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
