/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Insets;

/**
 * FlowLayout subclass that fully supports wrapping of components.
 */
public class WrapLayout extends FlowLayout {
  private static final long serialVersionUID = 1L;

  // The preferred size for this container.
  private Dimension preferredLayoutSize;

  /**
   * Constructs a new <code>WrapLayout</code> with a left
   * alignment and a default 5-unit horizontal and vertical gap.
   */
  public WrapLayout() {
    super(LEFT);
  }

  /**
   * Constructs a new <code>FlowLayout</code> with the specified
   * alignment and a default 5-unit horizontal and vertical gap.
   * The value of the alignment argument must be one of
   * <code>WrapLayout</code>, <code>WrapLayout</code>,
   * or <code>WrapLayout</code>.
   * @param align the alignment value
   */
  public WrapLayout(int align) {
    super(align);
  }

  /**
   * Creates a new flow layout manager with the indicated alignment
   * and the indicated horizontal and vertical gaps.
   * <p>
   * The value of the alignment argument must be one of
   * <code>WrapLayout</code>, <code>WrapLayout</code>,
   * or <code>WrapLayout</code>.
   * @param align the alignment value
   * @param hgap the horizontal gap between components
   * @param vgap the vertical gap between components
   */
  public WrapLayout(int align, int hgap, int vgap) {
    super(align, hgap, vgap);
  }

  /**
   * Returns the preferred dimensions for this layout given the
   * <i>visible</i> components in the specified target container.
   * @param target the component which needs to be laid out
   * @return the preferred dimensions to lay out the
   * subcomponents of the specified container
   */
  @Override
  public Dimension preferredLayoutSize(Container target) {
    return layoutSize(target, true);
  }

  /**
   * Returns the minimum dimensions needed to layout the <i>visible</i>
   * components contained in the specified target container.
   * @param target the component which needs to be laid out
   * @return the minimum dimensions to lay out the
   * subcomponents of the specified container
   */
  @Override
  public Dimension minimumLayoutSize(Container target) {
    return layoutSize(target, false);
  }

  /**
   * Returns the minimum or preferred dimension needed to layout the target
   * container.
   *
   * @param target target to get layout size for
   * @param preferred should preferred size be calculated
   * @return the dimension to layout the target container
   */
  private Dimension layoutSize(Container target, boolean preferred) {
    synchronized (target.getTreeLock()) {
      //  Each row must fit with the width allocated to the containter.
      //  When the container width = 0, the preferred width of the container
      //  has not yet been calculated so lets ask for the maximum.

      Dimension targetSize = target.getSize();
      int targetWidth = 0;

      if (targetSize.width == 0)
        targetWidth = Integer.MAX_VALUE;
      else
        targetWidth = targetSize.width;

      int hgap = getHgap();
      int vgap = getVgap();
      Insets insets = target.getInsets();
      int maxWidth = targetWidth - (insets.left + insets.right + hgap * 2);

      //  Fit components into the allowed width

      Dimension dim = new Dimension(0, 0);
      int rowWidth = 0;
      int rowHeight = 0;

      int nmembers = target.getComponentCount();

      for (int i = 0; i < nmembers; i++) {
        Component m = target.getComponent(i);

        if (m.isVisible()) {
          Dimension d = preferred ? m.getPreferredSize() : m.getMinimumSize();

          if (rowWidth + d.width > maxWidth) {
            addRow(dim, rowWidth, rowHeight);
            rowWidth = 0;
            rowHeight = 0;
          }

          if (rowWidth > 0) {
            rowWidth += getHgap();
          }

          rowWidth += d.width;
          rowHeight = Math.max(rowHeight, d.height);
        }
      }

      addRow(dim, rowWidth, rowHeight);

      dim.width += insets.left + insets.right + hgap * 2;
      dim.height += insets.top + insets.bottom + vgap * 2;

      return dim;
    }
  }

  /**
   * Add a new row to the given dimension.
   *
   * @param dim dimension to add row to
   * @param rowWidth the width of the row to add
   * @param rowHeight the height of the row to add
   */
  private void addRow(Dimension dim, int rowWidth, int rowHeight) {
    dim.width = Math.max(dim.width, rowWidth);

    if (dim.height > 0) {
      dim.height += getVgap();
    }

    dim.height += rowHeight;
  }

  /**
   * Lays out the container. This method lets each component take
   * its preferred size by reshaping the components in the
   * target container in order to satisfy the alignment of
   * this <code>WrapLayout</code> object.
   * @param target the specified component being laid out
   */
  @Override
  public void layoutContainer(final Container target) {
    Dimension size = preferredLayoutSize(target);

    if (size.equals(preferredLayoutSize)) {
      super.layoutContainer(target);
    }
    else {
      preferredLayoutSize = size;
      target.invalidate();
      Container top = target;

      while (top.getParent() != null) {
        top = top.getParent();
      }

      top.validate();
    }
  }
}
