/*
 * $Id$
 *
 * Copyright (c) 2006 by Joel Uckelman
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
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

import javax.swing.JScrollBar;
import javax.swing.JScrollPane;

/**
 ScrollPane extends JScrollPane to have complete mouse-wheel functionality.
 The mouse wheel scrolls vertically when hovering over the view or vertical
 scrollbar, and horizontally when CTRL is depressed or when hovering over
 the horizontal scrollbar. ScrollPane uses the system default scroll speed.
 For scrollpanes which will contain large images (e.g., maps) use
 {@link AdjustableSpeedScrollPane}, which has a user-configurable scroll
 speed, instead.

 @author Joel Uckelman
 @see javax.swing.JScrollPane
 @see VASSAL.tools.AdjustableSpeedScrollPane
*/
public class ScrollPane extends JScrollPane {
  private static final long serialVersionUID = 1L;

  /**
    Creates an empty (no viewport view) ScrollPane where both horizontal
    and vertical scrollbars appear when needed.
  */
  public ScrollPane() {
    this(null, VERTICAL_SCROLLBAR_AS_NEEDED,
        HORIZONTAL_SCROLLBAR_AS_NEEDED);
  }

  /**
    Creates a ScrollPane that displays the contents of the specified
    component, where both horizontal and vertical scrollbars appear whenever
    the component's contents are larger than the view.

    @param view the component to display in the scrollpane's viewport
  */
  public ScrollPane(Component view) {
    this(view, VERTICAL_SCROLLBAR_AS_NEEDED,
        HORIZONTAL_SCROLLBAR_AS_NEEDED);
  }

  /**
    Creates an empty (no viewport view) ScrollPane with specified scrollbar
    policies. The available policy settings are listed at
    {@link JScrollPane#setVerticalScrollBarPolicy} and
    {@link JScrollPane#setHorizontalScrollBarPolicy}.

    @param vsbPolicy an integer that specifies the vertical scrollbar policy
    @param hsbPolicy an integer that specifies the horizontal scrollbar policy
  */
  public ScrollPane(int vsbPolicy, int hsbPolicy) {
    this(null, vsbPolicy, hsbPolicy);
  }

  /**
    Creates a ScrollPane that displays the view component in a viewport with
    the specified scrollbar policies. The available policy settings are
    listed at {@link JScrollPane#setVerticalScrollBarPolicy} and
    {@link JScrollPane#setHorizontalScrollBarPolicy}.

    @param view the component to display in the scrollpane's viewport
    @param vsbPolicy an integer that specifies the vertical scrollbar policy
    @param hsbPolicy an integer that specifies the horizontal scrollbar policy
  */
  public ScrollPane(Component view, int vsbPolicy, int hsbPolicy) {
    super(view, vsbPolicy, hsbPolicy);

    // clear existing MouseWheelListeners
    MouseWheelListener[] listeners;

    listeners = getMouseWheelListeners();
    for (int i = 0; i < listeners.length; ++i)
      removeMouseWheelListener(listeners[i]);

    listeners = viewport.getMouseWheelListeners();
    for (int i = 0; i < listeners.length; ++i)
      viewport.removeMouseWheelListener(listeners[i]);

    listeners = verticalScrollBar.getMouseWheelListeners();
    for (int i = 0; i < listeners.length; ++i)
      verticalScrollBar.removeMouseWheelListener(listeners[i]);

    listeners = horizontalScrollBar.getMouseWheelListeners();
    for (int i = 0; i < listeners.length; ++i)
      horizontalScrollBar.removeMouseWheelListener(listeners[i]);

    // add our own MouseWheelListeners
    // NB: block scrolling isn't used with the mouse wheel
    viewport.addMouseWheelListener(new MouseWheelListener() {
      public void mouseWheelMoved(MouseWheelEvent e) {
        if (e.getScrollAmount() == 0) return;

        if (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) {
          final JScrollBar bar = e.isShiftDown() ?
            horizontalScrollBar : verticalScrollBar;
          if (bar == null || !bar.isVisible()) return;

          bar.setValue(
            bar.getValue() +
            e.getUnitsToScroll() *
            bar.getUnitIncrement()
          );
        }
      }
    });

    verticalScrollBar.addMouseWheelListener(new MouseWheelListener() {
      public void mouseWheelMoved(MouseWheelEvent e) {
        if (e.getScrollAmount() == 0) return;

        if (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) {
          verticalScrollBar.setValue(
            verticalScrollBar.getValue() +
            e.getUnitsToScroll() *
            verticalScrollBar.getUnitIncrement()
          );
        }
      }
    });

    horizontalScrollBar.addMouseWheelListener(new MouseWheelListener() {
      public void mouseWheelMoved(MouseWheelEvent e) {
        if (e.getScrollAmount() == 0) return;

        if (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) {
          horizontalScrollBar.setValue(
            horizontalScrollBar.getValue() +
            e.getUnitsToScroll() *
            horizontalScrollBar.getUnitIncrement()
          );
        }
      }
    });
  }
}
