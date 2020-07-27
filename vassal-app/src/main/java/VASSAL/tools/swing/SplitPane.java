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

import java.awt.Component;

import VASSAL.tools.ComponentSplitter;

/**
 * Contains methods to automatically show/hide one of its components (the "hideable" component) while the other (the
 * "base" component) remains always visible. Can optionally change the size of its top level ancestorExtension when
 * the component is shown/hidden. The hideable component is initially hidden
 *
 * TODO: remove {@link InternalSplitPane} and {@link ComponentSplitter.SplitPane}, move the code into this class after
 * modules stop using them
 */
public class SplitPane extends ComponentSplitter.SplitPane {

  /**
   * Initialize the SplitPane with the two component
   *
   * @param hideableComponent
   * @param baseComponent
   * @param hideablePosition         one of {@link #HIDE_TOP}, {@link #HIDE_BOTTOM}, {@link #HIDE_LEFT} or {@link #HIDE_RIGHT}
   * @param resizeOnVisibilityChange
   */
  public SplitPane(Component hideableComponent, Component baseComponent, int hideablePosition,
                   boolean resizeOnVisibilityChange) {
    super(hideableComponent, baseComponent, hideablePosition, resizeOnVisibilityChange);
  }
}
