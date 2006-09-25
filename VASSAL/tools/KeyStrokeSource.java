/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import javax.swing.JComponent;

/**
 * Utility class for registering a component as a source of hotkey events
 * @see VASSAL.build.GameModule#addKeyStrokeSource
 * @see VASSAL.build.GameModule#addKeyStrokeListener
 */
public class KeyStrokeSource {
  private JComponent c;
  private int mode;

  public KeyStrokeSource(JComponent c, int mode) {
    this.c = c;
    this.mode = mode;
  }

  public JComponent getComponent() {
    return c;
  }

  public int getMode() {
    return mode;
  }
}
