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
package VASSAL.preferences;

import java.awt.Window;
import java.awt.event.ComponentEvent;
import java.util.StringTokenizer;

/**
 * A Preferences option controlling the visibility of a window
 */
public class VisibilityOption extends PositionOption {
  private boolean isVisible = true;
  public VisibilityOption(String key, Window f) {
    super(key, f);
    isVisible = f.isVisible();
  }

  public void componentShown(ComponentEvent e) {
    isVisible = true;
  }

  public void componentHidden(ComponentEvent e) {
    isVisible = false;
  }

  public void setValue(String in) {
    StringTokenizer st = new StringTokenizer(in,"\t");
    super.setValue(st.nextToken());
    if (st.hasMoreTokens()) {
      isVisible = "true".equals(st.nextToken());
    }
    else {
      isVisible = true;
    }
    theFrame.setVisible(isVisible);
  }

  public String getValueString() {
    return super.getValueString()+"\t"+isVisible;
  }
}
