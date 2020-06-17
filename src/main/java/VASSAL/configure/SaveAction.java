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
package VASSAL.configure;

import java.net.URL;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;

import VASSAL.i18n.Resources;

/**
 * General-purpose "Save" action
 */
public abstract class SaveAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  protected String parentType = "";

  public SaveAction() {
    final URL iconURL = getClass().getResource("/images/Save16.gif");
    if (iconURL != null) {
      putValue(Action.SMALL_ICON, new ImageIcon(iconURL));
    }

    putValue(Action.NAME, Resources.getString("Editor.save", parentType));
    putValue(Action.SHORT_DESCRIPTION, Resources.getString("Editor.save", parentType));
  }

  public void setParent(String p) {
    parentType = p;
  }
}
