/*
 * $Id$
 *
 * Copyright (c) 2008-2009 Brent Easton
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

import javax.swing.Action;
import javax.swing.JButton;

import VASSAL.i18n.Resources;

/**
 * Produce standard Vassal buttons.
 *
 */
public class ButtonFactory {

  public static JButton getOkButton() {
    JButton button = new JButton(Resources.getString(Resources.OK));
    button.setToolTipText(Resources.getString(Resources.OK));
    return button;
  }

  public static JButton getCancelButton() {
    JButton button =  new JButton(Resources.getString(Resources.CANCEL));
    button.setToolTipText(Resources.getString(Resources.CANCEL));
    return button;
  }

  public static JButton getHelpButton() {
    JButton button =  new JButton(Resources.getString(Resources.HELP));
    button.setToolTipText(Resources.getString(Resources.HELP));
    return button;
  }

  public static JButton getHelpButton(Action a) {
    JButton button = getHelpButton();
    button.setAction(a);
    return button;
  }

}