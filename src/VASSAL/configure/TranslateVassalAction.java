/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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

// FIXME: Why is this in configure and not i18n?
package VASSAL.configure;

import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslateVassalWindow;

/**
 * Brings up a window in which the user can provide a foreign-language
 * translation of the VASSAL program.
 *
 * @author rodneykinney
 */
public class TranslateVassalAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private Frame parent;

  public TranslateVassalAction(Frame parent) {
    super(Resources.getString(
    "Editor.ModuleEditor.translate_vassal"));
    this.parent = parent;
  }

  public void actionPerformed(ActionEvent e) {
    new TranslateVassalWindow(parent).setVisible(true);
  }
}
