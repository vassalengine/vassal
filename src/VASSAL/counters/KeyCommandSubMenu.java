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
package VASSAL.counters;

import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;

/** Represents a sub-menu in a GamePiece's right-click drop-down menu */
public class KeyCommandSubMenu extends KeyCommand {
  private static final long serialVersionUID = 1L;

  private List<String> commands = new ArrayList<String>();

  public KeyCommandSubMenu(String name, GamePiece target,
                           TranslatablePiece i18nPiece) {
    super(name, NamedKeyStroke.NULL_KEYSTROKE, target, i18nPiece);
  }

  public void actionPerformed(ActionEvent evt) {
  }

  public void addCommand(String s) {
    commands.add(s);
  }

  public void setCommands(String[] s) {
    commands.clear();
    commands.addAll(Arrays.asList(s));
  }

  public Iterator<String> getCommands() {
    return commands.iterator();
  }
}
