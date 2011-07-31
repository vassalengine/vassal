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

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import VASSAL.build.Configurable;
import VASSAL.counters.MassPieceDefiner;

/**
 * Action to edit all {@link VASSAL.counters.GamePiece}'s within a given component
 */
public class EditContainedPiecesAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private Configurable target;

  public EditContainedPiecesAction(Configurable target) {
    super("Edit All Contained Pieces");
    this.target = target;
  }

  public void actionPerformed(ActionEvent evt) {
    final MassPieceDefiner mass = new MassPieceDefiner(target);
    Configurer c = new Configurer("", "") {
      public void setValue(String s) {
      }

      public java.awt.Component getControls() {
        return mass;
      }

      public String getValueString() {
        return "";
      }
    };
    final ConfigurerWindow w =  new ConfigurerWindow(c);
    w.setVisible(true);
    if (! w.isCancelled() && mass.isChanged()) {
      mass.save();
    }
  }

}
