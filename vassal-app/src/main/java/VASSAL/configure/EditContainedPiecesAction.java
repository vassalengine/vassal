/*
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
import VASSAL.i18n.Resources;

/**
 * Action to edit all {@link VASSAL.counters.GamePiece}'s within a given component
 */
public class EditContainedPiecesAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private ConfigureTree tree;
  private Configurable target;

  public EditContainedPiecesAction(Configurable target) {
    super(Resources.getString("Editor.EditContainedPiecesAction.what_it_does"));
    this.target = target;
  }

  public EditContainedPiecesAction(Configurable target, ConfigureTree tree) {
    this(target);
    this.tree = tree;
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    final MassPieceDefiner mass = new MassPieceDefiner(target, tree);
    final Configurer c = new Configurer("", "") {
      @Override
      public void setValue(String s) {
      }

      @Override
      public java.awt.Component getControls() {
        return mass;
      }

      @Override
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
