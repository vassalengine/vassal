/*
 *
 * Copyright (c) 2022 by The Vassal Development Team
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

import VASSAL.build.Configurable;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.widget.PieceSlot;
import VASSAL.i18n.Resources;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.util.Arrays;
import javax.swing.AbstractAction;
import org.apache.commons.lang3.ArrayUtils;


/**
 * Action to edit all {@link VASSAL.counters.GamePiece}'s within a given component
 * in separate Piece Definer windoes
 */
public class OpenContainedPiecesAction extends AbstractAction {
  private static final long serialVersionUID = 1L;

  private final Configurable target;
  protected HelpWindow helpWindow;
  protected Frame dialogOwner;
  protected ConfigureTree tree;

  public OpenContainedPiecesAction(Configurable target, HelpWindow helpWindow, Frame dialogOwner, ConfigureTree tree) {
    super(Resources.getString("Editor.OpenContainedPiecesAction.what_it_does"));
    this.target = target;
    this.helpWindow = helpWindow;
    this.dialogOwner = dialogOwner;
    this.tree = tree;
    setEnabled(Arrays.stream(target.getConfigureComponents()).anyMatch(c -> c instanceof PieceSlot));
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    final Configurable[] items = target.getConfigureComponents();
    ArrayUtils.reverse(items);
    for (final Configurable c : items) {
      if (c instanceof PieceSlot) {
        (new EditPropertiesAction(c, helpWindow, dialogOwner, tree)).actionPerformed(null);
      }
    }
  }
}