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
package VASSAL.build.widget;

import VASSAL.build.GameModule;
import VASSAL.configure.Configurer;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Obscurable;
import VASSAL.i18n.Resources;

/**
 * A CardSlot is identical to a PieceSlot except that it is initialized
 * with a card-like GamePiece.
 */
public class CardSlot extends PieceSlot {
  public CardSlot() {
  }

  public CardSlot(PieceSlot piece) {
    this();
    copyFrom(piece);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.CardSlot.component_type");
  }

  @Override
  public Configurer getConfigurer() {
    if (getPiece() == null) {
      GamePiece theCard = GameModule.getGameModule().createPiece(BasicPiece.ID + ";;;;");
      theCard = GameModule.getGameModule().createPiece(Obscurable.ID + "F;;" + Resources.getString("Editor.CardSlot.default_hide_command") + ";B", theCard); //NON-NLS
      setPiece(theCard);
    }
    return super.getConfigurer();
  }
}
