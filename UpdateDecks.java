import java.util.Enumeration;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.counters.Deck;
import VASSAL.counters.GamePiece;

/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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

public class UpdateDecks extends AbstractConfigurable implements GameComponent {

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public void setAttribute(String key, Object value) {
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setup(boolean gameStarting) {
    if (gameStarting) {
      for (Enumeration e = GameModule.getGameModule().getGameState().getPieces();e.hasMoreElements();) {
        GamePiece p = (GamePiece) e.nextElement();
        if (p instanceof Deck) {
          ((Deck)p).setPersistable(true);
        }
      }
    }
  }

  public Command getRestoreCommand() {
    return null;
  }

  public void removeFrom(Buildable parent) {
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {    
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

}
