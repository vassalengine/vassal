/*
 * Copyright (c) 2023 by Brian Reynolds, Joel Uckelman 
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

import VASSAL.build.GameModule;
import VASSAL.command.Command;

import java.util.ArrayList;
import java.util.List;

public class DeckManager {
  
  protected List<Deck> deckEmptiedKeyQueue = new ArrayList<>();

  /**
   * Add an emptied deck that wants to sent an I-am-empty key
   */
  public void addEmptyDeck(Deck d) {
    deckEmptiedKeyQueue.add(d);
  }

  /**
   * Clears the list of I-am-empty decks
   */
  public void clearEmptyDecksList() {
    deckEmptiedKeyQueue.clear();
  }

  /**
   * If any decks want to send their I-am-empty key, send it now.
   */
  public Command checkEmptyDecks(Command c) {
    final GameModule gm = GameModule.getGameModule();
    gm.pauseLogging();

    final List<Deck> decksEmpty = new ArrayList<>(deckEmptiedKeyQueue); // Defensively defensify

    for (final Deck deck : decksEmpty) {
      deck.sendEmptyKey();
    }

    deckEmptiedKeyQueue.clear();

    c = c.append(gm.resumeLogging());
    return c;
  }
}
