/*
 *
 * Copyright (c) 2021 by The VASSAL Development Team
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
package VASSAL.build.module.map.deck;

import VASSAL.counters.Deck;
import VASSAL.counters.KeyCommand;

import java.util.List;

public interface DeckKeyCommand {

  /**
   * Return a list of KeyCommands provided by this DeckKeyCommand
   * @param deck The Deck to perform the Commands on
   * @return List of KeyCommands
   */
  List<KeyCommand> getKeyCommands(Deck deck);

  /**
   * Register any Listeners for Keystroke provided by this DeckKeyCommand
   * @param deck The Deck to perform the actions on if KeyStroke seem
   */
  void registerListeners(Deck deck);

  /**
   * Remove any Listener previously registered for this DeckKeyCommand
   */
  void deregisterListeners();

  /** Return an array of additional report parameters that can be used in the
   * Report format message of a Deck Key Command
   * @return Array of property names
   */
  String[] getAdditionalReportProperties();
}
