
/*
 * Copyright (c) 2020 Vassalengine.org
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
package VASSAL.command;

import java.awt.Point;

import VASSAL.build.module.DeckReposition;
import VASSAL.counters.Deck;

/**
 * A {@link Command} for sending {@link DeckReposition} actions to other clients
 */
public class DeckRepositionCommand extends Command {
  private Deck deck;
  private Point newPosition;

  /**
   */
  public DeckRepositionCommand(Deck deck, Point newPosition) {
    this.deck = deck;
    this.newPosition = newPosition;
  }

  /**
   * Executes the command (starts a Flare at the specified location)
   */
  @Override
  protected void executeCommand() {
    DeckReposition dr = new DeckReposition(this.deck, this.newPosition);
  }

  /**
    */
  @Override
  protected Command myUndoCommand() {
    return null;
  }
}


