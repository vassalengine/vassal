
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

import VASSAL.build.module.map.Flare;

/**
 * A {@link Command} for sending {@link Flare} actions to other clients
 */
public class FlareCommand extends Command {
  private final Flare flare;
  private final Point clickPoint;

  /**
   * @param flare Flare object to define our new action from
   */
  public FlareCommand(final Flare flare) {
    clickPoint = new Point(flare.getClickPoint());
    this.flare = flare;
  }

  /**
   * Executes the command (starts a Flare at the specified location)
   */
  @Override
  protected void executeCommand() {
    flare.setClickPoint(clickPoint);
    flare.startAnimation(false);
  }

  /**
   * @return null - no undo needed for Flare commands
   */
  @Override
  protected Command myUndoCommand() {
    return null;
  }

  /**
   * @return 0
   */
  public int getValue() {
    return 0;
  }

  /**
   * @return specified flare location on map
   */
  public Point getClickPoint() {
    return clickPoint;
  }

  /**
   * @return unique ID of the Flare object that this flare command is intended to activate
   */
  public String getId() {
    return flare.getId();
  }
}
