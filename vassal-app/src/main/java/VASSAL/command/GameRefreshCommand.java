
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
 * A {@link Command} for sending {@link GameRefresh} actions to other clients
 */
public class GameRefreshCommand extends Command {

  /**
   */
  public GameRefreshCommand() {
  }

  /**
   * Executes the command (starts a Flare at the specified location)
   */
  @Override
  protected void executeCommand() {
  }

  /**
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
   * @return unique ID of the Flare object that this flare command is intended to activate
   */
  public String getId() {
    return "";
  }
}
