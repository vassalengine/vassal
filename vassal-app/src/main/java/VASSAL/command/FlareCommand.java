
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

public class FlareCommand extends Command {
  private final Flare flare;
  private final Point clickPoint;
  private final String id;
    
  public FlareCommand(final Flare flare) {
    clickPoint = new Point(flare.getClickPoint());
    this.flare = flare;
    id = flare.getMap().getId();
  }

  public FlareCommand(final Flare flare, final String id) {
    clickPoint = new Point(flare.getClickPoint());
    this.flare = flare;
    this.id    = id;
  }


  protected void executeCommand() {
    if (id.equals(flare.getMap().getId())) {
      flare.setClickPoint(clickPoint);
      flare.startAnimation(false);
    }
  }
    
  protected Command myUndoCommand() {
    return null;
  }
    
  public int getValue() {
    return 0;
  }
    
  public Point getClickPoint() {
    return clickPoint;
  }

  public String getMapId() {
    return id;
  }
}
