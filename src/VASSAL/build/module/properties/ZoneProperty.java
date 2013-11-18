/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Brent Easton, Rodney Kinney
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
package VASSAL.build.module.properties;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;

/**
 *
 * @author Brent Easton
 *
 * A class implementing a cut-down Global property at the Zone level. Zone Properties
 * do not have Toolbar butttons, but are controlled by SetGlobalProperty traits in
 * counters.
 */
public class ZoneProperty extends GlobalProperty {

  protected Zone parentZone;

  public ZoneProperty() {
    super();
  }

  public ZoneProperty(GlobalProperty p) {
    super(p);
  }

  public static String getConfigureTypeName() {
    return "Global Property";
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  protected String getPropertyId() {
    String zoneName = parentZone == null ? null : parentZone.getName();
    Board board = parentZone == null ? null : parentZone.getBoard();
    String boardName = board == null ? null : board.getName();
    Map map = board == null ? null : board.getMap();
    String mapName = map == null ? null : map.getConfigureName();
    return getConfigureName()+"\t"+zoneName+"\t"+boardName+"\t"+mapName;
  }

  public void addTo(Buildable parent) {
    property.addTo((MutablePropertiesContainer) parent);
    GameModule.getGameModule().addCommandEncoder(this);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    propertySource = (PropertySource) parent;
    parentZone = (Zone) parent;
  }

}
