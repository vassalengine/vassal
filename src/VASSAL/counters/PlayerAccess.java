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
package VASSAL.counters;

import VASSAL.build.GameModule;

/**
 * Access is granted if {@link GameModule#getUserId()} matches
 *
 * @author rkinney
 *
 */
public class PlayerAccess implements PieceAccess {
  private static PlayerAccess instance;

  public static PlayerAccess getInstance() {
    if (instance == null) {
      instance = new PlayerAccess();
    }
    return instance;
  }

  public String getCurrentPlayerId() {
    return GameModule.getUserId();
  }

  public boolean currentPlayerHasAccess(String id) {
    return id == null || (!GlobalAccess.isHideAll() && id.equals(getCurrentPlayerId()));
  }

  public boolean currentPlayerCanModify(String ownerId) {
    return ownerId == null || ownerId.equals(getCurrentPlayerId());
  }
}
