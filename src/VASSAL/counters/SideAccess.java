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

import VASSAL.build.module.PlayerRoster;

/**
 * Access is granted if {@link VASSAL.build.module.PlayerRoster#getMySide()}
 * matches
 *
 * @author rkinney
 *
 */
public class SideAccess implements PieceAccess {
  private static SideAccess instance;

  public static SideAccess getInstance() {
    if (instance == null) {
      instance = new SideAccess();
    }
    return instance;
  }

  public String getCurrentPlayerId() {
    return PlayerRoster.getMySide();
  }

  public boolean currentPlayerHasAccess(String id) {
    return id == null || (!GlobalAccess.isHideAll() && id.equals(getCurrentPlayerId()));
  }

  public boolean currentPlayerCanModify(String ownerId) {
    String currentPlayerId = getCurrentPlayerId();
    return currentPlayerId != null && (ownerId == null || ownerId.equals(currentPlayerId));
  }

}
