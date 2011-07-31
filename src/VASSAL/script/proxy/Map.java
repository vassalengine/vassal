/*
 * $Id$
 *
 * Copyright (c) 2008 by Brent Easton
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
package VASSAL.script.proxy;

import java.util.ArrayList;
import java.util.List;

import VASSAL.script.BeanShell;

/**
 * BeanShell proxy for the Vassal Map object
 *
 * BeanShell proxies control access to Vassal components and expose
 * a consistent API to BeanShell Scripts.
 *
 * PLEASE NOTE
 * New methods may be added, but DO NOT remove or change the
 * call signature of any existing methods
 *
 */
public class Map {

  private VASSAL.build.module.Map vassalMap;

  public Map(VASSAL.build.module.Map m) {
    vassalMap = m;
  }

  public String getName() {
    return vassalMap.getMapName();
  }

  public Object getProperty(String name) {
    return BeanShell.wrap(vassalMap.getProperty(name).toString());
  }

  public List<GamePiece> getPieces() {
    VASSAL.counters.GamePiece[] vPieces = vassalMap.getPieces();
    ArrayList<GamePiece> pieces = new ArrayList<GamePiece>(vPieces.length);
    for (int i = 0; i < vPieces.length; i++) {
      pieces.add(new GamePiece(vPieces[i]));
    }
    return pieces;
  }

}