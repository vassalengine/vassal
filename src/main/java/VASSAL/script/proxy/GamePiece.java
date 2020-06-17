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

import java.awt.Point;

import VASSAL.script.BeanShell;

/**
 * BeanShell proxy for the Vassal GamePiece object
 *
 * BeanShell proxies control access to Vassal components and expose
 * a consistent API to BeanShell Scripts.
 *
 * PLEASE NOTE
 * New methods may be added, but DO NOT remove or change the
 * call signature of any existing methods
 */
public class GamePiece {

  private VASSAL.counters.GamePiece vassalGamePiece;

  public GamePiece(VASSAL.counters.GamePiece g) {
    setVassalGamePiece(g);
  }

  protected void setVassalGamePiece(VASSAL.counters.GamePiece g) {
    vassalGamePiece = g;
  }

  VASSAL.counters.GamePiece getVassalGamePiece() {
    return vassalGamePiece;
  }

  public String getName() {
    return vassalGamePiece.getName();
  }

  public Object getProperty(String key) {
    return BeanShell.wrap(vassalGamePiece.getProperty(key).toString());
  }

  public Map getMap() {
    return new Map(vassalGamePiece.getMap());
  }

  public Point getPosition() {
    return vassalGamePiece.getPosition();
  }

  public Stack getParent() {
    return new Stack(vassalGamePiece.getParent());
  }
}