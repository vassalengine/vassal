/*
 *
 * Copyright (c) 2008-2009 Brent Easton
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
package VASSAL.script;

import VASSAL.build.module.Plugin;
import VASSAL.counters.CalculatedProperty;

public class BshPlugin extends Plugin {

  public BshPlugin() {
    super();
  }

  @Override
  public void init() {
    registerGamePiece(new CalculatedProperty());
    registerCommandEncoder(new BshCommandEncoder());
  }
}