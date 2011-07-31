/*
 * $Id$
 *
 * Copyright (c) 2008-2009 Brent Easton
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */
package VASSAL.script;

import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.counters.CalculatedProperty;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

/**
 * Used by the Bsh Plugin
 * @author Brent
 *
 */
public class BshCommandEncoder extends BasicCommandEncoder {

  public Decorator createDecorator(String type, GamePiece inner) {
    if (type.startsWith(CalculatedProperty.ID)) {
      return new CalculatedProperty(type, inner);
    }
    return super.createDecorator(type, inner);
  }
}
