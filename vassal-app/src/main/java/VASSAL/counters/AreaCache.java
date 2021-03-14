/*
 * Copyright (c) 2021 by Joel Uckelman
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

import java.awt.Rectangle;
import java.awt.geom.Area;
import java.util.Map;

import VASSAL.tools.concurrent.ConcurrentSoftHashMap;

public class AreaCache {
  private static final Map<Rectangle, Area> CACHE = new ConcurrentSoftHashMap<>();

  public static Area get(Rectangle r) {
    return CACHE.computeIfAbsent(r, k -> new Area(k));
  }
}
