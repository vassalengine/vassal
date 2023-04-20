/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.configure;

@FunctionalInterface
public interface VisibilityCondition {
  boolean shouldBeVisible();

  class VisibilityAND implements VisibilityCondition {
    VisibilityCondition and1;
    VisibilityCondition and2;

    public VisibilityAND(VisibilityCondition a1, VisibilityCondition a2) {
      and1 = a1;
      and2 = a2;
    }

    public boolean shouldBeVisible() {
      return and1.shouldBeVisible() && and2.shouldBeVisible();
    }
  }
}
