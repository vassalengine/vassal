/*
 *
 * Copyright (c) 2023 by VASSAL Development Team, Brian Reynolds
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

/**
 * Allow "Anding" two VisibilityCondition items together, dealing with the whole "null means visible" situation
 */
public class VisibilityAND implements VisibilityCondition {
  final VisibilityCondition and1;
  final VisibilityCondition and2;

  public VisibilityAND(VisibilityCondition a1, VisibilityCondition a2) {
    and1 = a1;
    and2 = a2;
  }

  @Override
  public boolean shouldBeVisible() {
    return ((and1 == null) || and1.shouldBeVisible()) && ((and2 == null) || and2.shouldBeVisible());
  }
}
