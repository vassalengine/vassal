/*
 * Copyright (c) 2009 Brent Easton
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
package VASSAL.script.expression;

/**
 * An expression consisting of an Integer only
 *
 */
public class IntExpression extends ImmutableExpression {
  private IntExpression(int i) {
    super(String.valueOf(i));
  }

  public static Expression instance(int i) {
    return CACHE.computeIfAbsent(i, k -> new IntExpression(i));
  }
}
