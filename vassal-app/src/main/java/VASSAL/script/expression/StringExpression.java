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

import org.apache.commons.lang3.tuple.Pair;

/**
 * An expression consisting of a String only
 *
 */
public class StringExpression extends Expression {
  private StringExpression(String s) {
    super(s.intern());
  }

  @Override
  public String toBeanShellString() {
    return "\"" + getExpression() + "\"";
  }

  public static Expression instance(String s) {
    return CACHE.computeIfAbsent(Pair.of(s, StringExpression.class), k -> new StringExpression(s));
  }
}
