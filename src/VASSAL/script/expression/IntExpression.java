/*
 * $Id$
 *
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

import java.util.Map;

import VASSAL.build.module.properties.PropertySource;

/**
 * An expression consisting of an Integer only
 *
 */
public class IntExpression extends Expression {

  public IntExpression(int i) {
    try {
      setExpression(String.valueOf(i));
    }
    catch (NumberFormatException e) {
      setExpression("0");
    }
  }

  public String evaluate(PropertySource ps, Map<String, String> properties,
      boolean localized) {
    return getExpression();
  }

  public String toBeanShellString() {
    return getExpression();
  }

}