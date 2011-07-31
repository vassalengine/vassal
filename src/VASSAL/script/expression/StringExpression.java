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
 * An expression consisting of a String only
 *
 */
public class StringExpression extends Expression {

  public StringExpression (String s) {
    setExpression(s);
  }

  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized) {
    return getExpression();
  }

  public String toBeanShellString() {
    return "\"" + getExpression() + "\"";
  }

}