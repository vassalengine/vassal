/*
 * Copyright (c) 2013 Brent Easton
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

import org.apache.commons.lang3.tuple.Pair;

import VASSAL.build.module.properties.PropertySource;

/**
 * An expression consisting of a single property name
 */
public class SinglePropertyExpression extends Expression {
  public SinglePropertyExpression(String ex) {
    super(ex.startsWith("$") && ex.endsWith("$") ? ex.substring(1, ex.length() - 1) : ex);
  }

  @Override
  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized) throws ExpressionException {
    String value = null;
    try {
      if (properties != null) {
        value = properties.get(getExpression());
      }
      if (value == null && ps != null) {
        if (localized) {
          value = (String) ps.getLocalizedProperty(getExpression());
        }
        else {
          value = (String) ps.getProperty(getExpression());
        }
      }
    }
    catch (Exception ex) {
      throw new ExpressionException(getExpression(), ex.getMessage());
    }
    return value == null ? "" : value;
  }

  @Override
  public String toBeanShellString() {
    return BeanShellExpression.convertProperty(getExpression());
  }

  public static Expression instance(String s) {
    return CACHE.computeIfAbsent(Pair.of(s, SinglePropertyExpression.class), k -> new SinglePropertyExpression(s));
  }
}
