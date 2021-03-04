/*
 * $Id: SinglePropertyExpression.java 7725 2011-07-31 18:51:43Z uckelman $
 *
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

import VASSAL.build.module.properties.PropertySource;

/**
 * An expression consisting of a single property name
 *
 */
public class SinglePropertyExpression extends BaseExpression {

  public SinglePropertyExpression(String ex) {
    if (ex.startsWith("$") && ex.endsWith("$")) {
      setExpression(ex.substring(1, ex.length() - 1));
    }
    else {
      setExpression(ex);
    }
  }

  @Override
  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized)
      throws ExpressionException {
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

}
