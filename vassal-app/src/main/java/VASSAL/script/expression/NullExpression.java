/*
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
 * An Empty Expression
 *
 */
public class NullExpression extends Expression {
  private NullExpression() {
  }

  private static final NullExpression THE_EXPRESSION = new NullExpression();

  public static NullExpression instance() {
    return THE_EXPRESSION;
  }

  @Override
  public void setExpression(String s) {
  }

  @Override
  public String getExpression() {
    return "";
  }

  @Override
  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized) {
    return "";
  }

  @Override
  public String tryEvaluate(PropertySource ps) {
    return "";
  }

  @Override
  public String tryEvaluate() {
    return "";
  }

  @Override
  public String tryEvaluate(PropertySource ps, boolean localized) {
    return "";
  }

  @Override
  public String tryEvaluate(PropertySource ps, Map<String, String> properties, boolean localized) {
    return "";
  }

  @Override
  public String toBeanShellString() {
    return "";
  }
}
