/*
 * Copyright (c) 2021 Brent Easton, Joel Uckelman
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
 * An immutable Expression
 *
 */
public abstract class ImmutableExpression extends Expression {
  @Override
  public void setExpression(String s) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized) {
    return getExpression();
  }

  @Override
  public String tryEvaluate(PropertySource ps) {
    return getExpression();
  }

  @Override
  public String tryEvaluate() {
    return getExpression();
  }

  @Override
  public String tryEvaluate(PropertySource ps, boolean localized) {
    return getExpression();
  }

  @Override
  public String tryEvaluate(PropertySource ps, Map<String, String> properties, boolean localized) {
    return getExpression();
  }

  @Override
  public String toBeanShellString() {
    return getExpression();
  }
}
