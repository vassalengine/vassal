/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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
package VASSAL.build.module.properties;

import VASSAL.counters.Decorator;
import VASSAL.counters.DynamicProperty;
import VASSAL.script.expression.Auditable;
import VASSAL.script.expression.FormattedStringExpression;

/**
 * Remote version of IncrementProperty, sets the value into a DP in a different piece,
 * replacing any $$ variables in the expression with values from the source piece.
 *
 */
public class RemoteIncrementProperty extends IncrementProperty implements RemotePropertyChanger {

  protected PropertySource targetPropertySource;

  public RemoteIncrementProperty(IncrementProperty ip) {
    super(ip.getProp(), ip.getRawValue(), ip.constraints);
  }

  @Override
  public String getNewValue(DynamicProperty target, Auditable owner, PropertySource ps) {

    String s = getRawValue();

    // Pre-Evaluate $$ variables against our source unit
    if (s.indexOf('$') >= 0) {
      final FormattedStringExpression fse = new FormattedStringExpression(s);
      s = fse.tryEvaluate(ps, owner, "Editor.DynamicProperty.key_commands");
    }

    // Set the updated format
    format.setFormat(s);

    // Set the target source for the super class to use
    targetPropertySource = Decorator.getOutermost(target);

    // Get the standard IncrementProperty to determine the new value now that
    return super.getNewValue(target.getValue());
  }

  // Over-ride the Target property source
  @Override
  public PropertySource getTargetPropertySource() {
    return targetPropertySource;
  }

}
