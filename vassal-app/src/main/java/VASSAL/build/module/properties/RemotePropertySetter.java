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
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;
import VASSAL.script.expression.FormattedStringExpression;

/**
 * A PropertySetter that runs agains a remote DP, replaces $$ values from a local piece
 */
public class RemotePropertySetter extends PropertySetter implements RemotePropertyChanger {

  public RemotePropertySetter(PropertySetter setter) {
    super(setter.getRawValue(), setter.getPropSource());
  }

  /**
   * Evaluate any expression using the remote piece as the source of properties
   * Pre-evaluate any $$ variables in the local piece.
   *
   * @param remoteDP  Remote DP being set
   * @return          New value to set
   */

  @Override
  public String getNewValue(DynamicProperty remoteDP, Auditable owner, PropertySource ps) {

    String s = getRawValue();

    // Pre-Evaluate $$ variables against our source unit
    if (s.indexOf('$') >= 0) {
      final FormattedStringExpression fse = new FormattedStringExpression(s);
      s = fse.tryEvaluate(ps, owner, "Editor.DynamicProperty.key_commands");
    }

    // Then evaluate the result against the target piece
    if (getFormat() != null) {
      getFormat().setFormat(s);
      s = getFormat().getText(Decorator.getOutermost(remoteDP), owner, AuditTrail.create(owner, s));
    }

    return s;
  }
}
