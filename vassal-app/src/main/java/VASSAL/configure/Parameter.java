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

package VASSAL.configure;

import VASSAL.tools.SequenceEncoder;

/**
 * A Description of a Parameter to be set in a target gamepiece
 * - A Dynamic property name
 * - A value, which may be an expression and contain $$ variables.
 */
public class Parameter {
  public static final char DELIMITER = ':';
  private final String propertyName;
  private final String value;

  public static String encode(Parameter param) {
    final SequenceEncoder se = new SequenceEncoder(DELIMITER);
    se.append(param.getPropertyName()).append(param.getValue());
    return se.getValue();
  }

  public Parameter(String propertyName, String value) {
    this.propertyName = propertyName;
    this.value = value;
  }

  public Parameter(String encoded) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(encoded, DELIMITER);
    propertyName = sd.nextToken("");
    value = sd.nextToken("");
  }

  public String getPropertyName() {
    return propertyName;
  }

  public String getValue() {
    return value;
  }

  public String encode() {
    return encode(this);
  }


  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof Parameter)) return false;
    final Parameter p = (Parameter) obj;

    return getPropertyName().equals(p.getPropertyName()) && getValue().equals(p.getValue());
  }
}