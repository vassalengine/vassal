/*
 *
 * Copyright (c) 2009-2012 Brent Easton
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
import VASSAL.tools.SequenceEncoder;

/**
 * Report Format or old-style Formatted String expression containing at
 * least one $variable$ name reference
 *
 */
public class FormattedStringExpression extends Expression {
  public FormattedStringExpression(String s) {
    super(s);
  }

  /**
   * Evaluate this expression.
   * NB. Code moved from FormattedString.java
   */
  @Override
  public String evaluate(PropertySource ps, Map<String, String> properties, boolean localized) {
    final StringBuilder buffer = new StringBuilder();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(getExpression(), '$');
    boolean isProperty = true;
    while (st.hasMoreTokens()) {
      final String token = st.nextToken();
      isProperty = !isProperty;
      if (token.length() > 0) {
        /*
         * Only even numbered tokens with at least one token after them are valid $propertyName$ strings.
         */
        if (!isProperty || ! st.hasMoreTokens()) {
          buffer.append(token);
        }
        else if (properties != null && properties.containsKey(token)) {
          final String value = properties.get(token);
          if (value != null) {
            buffer.append(value);
          }
        }
        else if (ps != null) {
          final Object value =
            localized ? ps.getLocalizedProperty(token) : ps.getProperty(token);
          if (value != null) {
            buffer.append(value);
          }
          else if (!localized) {
            buffer.append(token);
          }
        }
        else {
          buffer.append(token);
        }
      }
    }

    return buffer.toString();
  }

  /**
   * Convert to a BeanShell expression
   */
  @Override
  public String toBeanShellString() {
    final String s = getExpression();

    try {
      Integer.parseInt(s);
      return s;
    }
    catch (NumberFormatException e) {
      // Not an error
    }

    final StringBuilder buffer = new StringBuilder();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, '$');
    boolean isProperty = true;
    boolean first = true;
    while (st.hasMoreTokens()) {
      final String token = st.nextToken();
      isProperty = !isProperty;
      if (token.length() > 0) {
        /*
         * Only even numbered tokens with at least one token after them are valid $propertyName$ strings.
         */
        if (! first) {
          buffer.append('+');
        }
        if (isProperty && st.hasMoreTokens()) {
          buffer.append(BeanShellExpression.convertProperty(token));
        }
        else {
          buffer.append('\"');
          buffer.append(token);
          buffer.append('\"');
        }
        first = false;
      }
    }

    return buffer.toString();
  }

  public static Expression instance(String s) {
    return CACHE.computeIfAbsent(Pair.of(s, FormattedStringExpression.class), k -> new FormattedStringExpression(s));
  }
}
