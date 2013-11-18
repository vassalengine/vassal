/*
 * $Id: IntBuilder.java 7725 2011-07-31 18:51:43Z uckelman $
 *
 * Copyright (c) 2008-2012 Brent Easton
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

/*
 * StrBuilder.
 * Build an integer constant. Allow user to enter anything and clean it up by removing
 * non number characters
 */

package VASSAL.script.expression;

import javax.swing.JDialog;

import VASSAL.configure.StringConfigurer;

public class IntBuilder extends StrBuilder {
  private static final long serialVersionUID = 1L;

  public IntBuilder(StringConfigurer c, JDialog parent) {
    super(parent, "Number Builder", true);
    target = c;
    build("Number");
  }


  protected void save() {
    boolean negative = false;
    final StringBuilder result = new StringBuilder();
    String value = entry.getValueString();

    if (value.startsWith("-")) {
      value = value.substring(1);
      negative = true;
    }

    for (int i = 0; i < value.length(); i++) {
      final char c = value.charAt(i);
      if (c >= '0' && c <= '9') {
        result.append(c);
      }
    }

    value = (negative ? "-" : "") + result.toString();
    if (value.length() == 0 || value.equals("-")) {
      value = "0";
    }

    target.setValue(value);
    dispose();
  }

}