/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Brent Easton
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

import javax.swing.JDialog;

import VASSAL.configure.Configurer;
import VASSAL.counters.EditablePiece;
import VASSAL.counters.PropertiesPieceFilter;

public class PropertyExpressionBuilder extends ExpressionBuilder {
  private static final long serialVersionUID = 1L;

  public PropertyExpressionBuilder(Configurer c, JDialog parent) {
    super(c, parent);
  }

  public PropertyExpressionBuilder(Configurer c, JDialog parent, EditablePiece piece) {
    super(c, parent, piece);
  }

  /**
   * Convert an old-style Property Match Expression to a BeanShell Expression
   * @param s Old-style string
   * @return expression
   */
  public String convert(String s) {
    return PropertiesPieceFilter.toBeanShellString(s);
  }

}