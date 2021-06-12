/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.counters;

import VASSAL.configure.PropertyExpression;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;

/**
 * A filter for GamePieces
 *
 * Sample implementations include:
 * {@link PropertyExpression} - used for "matching expressions" in Global Key Commands and the like
 * {@link RangeFilter} - filters for pieces within range of a point
 * {@link BooleanOrPieceFilter} - "OR"s two Filters together to make one Amazing Disjunctive Filter
 * {@link BooleanAndPieceFilter} - "AND"s two Filters together and you'd better satisfy both
 * {@link CounterDetailViewer.Filter} - Filter created from Mouseover Stack Viewer settings
 * {@link Inventory.Selector} - Layered filter for Piece Inventory window
 */
@SuppressWarnings("JavadocReference")
@FunctionalInterface
public interface PieceFilter {
  /**
   * Test if a piece matches the filter.
   * This sig should only be used for filters that can not have Expressions as a component of the filter
   * and thus have no Expression Auditing requirementts
   *
   * @param piece piece
   * @return true if piece match filter
   */
  boolean accept(GamePiece piece);

  /**
   * Test if a piece matches the filter and provide Expression auditing facilities
   * @param piece piece to test
   * @param owner owner of the filter
   * @param fieldKey EMesage key of the field holding the filter expression
   * @return
   */
  default boolean accept(GamePiece piece, Auditable owner, String fieldKey) {
    return accept(piece);
  }

  /**
   * Test if a piece matches the filter and provide Expression auditing facilities
   * @param piece piece to test
   * @param owner owner of the filter
   * @param fieldKey Audit Trail to record evaluation of the filter
   * @return
   */
  default boolean accept(GamePiece piece, Auditable owner, AuditTrail audit) {
    return accept(piece);
  }


}
