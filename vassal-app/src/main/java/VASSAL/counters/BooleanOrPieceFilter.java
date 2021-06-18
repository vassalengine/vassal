/*
 *
 * Copyright (c) 2005 by Rodney Kinney
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

import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;

/**
 * Boolean Or of two PieceFilters
 */
public class BooleanOrPieceFilter implements PieceFilter {
  private final PieceFilter filter1;
  private final PieceFilter filter2;

  public BooleanOrPieceFilter(PieceFilter filter1, PieceFilter filter2) {
    this.filter1 = filter1;
    this.filter2 = filter2;
  }

  /** @deprecated */
  @Deprecated(since = "2021-06-11")
  @Override
  public boolean accept(GamePiece piece) {
    return filter1.accept(piece) || filter2.accept(piece);
  }

  @Override
  public boolean accept(GamePiece piece, Auditable owner, String fieldKey) {
    return PieceFilter.super.accept(piece, owner, fieldKey);
  }

  @Override
  public boolean accept(GamePiece piece, Auditable owner, AuditTrail audit) {
    final AuditTrail audit1 = audit == null ? null : new AuditTrail(audit);
    final AuditTrail audit2 = audit == null ? null : new AuditTrail(audit);
    return filter1.accept(piece, owner, audit1) || filter2.accept(piece, owner, audit2);
  }

  public PieceFilter getFilter1() {
    return filter1;
  }

  public PieceFilter getFilter2() {
    return filter2;
  }

}
