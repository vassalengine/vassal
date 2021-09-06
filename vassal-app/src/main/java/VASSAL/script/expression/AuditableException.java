/*
 *
 * Copyright (c) 2021 The VASSAL Development Team
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

public class AuditableException extends Exception {
  private static final long serialVersionUID = 1L;

  protected Auditable owner;
  protected AuditTrail auditTrail;

  public AuditableException(Auditable owner, AuditTrail auditTrail) {
    this.owner = owner;
    this.auditTrail = auditTrail;
  }

  public Auditable getOwner() {
    return owner;
  }

  public AuditTrail getAuditTrail() {
    return auditTrail;
  }

  public String getAuditReport() {
    return auditTrail == null ? "" : auditTrail.toString();
  }
}
