/*
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

package VASSAL.build;

import VASSAL.build.widget.PieceSlot;
import VASSAL.configure.ConfigureTree;
import VASSAL.counters.Decorator;
import VASSAL.counters.EditablePiece;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;
import VASSAL.script.expression.AuditableException;
import VASSAL.script.expression.ExpressionException;

/**
 * General-purpose condition indicating that VASSAL has encountered data that's inconsistent with the current module.
 * A typical example would be failing to find a map/board/image/prototype from the supplied name.  Covers a variety of
 * situations where the most likely cause is a module version compatibility issue.
 *
 * This is for recoverable errors that occur during game play, as opposed to {@link IllegalBuildException},
 * which covers errors when building a module
 * @see ErrorDialog.dataWarning()
 * @author rodneykinney
 *
 */
public class BadDataReport {
  private String message;
  private String data; /** Data about the cause of the error. Usually the expression that causes the problem */
  private Throwable cause;
  private boolean reportable = true;
  private AuditTrail auditTrail;
  private Auditable owner;  /** Owning component or trait that generated the error */

  public BadDataReport() {
  }

  /**
   * Basic Bad Data Report
   *
   * @param message Message to display
   * @param data Data causing error
   * @param cause Throwable that generated error
   */
  public BadDataReport(String message, String data, Throwable cause) {
    this.message = message;
    this.data = data;
    setCause(cause);
  }

  /**
   * BadDataReport for an Exception raised during Expression Execution
   *
   * @param message Message to display
   * @param data Data causing error
   * @param cause Throwable that generated error
   */
  public BadDataReport(String message, String data, ExpressionException cause) {
    this(null, null, message, data, cause);
  }

  /**
   * BadDataReport for a general exception raised during Expression Execution with supplied audit info
   *
   * @param message Message to display
   * @param data Data causing error
   * @param cause Throwable that generated error
   * @param owner Owning component
   * @param auditTrail Audit Trail of expression execution
   */
  public BadDataReport(String message, String data, Throwable cause, Auditable owner, AuditTrail auditTrail) {
    this(null, null, message, data, cause);
    this.owner = owner;
    this.auditTrail = auditTrail;
  }

  /**
   * Generic Bad Data Report, no exeption
   * @param message Message to display
   * @param data Data causing error
   */
  public BadDataReport(String message, String data) {
    this(message, data, null);
  }

  /**
   * Expanded Bad Data Report called by Traits.
   * Display additional information to aid debugging
   *
   * NB. Note use of piece.getLocalizedName() rather than
   * Decorator.getOuterMost().getLocalizedName() which can result in infinite
   * BadData reporting loops.

   * @param piece Trait that generated the error
   * @param message Resource message key to display
   * @param data Data causing error
   * @param cause Throwable that generated error
   */
  public BadDataReport(EditablePiece piece, String message, String data, Throwable cause) {
    this(getPieceName(piece), piece.getBaseDescription(), message, data, cause);
    setReportable(piece.getMap() != null);
  }

  public BadDataReport(EditablePiece piece, String message, String data) {
    this(piece, message, data, null);
  }

  public BadDataReport(EditablePiece piece, String message) {
    this(piece, message, "");
  }

  public BadDataReport(String pieceName, String traitDesc, String message, String data, Throwable cause) {
    String m = "";
    if (! (cause instanceof AuditableException)) {
      m = ((pieceName != null && pieceName.length() > 0) ? pieceName + " " : "");
      m += ((traitDesc != null && traitDesc.length() > 0) ? "[" + traitDesc + "] " : "");
      m += m.length() > 0 ? ". " : "";
    }
    m += message + ". " + getAuditMessage();

    this.message = m;
    this.data = data;
    setCause(cause);
  }

  /**
   * Return the name of the piece. For Decorators, return the name of the inner piece as
   * the Bad Data Report may have been generated by the call to get the name of the Decorator
   * in the first place.
   *
   * @param piece
   * @return
   */
  protected static String getPieceName(EditablePiece piece) {
    if (piece instanceof Decorator) {
      return ((Decorator) piece).getInner().getLocalizedName();
    }
    else {
      return piece.getLocalizedName();
    }
  }

  /**
   * Expanded Bad Data Report for AbstractConfigurables.
   * Display the name and type of the Configurable
   *
   * @param c AbstractConfigurable that generated the error
   * @param message Resource message key to display
   * @param data Data causing error
   * @param cause Throwable that generated error
   */
  public BadDataReport(AbstractConfigurable c, String message, String data, Throwable cause) {
    this.message = c.getConfigureName() + "[" + ConfigureTree.getConfigureName(c.getClass()) + "]: " + message + " " + getAuditMessage();
    this.data = data;
    setCause(cause);
  }

  public BadDataReport(AbstractConfigurable c, String messageKey, String data) {
    this(c, messageKey, data, null);
  }

  private String getAuditMessage() {
    return Resources.getString(AuditTrail.isEnabled() ? "BadDataReport.see_errorlog" : "BadDataReport.enable_pref");
  }
  /**
   * Expanded Bad Data Report for PieceSlot
   * Display the name of the slot
   *
   * @param c PieceSlot that generated the error
   * @param message Resource message key to display
   * @param data Data causing error
   */
  public BadDataReport(PieceSlot slot, String message, String data) {
    this.message = slot.getName() + ": " + message;
    this.data = data;
    setCause(null);
  }

  /**
   * Should this report be reported?
   *
   * @return false if this report should be ignored
   */
  public boolean isReportable() {
    return reportable;
  }

  public void setReportable(boolean reportable) {
    this.reportable = reportable;
  }

  /**
   * Build a single line printable message for this Bad Data Report
   * @return message
   */
  public String getMessage() {
    final StringBuilder sb = new StringBuilder();
    if (owner != null) {
      sb.append(getDescription(owner));
    }
    if (data != null) {
      sb.append(' ').append(Resources.getString("Audit.source", data));
    }
    sb.append(' ').append(Resources.getString("Audit.error", message));
    return sb.toString();
  }

  private String getDescription(Auditable owner) {
    return owner.getComponentName() + " " + owner.getComponentTypeName();
  }

  public Throwable getCause() {
    return cause;
  }

  private void setCause(Throwable cause) {
    this.cause = cause;
    if (cause instanceof AuditableException) {
      auditTrail = ((AuditableException) cause).getAuditTrail();
      owner =  ((AuditableException) cause).getOwner();
    }
  }

  public String getData() {
    return data;
  }

  public AuditTrail getAuditTrail() {
    return auditTrail;
  }

  public String getAuditReport() {
    return auditTrail == null ? "" : auditTrail.toString();
  }

  /**
   * Return the full audit trail if it exists
   * @return full data message
   */
  public String getFullData() {
    return auditTrail == null ? "" : ("\n" + auditTrail.toString());
  }
}
