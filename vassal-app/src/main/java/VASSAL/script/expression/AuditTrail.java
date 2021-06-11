/*
 * Copyright (c) 2021 The Vassal Development Team
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

import VASSAL.build.AbstractConfigurable;
import VASSAL.configure.Configurer;
import VASSAL.counters.EditablePiece;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;

import VASSAL.tools.FormattedString;
import java.util.ArrayList;
import java.util.List;

/**
 * Class to track the evaluation history of an expression.
 * AuditTrails will create an overhead on every BeanShell expression evaluated, so only
 * generate them if a preference has been set by the user.
 */
public class AuditTrail {

  private Auditable source;
  private List<String> messages = new ArrayList<>();
  private static Boolean enabled;

  public static boolean isEnabled() {
    // Since this will be called for every single Beanshell evaluation, make it as low overhead as possible.
    // Keep a local copy of the pref value, updated via a property change listener.
    if (enabled == null) {
      final Configurer option = Prefs.getGlobalPrefs().getOption(Prefs.BAD_DATA_AUDIT_TRAILS);
      if (option == null) {
        // Running Tests
        return false;
      }
      enabled = (Boolean) Prefs.getGlobalPrefs().getValue(Prefs.BAD_DATA_AUDIT_TRAILS);
      option.addPropertyChangeListener(e -> enabled = (Boolean) e.getNewValue());
    }
    return enabled;
  }

  /**
   * Factory method to create a new AuditTrail only if auditing is enabled
   *
   * @param source             Audit source component
   * @param originalExpression Expression being audited
   * @param comment            Additional comment about the audit source
   * @return An AuditTrail object if auditing is enabled, otherwise null
   */
  public static AuditTrail create(Auditable source, String originalExpression, String comment) {
    if (isEnabled()) {
      return new AuditTrail(source, originalExpression, comment);
    }
    else {
      return null;
    }
  }

  /**
   * Factory method to create a new AuditTrail only if auditing is enabled
   *
   * @param source             Audit source component
   * @param originalExpression Expression being audited
   * @param comment            Additional comment about the audit source
   * @return An AuditTrail object if auditing is enabled, otherwise null
   */
  public static AuditTrail create(Auditable source, Expression originalExpression, String comment) {
    return create(source, originalExpression.getExpression(), comment);
  }

  public static AuditTrail create(Auditable source, FormattedString originalExpression, String comment) {
    return create(source, originalExpression.getFormat(), comment);
  }

  /**
   * Factory method to create a new AuditTrail only if auditing is enabled
   *
   * @param source             Audit source component
   * @param originalExpression Expression being audited
   * @return An AuditTrail object if auditing is enabled, otherwise null
   */
  public static AuditTrail create(Auditable source, String originalExpression) {
    return create(source, originalExpression, "");
  }

  /**
   * Factory method to create a new AuditTrail only if auditing is enabled
   *
   * @param source             Audit source component
   * @param originalExpression Expression being audited
   * @return An AuditTrail object if auditing is enabled, otherwise null
   */
  public static AuditTrail create(Auditable source, Expression originalExpression) {
    return create(source, originalExpression.getExpression(), "");
  }

  public static AuditTrail create(Auditable source, FormattedString originalExpression) {
    return create(source, originalExpression.getFormat(), "");
  }
  /**
   * Factory method to create a new AuditTrail only if auditing is enabled
   *
   * @param source Audit source component
   * @return An AuditTrail object if auditing is enabled, otherwise null
   */
  public static AuditTrail create(Auditable source) {
    return create(source, "", "");
  }

  /**
   * Copy constructor for AuditTrail
   *
   * @param audit AuditTrail to copy
   */
  public AuditTrail(AuditTrail audit) {
    this.source = audit.source;
    this.messages = new ArrayList<>(audit.messages);
  }

  /**
   * Create a new AuditTrail
   *
   * @param source             Audit source component
   * @param originalExpression Expression being audited
   * @param sourceField        Comment describing the source field of the expression being audited
   * @return An AuditTrail object if auditing is enabled, otherwise null
   */
  public AuditTrail(Auditable source, String originalExpression, String sourceField) {
    setSource((Auditable) source);
    if (sourceField != null && ! sourceField.isEmpty()) {
      addMessage(Resources.getString("Audit.source_field", sourceField));
    }
    setExpression(originalExpression);
  }

  public void setExpression(String expression) {
    if (expression != null && ! expression.isEmpty()) {
      addMessage(Resources.getString("Audit.expression", expression));
    }
  }

  public AuditTrail(Auditable source, String sourceField) {
    this(source, "", sourceField);
  }

  /**
   * Add a message to the Audit Trail
   *
   * @param message message to record in Audit Trail
   */
  public void addMessage(String message) {
    if (message != null && !message.isEmpty()) {
      messages.add(message);
    }
  }


  public void setSource(Auditable source) {
    this.source = source;

    // Add some source identification to the audit trail
    if (source != null) {
      if (source instanceof EditablePiece) {
        addMessage(Resources.getString("Audit.source_type", "Piece Trait"));
        addMessage(Resources.getString("Audit.source_name", source.getComponentName()));
        addMessage(Resources.getString("Audit.source_description", source.getComponentTypeName()));
      }
      if (source instanceof AbstractConfigurable) {
        addMessage(Resources.getString("Audit.source_type", source.getComponentTypeName()));
        addMessage(Resources.getString("Audit.source_name", source.getComponentName()));
      }
    }
  }

  /**
   * Override toString to return a formated Audit Report
   *
   * @return Audit Report
   */
  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("Expression Audit:");

    for (final String message : messages) {
      sb.append("\n   ").append(message);
    }
    return sb.toString();
  }
}

