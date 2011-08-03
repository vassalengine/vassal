/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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
package VASSAL.command;

import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;

import VASSAL.Info;
import VASSAL.build.GameModule;

/**
 * Evaluates properties of the GameModule and conditionally executes
 * another Command if all values are satisfied.
 */
public class ConditionalCommand extends Command {
  private Condition[] conditions;
  /** Command to execute if the condition is accepted */
  private Command delegate;

  public ConditionalCommand(Condition[] conditions, Command delegate) {
    this.conditions = conditions;
    this.delegate = delegate;
  }

  protected void executeCommand() {
    for (int i = 0; i < conditions.length; ++i) {
      if (!conditions[i].isSatisfied()) {
        return;
      }
    }
    delegate.execute();
  }

  protected Command myUndoCommand() {
    return null;
  }

  public Command getDelegate() {
    return delegate;
  }

  public Condition[] getConditions() {
    return conditions;
  }

  /**
   * The class representing a condition that must be satisfied if the
   * Command is to be executed
   */
  public static abstract class Condition {
    public abstract boolean isSatisfied();
  }

  public static class Eq extends Condition {
    /** The property to be checked */
    private String property;
    /** To pass the check the value of the property
     * must match one of these values. */
    private List<String> allowed;

    public Eq(String property, List<String> allowed) {
      this.property = property;
      this.allowed = allowed;
    }

    @Deprecated
    public Eq(String property, Vector<String> allowed) {
      this.property = property;
      this.allowed = allowed;
    }

    public String getProperty() {
      return property;
    }

    public List<String> getValueList() {
      return Collections.unmodifiableList(allowed);
    }

    /** @deprecated Use {@link #getValueList()} instead. */
    @Deprecated
    public Enumeration<String> getValues() {
      return Collections.enumeration(allowed);
    }

    public boolean isSatisfied() {
      String propertyValue =
        GameModule.getGameModule().getAttributeValueString(property);
      return allowed.contains(propertyValue);
    }
  }

  public static class Not extends Condition {
    private Condition sub;

    public Not(Condition sub) {
      this.sub = sub;
    }

    public boolean isSatisfied() {
      return !sub.isSatisfied();
    }

    public Condition getSubCondition() {
      return sub;
    }
  }

  public static class Lt extends Condition {
    private String property;
    private String value;

    public Lt(String property, String value) {
      this.property = property;
      this.value = value;
    }

    public String getProperty() {
      return property;
    }

    public String getValue() {
      return value;
    }

// FIXME: what versions are being compared here?
    public boolean isSatisfied() {
      String propertyValue =
        GameModule.getGameModule().getAttributeValueString(property);
      return Info.compareVersions(propertyValue, value) < 0;
    }
  }

  public static class Gt extends Condition {
    private String property;
    private String value;

    public Gt(String property, String value) {
      this.property = property;
      this.value = value;
    }

    public String getProperty() {
      return property;
    }

    public String getValue() {
      return value;
    }

// FIXME: what versions are being compared here?
    public boolean isSatisfied() {
      String propertyValue =
        GameModule.getGameModule().getAttributeValueString(property);
      return Info.compareVersions(propertyValue, value) > 0;
    }
  }
}
