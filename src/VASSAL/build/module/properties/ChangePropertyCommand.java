/*
 * $Id$
 *
 * Copyright (c) 2000-2008 by Rodney Kinney
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
package VASSAL.build.module.properties;

import VASSAL.command.Command;

/**
 * Command to change the value of a {@link MutableProperty}
 *
 * @author rodneykinney
 *
 */
public class ChangePropertyCommand extends Command {
  private MutableProperty property;
  private String propertyName;
  private String newValue;
  private String oldValue;

  public ChangePropertyCommand(MutableProperty property, String propertyName, String oldValue, String newValue) {
    super();
    this.property = property;
    this.propertyName = propertyName;
    this.newValue = newValue;
    this.oldValue = oldValue;
  }

  protected void executeCommand() {
    property.setPropertyValue(newValue);
  }

  protected Command myUndoCommand() {
    return new ChangePropertyCommand(property, propertyName, newValue, oldValue);
  }

  public MutableProperty getProperty() {
    return property;
  }

  public String getPropertyName() {
    return propertyName;
  }

  public String getNewValue() {
    return newValue;
  }

  public String getOldValue() {
    return oldValue;
  }
}
