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

/**
 * An Auditable class can have the execution of any {@link Expression} it contains tracked
 * and errors reported with additional detailusing the {@link AuditTrail} class
 */
public interface Auditable {
  /**
   * Return a description of the Type of trait or Component an Auditable is
   * @return Component Type
   */
  default String getComponentTypeName() {
    return "";
  };

  /**
   * Return the name of the trait or Component an Auditable is
   * @return Component name
   */
  default String getComponentName() {
    return "";
  };
}
