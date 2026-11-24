/*
 *
 * Copyright (c) 2026 by the VASSAL Development Team
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

/**
 * Thrown when the class named by a buildFile tag is in neither the module's
 * {@link VASSAL.tools.DataArchive} nor the classpath.
 *
 * <p>This is the one way of failing to create a component which can only ever
 * mean bad data in the module: VASSAL has no way to build a component whose
 * class does not exist, and no version of VASSAL ever could. It is separated
 * from the rest of {@link IllegalBuildException} so that {@link Builder#build}
 * can report it as bad module data rather than as a bug in VASSAL.</p>
 */
public class ComponentClassNotFoundException extends IllegalBuildException {
  private static final long serialVersionUID = 1L;

  private final String className;

  public ComponentClassNotFoundException(String message, String className, Throwable cause) {
    super(message, cause);
    this.className = className;
  }

  /**
   * @return the class named by the buildFile tag, which was not found
   */
  public String getClassName() {
    return className;
  }
}
