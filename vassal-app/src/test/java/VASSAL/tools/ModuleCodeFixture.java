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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools;

/**
 * Stands in for a class a module ships inside its archive. {@link DataArchiveTest} copies this
 * class's bytes into a zip and has a {@link DataArchive} define them, so the resulting class is
 * loaded exactly as a real module's custom code is.
 */
public class ModuleCodeFixture {
  public static void boom() {
    throw new IllegalArgumentException("Invalid state list change");
  }
}
