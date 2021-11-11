/*
 *
 * Copyright (c) 2021 by vassalengine.org and the VASSAL Team. Added by Brian Reynolds.
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

package VASSAL.configure;

/**
 * Components that have descriptions which can be displayed next to their component types/names in the main Editor window
 */
public interface ComponentDescription {
  /**
   * @return Description to be displayed next to component type/name in the main Editor window. Or null or "" to not display anything.
   */
  default String getDescription() {
    return null;
  }
}
