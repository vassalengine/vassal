/*
 *
 * Copyright (c) 2006 by Rodney Kinney
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

import VASSAL.script.expression.Auditable;

/**
 * A PropertySource provides an interface to read properties (values), usually game- or UI-related, from various
 * game objects. {@link VASSAL.counters.GamePiece} is the "OG" of PropertySources, but all of the objects which provide
 * containers for Global Properties (e.g. Zones, Maps, and GameModule) implement it as well.
 */
public interface PropertySource extends Auditable {

  /**
   * When using this interface a piece's own properties are preferred to those of
   * "Global Properties", and those in turn are searched Zone-first then Map, then Module.
   *
   * @param key String key of property to be returned
   * @return Object containing value of the specified property
   */
  Object getProperty(Object key);

  /**
   * Returns a localized translation of the specified property value, if available. Otherwise
   * returns the non-localized version.
   * @param key String key of property to be returned
   * @return Object containing localized text of the specified property, if available, otherwise non-localized value
   */
  Object getLocalizedProperty(Object key);
}
