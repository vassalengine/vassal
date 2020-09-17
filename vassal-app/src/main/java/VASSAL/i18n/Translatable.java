/*
 *
 * Copyright (c) 2007 by Rodney Kinney, Brent Easton
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
package VASSAL.i18n;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;

/**
 * Defines VASSAL's standard interface for getting and setting XML (buildFile) attributes,
 * as well as attendant i18n translatable field infrastructure.
 * Components which have any translatable fields, or have any Translatable
 * children, must implement Translatable.
 */
public interface Translatable extends Buildable {
  ComponentI18nData getI18nData();

  /**
   * @return a String representation of the attribute with the given name. When initializing a module, this String value will be passed to
   * {@link #setAttribute}.
   *
   * @param key the name of the attribute. If the implementing class extends {@link AbstractConfigurable}, will be one of those listed in {@link AbstractConfigurable#getAttributeNames}.
   */
  String getAttributeValueString(String key);

  /**
   * Sets a buildFile (XML) attribute value for this component.
   *
   * @param key the name of the attribute. If the implementing class extends {@link AbstractConfigurable}, will be one of those listed in {@link AbstractConfigurable#getAttributeNames}.
   * @param value If the <code>value</code> parameter is a String, it will be the value returned by {@link #getAttributeValueString} for the same
   *              <code>key</code>. If the implementing class extends {@link AbstractConfigurable}, then <code>value</code> can also be an instance of
   *              the corresponding Class listed in {@link AbstractConfigurable#getAttributeTypes}
   */
  void setAttribute(String key, Object value);
}
