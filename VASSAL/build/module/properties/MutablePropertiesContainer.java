/*
 * $Id$
 *
 * Copyright (c) 2006-2007 by Rodney Kinney
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

import java.util.HashMap;
import java.util.Map;

/**
 * A component that can contain mutable (updateable) properties
 * 
 * @author rkinney
 * 
 */
public interface MutablePropertiesContainer {
  /**
   * Add a property under the given key
   * @param key
   * @param p
   */
  void addMutableProperty(String key, MutableProperty p);
  
  /**
   * Remove the property with the given key
   * @param key
   */
  MutableProperty removeMutableProperty(String key);

  /** Find a GlobalProperty object with the given name */
  MutableProperty getMutableProperty(String propertyName);
  
  /**
   * Simple implementation of {@link MutablePropertiesContainer}
   * @author rkinney
   *
   */
  public static class Impl implements MutablePropertiesContainer {
    private Map<String,MutableProperty> props =
      new HashMap<String,MutableProperty>();

    public void addMutableProperty(String key, MutableProperty p) {
      props.put(key,p);
    }

    public MutableProperty getMutableProperty(String propertyName) {
      return props.get(propertyName);
    }

    public MutableProperty removeMutableProperty(String key) {
      return props.remove(key);
    }
  }
}
