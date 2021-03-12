/*
 *
 * Copyright (c) 2006-2021 by Rodney Kinney, Brent Easton, Brian Reynolds
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
 * A component that can contain mutable (updatable) properties
 *
 * @author rkinney
 *
 */
public interface TranslatableStringContainer {
  /**
   * Add a property under the given key
   * @param key key
   * @param p property
   */
  void addTranslatableString(String key, TranslatableString p);

  /**
   * Remove the property with the given key
   * @param key key
   */
  TranslatableString removeTranslatableString(String key);

  /** Find an object with the given name */
  TranslatableString getTranslatableString(String propertyName);

  /** Return a unique Id for the container */

  String getTranslatableStringContainerId();

  /**
   * Simple implementation of {@link TranslatableStringContainer}
   */
  class Impl implements TranslatableStringContainer {
    private final Map<String, TranslatableString> props = new HashMap<>();
    private final String id;

    public Impl() {
      this("");
    }

    public Impl(String id) {
      this.id = id;
    }

    @Override
    public String getTranslatableStringContainerId() {
      return id;
    }

    @Override
    public void addTranslatableString(String key, TranslatableString p) {
      props.put(key, p);
    }

    @Override
    public TranslatableString getTranslatableString(String propertyName) {
      return props.get(propertyName);
    }

    @Override
    public TranslatableString removeTranslatableString(String key) {
      return props.remove(key);
    }
  }
}
