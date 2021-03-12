/*
 *
 * Copyright (c) 2000-2021 by Rodney Kinney, Brent Easton, Brian Reynolds
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.List;

/**
 * A container for a String property that can be translated
 */
public interface TranslatableString {
  String getPropertyValue();

  TranslatableStringContainer getParent();

  void setPropertyValue(String newValue);


  class Util {
    /**
     * Look for a {@link TranslatableString} in the list of {@link TranslatableStringContainer}. Return the first one
     * found, searching the lists in order. The list may contain null references, which are skipped
     *
     * @param propertyContainers list of containers
     * @return first property found
     */
    public static TranslatableString findTranslatableString(String propertyName, List<TranslatableStringContainer> propertyContainers) {
      TranslatableString p = null;
      for (final TranslatableStringContainer c : propertyContainers) {
        p = (c == null ? null : c.getTranslatableString(propertyName));
        if (p != null) {
          break;
        }
      }
      return p;
    }
  }

  /**
   * Simple implementation of {@link TranslatableString} Support dynamic changing of the property name, provided that
   * the {@link #addTo(TranslatableStringContainer)} method is used to register this property with a properties
   * container.
   */
  class Impl implements TranslatableString {
    private final PropertyChangeSupport propSupport;
    private String value = "";
    private String propertyName;
    private TranslatableStringContainer parent;

    // Maintain a static list of all Translatable Strings known to module
    private static final List<Impl> allProperties = new ArrayList<>();
    public static List<Impl> getAllProperties() {
      return allProperties;
    }

    /**
     * @param source will be the source of any {@link PropertyChangeEvent} fired by this object
     */
    public Impl(String propertyName, Object source) { //NOPMD
      this.propertyName = propertyName;
      propSupport = new PropertyChangeSupport(this);
    }

    public void addTo(TranslatableStringContainer c) {
      parent = c;
      parent.addTranslatableString(propertyName, this);
      allProperties.add(this);
    }

    public void setPropertyName(String name) {
      if (parent != null) {
        parent.removeTranslatableString(propertyName);
        parent.addTranslatableString(name, this);
      }
      propertyName = name;
    }

    public void removeFromContainer() {
      if (parent != null) {
        parent.removeTranslatableString(propertyName);
        allProperties.remove(this);
      }
    }

    @Override
    public String getPropertyValue() {
      return value;
    }

    public String getName() {
      return propertyName;
    }

    @Override
    public TranslatableStringContainer getParent() {
      return parent;
    }

    @Override
    public void setPropertyValue(String newValue) {
      if (newValue == null) {
        newValue = "";
      }
      final String oldValue = value;
      value = newValue;
      propSupport.firePropertyChange(propertyName, oldValue, newValue);
    }
  }
}
