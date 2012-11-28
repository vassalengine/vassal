/*
 * $Id$
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.List;

import VASSAL.command.Command;

/**
 * A container for a String property that can be updated
 *
 * @author rkinney
 */
public interface MutableProperty {
  String getPropertyValue();

  Command setPropertyValue(String newValue);

  void addMutablePropertyChangeListener(PropertyChangeListener l);

  void removeMutablePropertyChangeListener(PropertyChangeListener l);

  MutablePropertiesContainer getParent();

  public static class Util {
    /**
     * Look for a {@link MutableProperty} in the list of {@link MutablePropertiesContainer}. Return the first one
     * found, searching the lists in order. The list may contain null references, which are skipped
     *
     * @param propertyContainers
     * @return
     */
    public static MutableProperty findMutableProperty(String propertyName, List<MutablePropertiesContainer> propertyContainers) {
      MutableProperty p = null;
      for (MutablePropertiesContainer c : propertyContainers) {
        p = (c == null ? null : c.getMutableProperty(propertyName));
        if (p != null) {
          break;
        }
      }
      return p;
    }
  }
  /**
   * Simple implementation of {@link MutableProperty} Support dynamic changing of the property name, provided that
   * the {@link #addTo(MutablePropertiesContainer)} method is used to register this property with a properties
   * container.
   *
   * @author rkinney
   *
   */
  public static class Impl implements MutableProperty {
    private PropertyChangeSupport propSupport;
    private String value="";
    private String propertyName;
    private MutablePropertiesContainer parent;

    // Maintain a static list of all Global Properties known to module
    private static List<Impl> allProperties = new ArrayList<Impl>();
    public static List<Impl> getAllProperties() {
      return allProperties;
    }

    /**
     *
     * @param source
     *          will be the source of any {@link PropertyChangeEvent} fired by this object
     */
    public Impl(String propertyName, Object source) {
      this.propertyName = propertyName;
      propSupport = new PropertyChangeSupport(this);
    }

    public void addMutablePropertyChangeListener(PropertyChangeListener l) {
      propSupport.addPropertyChangeListener(l);
    }

    public void addTo(MutablePropertiesContainer c) {
      parent = c;
      parent.addMutableProperty(propertyName, this);
      allProperties.add(this);
    }

    public void setPropertyName(String name) {
      if (parent != null) {
        parent.removeMutableProperty(propertyName);
        parent.addMutableProperty(name, this);
      }
      propertyName = name;
    }

    public void removeFromContainer() {
      if (parent != null) {
        parent.removeMutableProperty(propertyName);
        allProperties.remove(this);
      }
    }

    public String getPropertyValue() {
      return value;
    }

    public String getName() {
      return propertyName;
    }

    public MutablePropertiesContainer getParent() {
      return parent;
    }

    public void removeMutablePropertyChangeListener(PropertyChangeListener l) {
      propSupport.removePropertyChangeListener(l);
    }

    protected Command getChangeCommand(String oldValue, String newValue) {
      return new ChangePropertyCommand(this,propertyName, oldValue, newValue);
    }

    public Command setPropertyValue(String newValue) {
      if (newValue == null) {
        newValue = "";
      }
      String oldValue = value;
      Command c = getChangeCommand(value, newValue);
      value = newValue;
      propSupport.firePropertyChange(propertyName, oldValue, newValue);
      return c;
    }
  }
}
