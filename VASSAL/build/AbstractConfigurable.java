/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.Enumeration;
import java.util.Vector;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.VisibilityCondition;

/**
 * An abstract implementation of the Configurable interface.  Takes care of
 * most of the Configurable functionality
 */
public abstract class AbstractConfigurable extends AbstractBuildable implements AutoConfigurable {
  protected PropertyChangeSupport changeSupport;
  protected String name;
  protected Configurer config;

  /**
   * Remove a Buildable object from this object
   */
  public void remove(Buildable b) {
    buildComponents.remove(b);
  }

  public String getConfigureName() {
    return name;
  }

  /**
   * Sets the name and fires a PropertyChangeEvent
   */
  public void setConfigureName(String s) {
    String oldName = name;
    name = s;
    if (changeSupport != null) {
      changeSupport.firePropertyChange(NAME_PROPERTY, oldName, name);
    }
  }

  /**
   * Return an array of Strings describing the attributes
   * of this object.  These strings are used as prompts in
   * the Properties window for this object.  The order of
   * descriptions should be the same as the order of names
   * in {@link AbstractBuildable#getAttributeNames}
   */
  public abstract String[] getAttributeDescriptions();

  /**
   * Return the Class for the attributes of this object.
   * Valid classes are:  String, Integer, Double, Boolean, Image,
   * Color, and KeyStroke
   *
   * The order of classes should be the same as the order of names
   * in {@link AbstractBuildable#getAttributeNames} */
  public abstract Class[] getAttributeTypes();

  /**
   * By default, all attributes are visible
   * @param name
   * @return
   */
  public VisibilityCondition getAttributeVisibility(String name) {
    return null;
  }

  public void addPropertyChangeListener(PropertyChangeListener l) {
    if (changeSupport == null) {
      changeSupport = new PropertyChangeSupport(this);
    }
    changeSupport.addPropertyChangeListener(l);
  }

  public Configurable[] getConfigureComponents() {
    Vector v = new Vector();
    for (Enumeration e = getBuildComponents();
         e.hasMoreElements();) {
      Buildable b = (Buildable) e.nextElement();
      if (b instanceof Configurable) {
        v.addElement(b);
      }
    }
    Configurable c[] = new Configurable[v.size()];
    for (int i = 0; i < v.size(); ++i) {
      c[i] = (Configurable) v.elementAt(i);
    }
    return c;
  }

  /**
   * The default {@link Configurer} of an {@link AbstractConfigurable} class is an instance of {@link AutoConfigurer}
   */
  public Configurer getConfigurer() {
    if (config == null) {
      config = new AutoConfigurer(this);
    }
    else {
      ((AutoConfigurer) config).reset();
    }
    return config;
  }
}
