/*
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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Translatable;

/**
 * An object that implements the Configurable interface can, in
 * addition to being built from a configuration XML file, be
 * manipulated by the user directly in the VASSAL module editor via
 * the Configuration window.
 */
public interface Configurable extends Translatable {
  public static final String NAME_PROPERTY = "name"; //$NON-NLS-1$

  /**
   * Remove this component from its parent
   */
  public void removeFrom(Buildable parent);


  /**
   * Remove a child component
   */
  public void remove(Buildable child);

  /**
   * The name of this Configurable Object
   */
  public String getConfigureName();

  /**
   * @return a HelpFilte describing how to use and configure
   * this component
   */
  public HelpFile getHelpFile();

  /**
   * @return an array of Configurer objects representing
   * the Configurable children of this Configurable object
   */
  public Configurable[] getConfigureComponents();

  /**
   * Return a {@link Configurer} object which can be used to set the
   * attributes of this object
   */
  public Configurer getConfigurer();

  /**
   * Return a list of valid sub-component Classes.  If a Class
   * appears in this list, then instances of that class may be added
   * to this component from the Configuration Window.
   */
  public Class[] getAllowableConfigureComponents();

  /**
   * Add a PropertyChangeListener.  A PropertyChangeEvent should be fired
   * with property name {@link #NAME_PROPERTY} when the value returned from
   * {@link #getConfigureName} has changed
   */
  public void addPropertyChangeListener(java.beans.PropertyChangeListener l);
}
