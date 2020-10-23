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

import java.beans.PropertyChangeListener;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigureTree;
import VASSAL.i18n.Translatable;

/**
 * An object that implements the Configurable interface, in
 * addition to being built from a configuration XML file (because
 * this class ultimately extends {@link Buildable}, can be
 * manipulated by the user directly in the VASSAL module editor via
 * the Configuration window. It also extends the {@link Translatable}
 * interface which provides methods for the getting and setting of
 * XML attributes and adds i18n translation infrastructure.
 */
public interface Configurable extends Translatable {
  String NAME_PROPERTY = "name"; //$NON-NLS-1$

  /**
   * Remove this component from its parent
   */
  void removeFrom(Buildable parent);

  /**
   * Remove a child component
   */
  void remove(Buildable child);

  /**
   * The name of this Configurable Object
   */
  String getConfigureName();

  /**
   * @return a HelpFile describing how to use and configure    * this component
   */
  HelpFile getHelpFile();

  /**
   * @return an array of Configurer objects representing
   * the Configurable children of this Configurable object
   */
  Configurable[] getConfigureComponents();

  /**
   * @return a {@link Configurer} object which can be used to set the
   * attributes of this object
   */
  Configurer getConfigurer();

  /**
   * @return a list of valid sub-component Classes.  If a Class
   * appears in this list, then instances of that class may be added
   * to this component from the Editor's {@link ConfigureTree} window by
   * right-clicking on the component and selecting the appropriate "Add"
   * option.
   */
  Class[] getAllowableConfigureComponents();

  /**
   * Add a PropertyChangeListener.  A PropertyChangeEvent should be fired
   * with property name {@link #NAME_PROPERTY} when the value returned from
   * {@link #getConfigureName} has changed
   */
  void addPropertyChangeListener(PropertyChangeListener l);
}
