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

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * A Buildable is an object which can be initialized from an XML configuration file
 * (e.g. <code>"buildFile"</code>) in which each XML element represents an component object. The Buildable
 * objects are built into a component hierarchy that mirrors the XML structure. Many Buildables
 * in a vassal module are also {@link Configurable}, so that their attributes can be configured
 * in the Editor. SEE ALSO: {@link AbstractBuildable}, {@link AbstractConfigurable}
 */
public interface Buildable {
  /**
   * Build the object from the XML representation of the Buildable
   * @param e the XML element containing the object data
   */
  void build(Element e);

  /**
   * Adds this component to its parent. In order to make Buildable objects extensible, the child
   * is responsible for adding itself to the parent. That way, Buildable subcomponents can be defined
   * in an extension package without needing to modify the containing class.
   * @param parent parent Buildable to add this component to as a subcomponent.
   */
  void addTo(Buildable parent);

  /**
   * Adds a child component.  Both this method and {@link #addTo} are
   * invoked when adding a child to a parent
   * @param child Child to attach to this buildable as a subcomponent
   */
  void add(Buildable child);

  /**
   * @param doc XML file
   * @return an XML element from which this component can be built
   */
  Element getBuildElement(Document doc);

  /**
   * Is this component a reqired component within its parent?
   *
   * @return  true if component is mandatory
   */
  default boolean isMandatory() {
    return false;
  }

  /**
   * Is this component allowed to be moved around the Configure Tree?
   * @return true is component is movable
   */
  default boolean isMovable() {
    return true;
  }

  /**
   * Does this component need to be unique within it's parent?
   * @return
   */
  default boolean isUnique() {
    return false;
  }

}
