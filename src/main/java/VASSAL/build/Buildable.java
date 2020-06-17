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

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * A Buildable instance can be initialized from a configuration file.
 * The configuration file is an XML file in which each XML element
 * represents an object.  The Buildable objects are built into an
 * containment hierarchy that mirrors the XML structure.
 */
public interface Buildable {
  /**
   * Build the object
   * @param e the XML element containing the object data
   */
  public void build(Element e);

  /**
   * Adds this component to its parent.
   * In order to make Buildable objects extensible, the child
   * is reponsible for adding itself to the parent.  That way,
   * Buildable subcomponents can be defined in an extension package
   * without needing to modify the containing class.
   */
  public void addTo(Buildable parent);

  /**
   * Adds a child component.  Both this method and {@link #addTo} are
   * invoked when adding a child to a parent
   */
  public void add(Buildable child);

  /**
   * @return an XML element from which
   * this component can be built
   */
  public Element getBuildElement(Document doc);
}
