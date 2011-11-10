/*
 * $Id$
 *
 * Copyright (c) 2011 by Pieter Geerkens
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
package VASSAL.configure;

import java.util.Iterator;

import VASSAL.configure.Attribute.AbstractAttribute;

/** 
 * An interface definition of the methods from <code>AbstractConfigurable</code> 
 * to be implemented as a list of Attributes. All of the methods returning an array of 
 * <code>Attribute</code> properties (Type, Name or Description) must preserve the 
 * order in which attributes have been added. (One such implementation is provided 
 * by <code>LinkedHashMap</code>.) Implementation of this functionality in a utility 
 * superclass is expected to both simplify, and ease writing and maintenance of, 
 * the UI code for VASSAL.
 * @author Pieter Geerkens
 * 
 */
public interface AttributeList extends Iterable<AbstractAttribute<?>>{
  /**
   * Add an <code>AbstractAttribute<code> to the list.
   * @param attribute the <code>Attribute</code> to be appended
   */
  public void addAttribute (AbstractAttribute<?> attribute);
  
  /**
   * 
   * @param key the name of the attribute for which the (typed) value is desired.
   * @return a typed reference to the attribute (boxed if necessary). 
   */
  public AbstractAttribute<?> attribute(String key);
  
  /**
   * Returns an array of the <b>Types</b> by which the <code>Attribute</code>s in the List wish to be 
   * known as by <code>Configurer</code> & kin. As with the other array-returning methods,
   * the order in which attributes were added to the list must be preserved.   
   * @return an <b>array</b> of Class<?>.
   */
  public Class<?>[] getAttributeTypes();
  public String[] getAttributeDescriptions();
  public String[] getAttributeNames();
  public <T> T getAttributeValue(Class<T> clazz, String key);
  public String getAttributeValueString(String key);
  public void setAttribute(String key, Object value);
  public VisibilityCondition getAttributeVisibility(String key);
  
  /**
   * {@inheritDoc}
   * <p>Implementations must generate iterators that preserve the order in which 
   * <code>attributes</code> were added.
   * 
   * @return an iterator of <code>AbstractAttribute&lt;?></code> elements..
   */
  @Override
  public Iterator<AbstractAttribute<?>> iterator();
}
