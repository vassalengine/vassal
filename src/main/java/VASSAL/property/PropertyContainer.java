/*
 * $Id$
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.property;

import java.util.List;
import java.util.Set;

/**
 * Provides an interface for getting and setting {@link Property}s and
 * listening for changes to them.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public interface PropertyContainer {
  /**
   * Returns the {@link Property}s known to this container.
   *
   * @return a set of known {@link Property}s
   */
  public Set<Property<?>> getProperties();

  /**
   * Sets the value of a {@link Property}.
   *
   * @param prop the {@link Property}
   * @param value the value
   */
  public <T> void setValue(Property<T> prop, T value);

  /**
   * Gets the value of a {@link Property}.
   *
   * @param prop the {@link Property}
   * @return the value
   */
  public <T> T getValue(Property<T> prop);

  /**
   * Adds a listener for all changes to this container.
   *
   * @param listener the listener to add
   */
  public void addPropertyListener(PropertyListener<Object> listener);

  /**
   * Removes a listner for all changes to this container.
   *
   * @param listener the listener to remove
   */
  public void removePropertyListener(PropertyListener<Object> listener);

  /**
   * Checks whether there are {@link PropertyListener}s for all changes
   * to this container.
   *
   * @return {@code true} if there are listeners
   */
  public boolean hasListeners();

  /**
   * Returns a list of {@link PropertyListener}s listening to all changes
   * to this container.
   *
   * @return a list of listeners
   */
  public List<PropertyListener<Object>> getPropertyListeners();

  /**
   * Adds a listener for a specific {@link Property}.
   *
   * @param prop the {@link Property} to listen on
   * @param listener the listener to add
   */
  public <T> void addPropertyListener(Property<T> prop,
                                      PropertyListener<? super T> listener);

  /**
   * Removes a listener for a specific {@link Property}.
   *
   * @param prop the {@link Property} listened on
   * @param listener the listener to remove
   */
  public <T> void removePropertyListener(Property<T> prop,
                                         PropertyListener<? super T> listener);

  /**
   * Returns a list of {@link PropertyListener}s listening to a given
   * {@link Property}.
   *
   * @param prop the {@link Property} listened on
   * @return a list of listeners
   */
  public <T> List<PropertyListener<? super T>>
                                        getPropertyListeners(Property<T> prop);

  /**
   * Checks whether there are {@link PropertyListener}s for a given
   * {@link Property}.
   *
   * @return {@code true} if there are listeners for the given property
   */
  public <T> boolean hasListeners(Property<T> prop);
}
