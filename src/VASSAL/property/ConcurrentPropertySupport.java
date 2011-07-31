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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * A thread-safe implementation of {@link PropertySupport}.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class ConcurrentPropertySupport implements PropertySupport {

// FIXME: consider using WeakReferences for listeners to prevent memory leaks
  protected final CopyOnWriteArrayList<PropertyListener<Object>> listeners =
    new CopyOnWriteArrayList<PropertyListener<Object>>();

  /**
   * {@inheritDoc}
   *
   * @throws IllegalArgumentException if {@code l} is {@code null}
   */
  public void addPropertyListener(PropertyListener<Object> l) {
    if (l == null) throw new IllegalArgumentException();
    listeners.add(l);
  }

  /**
   * {@inheritDoc}
   *
   * @throws IllegalArgumentException if {@code l} is {@code null}
   */
  public void removePropertyListener(PropertyListener<Object> l) {
    if (l == null) throw new IllegalArgumentException();
    listeners.remove(l);
  }

  /** {@inheritDoc} */
  public boolean hasListeners() {
    return !listeners.isEmpty();
  }

  /** {@inheritDoc} */
  public List<PropertyListener<Object>> getPropertyListeners() {
    return listeners.isEmpty() ?
      Collections.<PropertyListener<Object>>emptyList() :
      new ArrayList<PropertyListener<Object>>(listeners);
  }

  protected final ConcurrentMap<Property<?>,List<PropertyListener<?>>>
    plisteners = new ConcurrentHashMap<Property<?>,List<PropertyListener<?>>>();

  /** {@inheritDoc} */
  public <T> void addPropertyListener(Property<T> prop,
                                      PropertyListener<? super T> l) {
    final List<PropertyListener<?>> empty =
      new CopyOnWriteArrayList<PropertyListener<?>>();

    List<PropertyListener<?>> list = plisteners.putIfAbsent(prop, empty);
    if (list == null) list = empty;

    list.add(l);
  }

  /** {@inheritDoc} */
  public <T> void removePropertyListener(Property<T> prop,
                                         PropertyListener<? super T> l) {
    final List<PropertyListener<?>> list = plisteners.get(prop);
    if (list != null) list.remove(l);
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public <T> List<PropertyListener<? super T>>
                                       getPropertyListeners(Property<T> prop) {
    final List<PropertyListener<?>> list = plisteners.get(prop);
    return list == null || list.isEmpty() ?
      Collections.<PropertyListener<? super T>>emptyList() :
      new ArrayList<PropertyListener<? super T>>((List) list);
  }

  /** {@inheritDoc} */
  public <T> boolean hasListeners(Property<T> prop) {
    final List<PropertyListener<?>> list = plisteners.get(prop);
    return list != null && !list.isEmpty();
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public <T> void fireChanged(Object src, Property<T> prop,
                              T oldVal, T newVal) {
    // do nothing if oldVal and newVal are the same
    if (oldVal == newVal || (oldVal != null && oldVal.equals(newVal))) return;

    // notify all general listeners
    for (PropertyListener<Object> l : listeners) {
      l.propertyChanged(src, prop, oldVal, newVal);
    }

    // notify all listeners on this property
    final List<PropertyListener<?>> list = plisteners.get(prop);
    if (list != null) {
      for (PropertyListener<?> l : list) {
        ((PropertyListener<? super T>) l).propertyChanged(
          src, prop, oldVal, newVal
        );
      }
    }
  }
}
