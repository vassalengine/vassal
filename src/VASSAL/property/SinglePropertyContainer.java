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
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * A {@link PropertyContainer} which holds one {@link Property}.
 *
 * This class is intended for testing purposes only. It will not
 * generally be useful to have a dedicated container which holds only
 * one {@code Property}.
 *
 * @param <V> the class of the value of the contained {@link Property}
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class SinglePropertyContainer<V> implements PropertyContainer {

  protected final Property<V> prop;
  protected V value;

  /**
   * Creates a container for a single {@link Property}.
   *
   * @param prop the property
   * @param value the initial value of the property
   * @throws IllegalArgumentException if {@code prop} or {@code value} is
   * {@code null}
   */
  public SinglePropertyContainer(Property<V> prop, V value) {
    if (prop == null) throw new IllegalArgumentException();
    if (value == null) throw new IllegalArgumentException();

    this.prop = prop;
    this.value = value;
  }

  /** {@inheritDoc} */
  public Set<Property<?>> getProperties() {
    return Collections.<Property<?>>singleton(prop);
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public <T> void setValue(Property<T> prop, T value) {
    if (!this.prop.equals(prop)) throw new IllegalArgumentException();
    if (value == null) throw new IllegalArgumentException();

    if (this.value.equals(value)) return;

    final V oldVal = this.value;
    this.value = (V) value;

    for (PropertyListener<Object> l : listeners) {
      l.propertyChanged(this, this.prop, oldVal, this.value);
    }

    for (PropertyListener<? super V> l : plisteners) {
      l.propertyChanged(this, this.prop, oldVal, this.value);
    }
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public <T> T getValue(Property<T> prop) {
    if (!this.prop.equals(prop)) throw new IllegalArgumentException();
    return (T) value;
  }

  protected final CopyOnWriteArrayList<PropertyListener<Object>> listeners =
    new CopyOnWriteArrayList<PropertyListener<Object>>();

  protected final CopyOnWriteArrayList<PropertyListener<? super V>> plisteners =
    new CopyOnWriteArrayList<PropertyListener<? super V>>();

  /** {@inheritDoc} */
  public void addPropertyListener(PropertyListener<Object> l) {
    listeners.add(l);
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public <T> void addPropertyListener(Property<T> prop,
                                      PropertyListener<? super T> l) {
    if (!this.prop.equals(prop)) throw new IllegalArgumentException();
    plisteners.add((PropertyListener<? super V>) l);
  }

  /** {@inheritDoc} */
  public void removePropertyListener(PropertyListener<Object> l) {
    listeners.remove(l);
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public <T> void removePropertyListener(Property<T> prop,
                                         PropertyListener<? super T> l) {
    if (!this.prop.equals(prop)) throw new IllegalArgumentException();
    plisteners.remove((PropertyListener<? super V>) l);
  }

  /** {@inheritDoc} */
  public List<PropertyListener<Object>> getPropertyListeners() {
    return listeners.isEmpty() ?
      Collections.<PropertyListener<Object>>emptyList() :
      new ArrayList<PropertyListener<Object>>(listeners);
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public <T> List<PropertyListener<? super T>>
                                       getPropertyListeners(Property<T> prop) {
    if (!this.prop.equals(prop)) throw new IllegalArgumentException();
    return plisteners.isEmpty() ?
      Collections.<PropertyListener<? super T>>emptyList() :
      new ArrayList<PropertyListener<? super T>>((List) plisteners);
  }

  /** {@inheritDoc} */
  public boolean hasListeners() {
    return !listeners.isEmpty();
  }

  /** {@inheritDoc} */
  public <T> boolean hasListeners(Property<T> prop) {
    if (!this.prop.equals(prop)) throw new IllegalArgumentException();
    return !plisteners.isEmpty();
  }
}
