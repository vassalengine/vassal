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

/**
 * An abstract base class for {@link PropertyContainer}s.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public abstract class AbstractPropertyContainer implements PropertyContainer {
  /**
   * Creates an <code>AbstractPropertyContainer</code>.
   */
  public AbstractPropertyContainer() {
    this(new ConcurrentPropertySupport());
  }

  /**
   * Creates an <code>AbstractPropertyContainer</code>.
   *
   * @param ps the {@link PropertySupport}
   */
  public AbstractPropertyContainer(PropertySupport ps) {
    this.ps = ps;
  }

  protected final PropertySupport ps;

  /** {@inheritDoc} */
  public void addPropertyListener(PropertyListener<Object> l) {
    ps.addPropertyListener(l);
  }

  /** {@inheritDoc} */
  public <T> void addPropertyListener(Property<T> prop,
                                      PropertyListener<? super T> l) {
    ps.addPropertyListener(prop, l);
  }

  /** {@inheritDoc} */
  public void removePropertyListener(PropertyListener<Object> l) {
    ps.removePropertyListener(l);
  }

  /** {@inheritDoc} */
  public <T> void removePropertyListener(Property<T> prop,
                                         PropertyListener<? super T> l) {
    ps.removePropertyListener(prop, l);
  }

  /** {@inheritDoc} */
  public List<PropertyListener<Object>> getPropertyListeners() {
    return ps.getPropertyListeners();
  }

  /** {@inheritDoc} */
  public <T> List<PropertyListener<? super T>>
                                       getPropertyListeners(Property<T> prop) {
    return ps.getPropertyListeners(prop);
  }

  /** {@inheritDoc} */
  public boolean hasListeners() {
    return ps.hasListeners();
  }

  /** {@inheritDoc} */
  public <T> boolean hasListeners(Property<T> prop) {
    return ps.hasListeners(prop);
  }

  /** {@inheritDoc} */
  protected <T> void fireChanged(Property<T> prop, T oldVal, T newVal) {
    ps.fireChanged(this, prop, oldVal, newVal);
  }
}
