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

/**
 * The listener interface for receiving {@link Property} change events.
 *
 * @param <T> the class of the value of the {@link Property} listened to
 * @since 3.2.0
 * @author Joel Uckelman
 */
public interface PropertyListener<T> {
  /**
   * Invoked when a {@link Property} change occurs.
   *
   * @param src the source of the change
   * @param prop the changed {@link Property}
   * @param oldVal the old value of the {@link Property}
   * @param newVal the new value of the {@link Property}
   */
  public <U extends T> void propertyChanged(Object src, Property<U> prop,
                                            U oldVal, U newVal);
}
