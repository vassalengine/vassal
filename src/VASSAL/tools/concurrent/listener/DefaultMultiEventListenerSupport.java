/*
 * $Id$
 *
 * Copyright (c) 2010 by Joel Uckelman
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

package VASSAL.tools.concurrent.listener;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * A class to provide support for {@link EventListener}s.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class DefaultMultiEventListenerSupport
         implements MultiEventListenerSupport {

  protected final ConcurrentMap<Class<?>,List<EventListener<?>>> listeners =
    new ConcurrentHashMap<Class<?>,List<EventListener<?>>>();

  protected final Object src;

  /**
   * Creates a <code>DefaultMultiEventListenerSupport</code>.
   *
   * @param src the source of events
   */
  public DefaultMultiEventListenerSupport(Object src) {
    this.src = src;
  }

  /** {@inheritDoc} */
  public <T> void addEventListener(Class<T> c, EventListener<? super T> l) {
    // ensure that a listener list exists for class c
    listeners.putIfAbsent(c, new CopyOnWriteArrayList<EventListener<?>>());

    // add the listener to the list for every supertype of c
    for (Map.Entry<Class<?>,List<EventListener<?>>> e : listeners.entrySet()) {
      final Class<?> other = e.getKey();
      if (c.isAssignableFrom(other)) {
        e.getValue().add(l);
      }
    }
  }

  /** {@inheritDoc} */
  public <T> void removeEventListener(Class<T> c, EventListener<? super T> l) {
    // add the listener to the list for every supertype of c
    for (Map.Entry<Class<?>,List<EventListener<?>>> e : listeners.entrySet()) {
      final Class<?> other = e.getKey();
      if (c.isAssignableFrom(other)) {
        e.getValue().remove(l);
      }
    }
  }

  /** {@inheritDoc} */
  public boolean hasEventListeners(Class<?> c) {
    // check for listeners for every supertype of c
    for (Map.Entry<Class<?>,List<EventListener<?>>> e : listeners.entrySet()) {
      final Class<?> other = e.getKey();
      if (other.isAssignableFrom(c)) {
        if (!e.getValue().isEmpty()) {
          return true;
        }
      }
    }

    return false;
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public <T> List<EventListener<? super T>> getEventListeners(Class<T> c) {
    final List<EventListener<? super T>> list =
      new ArrayList<EventListener<? super T>>();

    // make a list of all listeners for every supertype of c
    for (Map.Entry<Class<?>,List<EventListener<?>>> e : listeners.entrySet()) {
      final Class<?> other = e.getKey();
      if (other.isAssignableFrom(c)) {
        list.addAll((List) e.getValue());
      }
    }

    return list;
  }

  /** {@inheritDoc} */
  @SuppressWarnings("unchecked")
  public void notify(Object event) {
    final Class<?> c = event.getClass();

    List<EventListener<?>> list = listeners.get(c);
    if (list == null) list = registerType(c);

    for (EventListener l : list) {
      l.receive(src, c.cast(event));
    }
  }

  protected List<EventListener<?>> registerType(Class<?> c) {
    // ensure that a listener list exists for class c
    listeners.putIfAbsent(c, new CopyOnWriteArrayList<EventListener<?>>());

    final Set<EventListener<?>> lset = new HashSet<EventListener<?>>();

    // make a set of all listeners for every supertype of c
    for (Map.Entry<Class<?>,List<EventListener<?>>> e : listeners.entrySet()) {
      final Class<?> other = e.getKey();
      if (other.isAssignableFrom(c)) {
        lset.addAll(e.getValue());
      }
    }

    final List<EventListener<?>> list = listeners.get(c);
    list.addAll(lset);

    return list;
  }
}
