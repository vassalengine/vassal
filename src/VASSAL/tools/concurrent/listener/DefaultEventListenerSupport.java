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
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * A class to provide support for {@link EventListener}s.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class DefaultEventListenerSupport<T> implements EventListenerSupport<T> {

  protected final List<EventListener<? super T>> listeners =
    new CopyOnWriteArrayList<EventListener<? super T>>();

  protected final Object src;

  /**
   * Creates a <code>DefaultEventListenerSupport</code>.
   *
   * @param src the source of events
   */
  public DefaultEventListenerSupport(Object src) {
    this.src = src;
  }

  /** {@inheritDoc} */
  public void addEventListener(EventListener<? super T> l) {
    listeners.add(l);
  }

  /** {@inheritDoc} */
  public void removeEventListener(EventListener<? super T> l) {
    listeners.remove(l);
  }

  /** {@inheritDoc} */
  public boolean hasEventListeners() {
    return !listeners.isEmpty();
  }

  /** {@inheritDoc} */
  public List<EventListener<? super T>> getEventListeners() {
    return new ArrayList<EventListener<? super T>>(listeners);
  }

  /** {@inheritDoc} */
  public void notify(T event) {
    for (EventListener<? super T> l : listeners) l.receive(src, event);
  }
}
