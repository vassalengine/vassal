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
public class EventListenerSupport<T> {

  protected final List<EventListener<? super T>> listeners =
    new CopyOnWriteArrayList<EventListener<? super T>>();

  protected final Object src;

  /**
   * Creates an <code>EventListenerSupport</code>.
   *
   * @param src the source of events
   */
  public EventListenerSupport(Object src) {
    this.src = src;
  }

  /**
   * Adds an {@link EventListener}.
   *
   * @param l the listener to add
   */
  public void addEventListener(EventListener<? super T> l) {
    listeners.add(l);
  }

  /**
   * Removes an {@link EventListener}.
   *
   * @param l the listener to remove
   */
  public void removeEventListener(EventListener<? super T> l) {
    listeners.remove(l);
  }

  /**
   * Checks whether there are any {@link EventListener}s.
   *
   * @return <code>true</code> if there are any listeners
   */
  public boolean hasEventListeners() {
    return !listeners.isEmpty();
  }
 
  /**
   * Gets the list of listerners.
   *
   * @return the list of listeners
   */
  public List<EventListener<? super T>> getEventListeners() {
    return new ArrayList<EventListener<? super T>>(listeners);
  }

  /**
   * Notify the listeners of an event.
   *
   * @param event the event to send
   */
  public void notify(T event) {
    for (EventListener<? super T> l : listeners) l.receive(src, event);
  }
}
