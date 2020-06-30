/*
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

/**
 * An interface for handling events.
 *
 * @author Joel Uckelman
 * @since 3.2.0
 * @see EventListenerSupport
 */
public interface EventListener<T> {
  /**
   * Receive notification of an event.
   *
   * @param src the source of the event
   * @param event the event
   */
  public void receive(Object src, T event);
}
