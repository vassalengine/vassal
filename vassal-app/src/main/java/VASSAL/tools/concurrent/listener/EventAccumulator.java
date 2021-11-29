/*
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

package VASSAL.tools.concurrent.listener;

import java.util.ArrayList;
import java.util.Collection;

import VASSAL.tools.lang.Pair;

/**
 * An {@link EventListener} which accumulates the events it receives into
 * a {@link java.util.List}.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
@Deprecated(since = "2021-12-01", forRemoval = true)
public class EventAccumulator<T> implements EventListener<T> {

  protected final Collection<Pair<Object, T>> col;

  public EventAccumulator() {
    this(new ArrayList<>());
  }

  public EventAccumulator(Collection<Pair<Object, T>> col) {
    this.col = col;
  }

  /** {@inheritDoc} */
  @Override
  public void receive(Object src, T event) {
    col.add(new Pair<>(src, event));
  }

  /**
   * Gets the collection of received source-event pairs.
   *
   * @return the collection of sources and events received
   */
  public Collection<Pair<Object, T>> events() {
    return col;
  }
}
