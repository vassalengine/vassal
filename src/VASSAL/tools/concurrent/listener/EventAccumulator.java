/*
 * $Id$
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

import org.apache.commons.lang3.tuple.ImmutablePair;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * An {@link EventListener} which accumulates the events it receives into
 * a {@link List}.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class EventAccumulator<T> implements EventListener<T> {

  private final Collection<ImmutablePair<Object,T>> sourceEventPairs;

  public EventAccumulator() {
    this(new ArrayList<>());
  }

  public EventAccumulator(Collection<ImmutablePair<Object,T>> sourceEventPairs) {
    this.sourceEventPairs = sourceEventPairs;
  }

  /** {@inheritDoc} */
  @Override
  public void receive(Object src, T event) {
    sourceEventPairs.add(new ImmutablePair<>(src, event));
  }

  /**
   * Gets the collection of received source-event pairs.
   *
   * @return the collection of sources and events received
   */
  public Collection<ImmutablePair<Object,T>> events() {
    return sourceEventPairs;
  }
}
