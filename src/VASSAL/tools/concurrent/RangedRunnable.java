/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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
package VASSAL.tools.concurrent;

import VASSAL.tools.lang.Pair;

/**
 * A {@link Runnable} which operates on a rolling range.
 *
 * A {@code RangedRunnable} can be updated after it has been submitted but
 * before it has been run. This cuts down on the number of times the
 * {@code RangedRunnable} must be submitted, and is appropriate for values
 * where only the most recent update matters.
 *
 * @param <T> the type of the lower and upper bounds in the range
 * @author Joel Uckelman
 * @since 3.1.11
 */
public abstract class RangedRunnable<T> implements Runnable {
  protected Pair<T,T> range;
  protected boolean submitted = false;

  /**
   * Creates a new {@code RangedRunnable} with the given lower bound.
   *
   * @param init the initial lower bound
   */
  public RangedRunnable(T init) {
    range = new Pair<T,T>(init, null);
  }

  /**
   * {@inheritDoc}
   *
   * <p>Flushes the range and calls {@see #run(Pair<T,T>)}.</p>
   */
  public final void run() {
    run(flush());
  }

  /**
   * Sets the value of the upper end of the range.
   *
   * @param last the value to set
   * @param submit whether to also submit the change
   */
  public final synchronized void setLast(T last, boolean submit) {
    if (last != range.second) {
      range = new Pair<T,T>(range.first, last);
    }

    if (submit && !submitted) {
      submit();
      submitted = true;
    }
  }

  /**
   * Hands the {@code RangedRunnable} off to be executed.
   *
   * <p>This method may be executed only from {@see #setLast}.</p>
   */
  protected abstract void submit();

  /**
   * Processes the given range.
   *
   * @param r the range to process
   */
  protected abstract void run(Pair<T,T> r);

  /**
   * Returns the old range and creates a new range adjacent to the old one.
   * The upper bound of the old range becomes the lower bound of the new range.
   *
   * @return the range being flushed
   */
  private final synchronized Pair<T,T> flush() {
    final Pair<T,T> flushed = range;
    range = new Pair<T,T>(flushed.second, null);
    submitted = false;
    return flushed;
  }
}
