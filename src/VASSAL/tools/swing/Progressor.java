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

package VASSAL.tools.swing;

import VASSAL.tools.concurrent.RangedRunnable;
import VASSAL.tools.lang.Pair;

/**
 * A progress handler.
 *
 * @author Joel Uckelman
 * @since 3.1.11
 */
public abstract class Progressor extends RangedRunnable<Integer> {
  protected final int max;

  /**
   * Creates a <code>Progressor</code> with the given bounds.
   *
   * @param init the initial progress value
   * @param max the maximum progress value
   *
   * @throws IllegalArgumentException if {@code init} is not in {@code [0,max]}
   * @throws IllegalArgumentException if {@code max &lt; 0}
   */
  public Progressor(int init, int max) {
    super(init);

    if (init < 0) throw new IllegalArgumentException();
    if (init > max) throw new IllegalArgumentException();
    if (max < 0) throw new IllegalArgumentException();

    this.max = max;
  }

  /**
   * Sets the progress.
   *
   * Setting the progress submits the {@link Runnable} only when it would
   * result in a change in the progress percentage.
   *
   * @param prog the new progress value
   * @throws IllegalArgumentException if {@code prog} is not in {@code [0,max]}
   * @throws IllegalArgumentException if {@code prog &lt;= } current value
   */
  public synchronized void set(int prog) {
    if (prog < 0) throw new IllegalArgumentException("prog == " + prog);
    if (prog > max) throw new IllegalArgumentException("prog == " + prog);

    final int cur = get();
    if (prog == cur) return;

    if (prog < cur) {
      throw new IllegalArgumentException(
        "prog == " + prog + ", get() == " + get()
      );
    }

    // submit only if this change affects the integer percentage
    setLast(prog, (100*prog)/max > (100*range.first)/max);
  }

  /**
   * Gets the progress.
   *
   * @return the progress value
   */
  public synchronized int get() {
    return range.second != null ? range.second : range.first;
  }

  /**
   * Increments the progress.
   */
  public synchronized void increment() {
    add(1);
  }

  /**
   * Adds some amount to the progress.
   */
  public synchronized void add(int p) {
    if (p != 0) set(get()+p);
  }

  /**
   * Sets the progress as a percentage.
   *
   * @param pct the progress percentage
   */
  public void setPct(int pct) {
    set((pct*max)/100);
  }

  /**
   * Gets the progress as a percentage.
   *
   * @return the progress percentage
   */
  public int getPct() {
    return (100*get())/max;
  }

  /**
   * Updates the current progress.
   *
   * @param prog the range of progress since the last update
   */
  protected abstract void run(Pair<Integer,Integer> prog);

  /**
   * Submits this {@link Runnable} to be run on the Event Dispatch Thread.
   */
  protected void submit() {
    EDT.execute(this);
  }
}
