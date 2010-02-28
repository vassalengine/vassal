/*
 * $Id: GameModule.java 5488 2009-04-11 14:01:20Z uckelman $
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

import java.beans.PropertyChangeSupport;

import VASSAL.tools.Pair;

/**
 * A progress handler.
 *
 * @author Joel Uckelman
 * @since 3.1.11
 */
public class Progressor extends RangedRunnable<Integer> {
  protected final PropertyChangeSupport pcs;
  protected final int max;

  /**
   * Creates a {@code Progressor} with the given bounds.
   *
   * @param pcs the {@code PropertyChangeSupport} to use for updates
   * @param init the initial progress value
   * @param max the maximum progress value
   *
   * @throws IllegalArgumentException if {@code init} is not in {@code [0,max]}
   * @throws IllegalArgumentException if {@code max &lt; 0}
   */
  public Progressor(PropertyChangeSupport pcs, int init, int max) {
    super(init);

    if (init < 0 || init > max) throw new IllegalArgumentException();
    if (max < 0) throw new IllegalArgumentException();

    this.pcs = pcs;
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
    if (prog < 0 || prog > max) throw new IllegalArgumentException();
    if (prog <= get()) throw new IllegalArgumentException();

    // submit only if this change affects the integer percentage
    setLast(prog, (100*prog)/max > getPct());
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
    set(get()+1);
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
   * Updates the current progress via the {@link PropertyChangeSupport}.
   *
   * @param prog the range of progress since the last update
   */
  protected void run(Pair<Integer,Integer> prog) {
    pcs.firePropertyChange(
      "progress", (100*prog.first)/max, (100*prog.second)/max
    );
  }

  /**
   * Submits this {@link Runnable} to be run on the Event Dispatch Thread.
   */
  protected void submit() {
    EDT.execute(this);
  }
}
