/*
 * $Id$
 *
 * Copyright (c) 2007-2008 by Joel Uckelman
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

package VASSAL.tools.opcache;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;

/**
 * An <code>OpObserver</code> is notified when its {@link Op} completes..
 *
 * <p>Objects implementing <code>OpObserver</code> are intended to
 * be passed as an argument to {@link Op.get(Key,OpObserver)} or
 * {@link Op.getFuture(Key,OpObserver)} which which call back to
 * {@link #imageOpChange} on completion.</p>
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public interface OpObserver<V> {
  /**
   * Callback which is run when the observed <code>Op</code> completes
   * successfully.
   *
   * @param op the successfully completed <code>Op</code>
   * @param val the value of the <code>Op</code>
   */
  public void succeeded(Op<V> op, V val);

  /**
   * Callback which is run when the observed <code>Op</code> is cancelled.
   *
   * @param op the cancelled <code>Op</code>
   * @param e the <code>CancellationException</code> thrown when the
   *  <code>Op</code> was cancelled
   */
  public void cancelled(Op<V> op, CancellationException e);

  /**
   * Callback which is run when the observed <code>Op</code> is interrupted.
   *
   * @param op the interrupted <code>Op</code>
   * @param e the <code>InterruptedException</code> thrown when the
   *  <code>Op</code> was interrupted
   */
  public void interrupted(Op<V> op, InterruptedException e);

  /**
   * Callback which is run when the observed <code>Op</code> fails.
   *
   * @param op the failed <code>Op</code>
   * @param e the <code>ExecutionException</code> thrown when the
   *  <code>Op</code> failed
   */
  public void failed(Op<V> op, ExecutionException e);
}
