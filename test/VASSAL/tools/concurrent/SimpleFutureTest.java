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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.concurrent;

import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.TimeUnit;

import org.junit.Test;

import static org.junit.Assert.*;

public class SimpleFutureTest {

  @Test
  public void testCancelReturnValueInitially() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    assertTrue(f.cancel(true));
  }

  @Test
  public void testCancelReturnValueAfterDone() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.set(42);
    assertFalse(f.cancel(true));
  }

  @Test
  public void testCancelReturnValueAfterCancel() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    assertTrue(f.cancel(true));
    assertFalse(f.cancel(true));
  }

  @Test
  public void testNotCancelledInitially() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    assertFalse(f.isCancelled());
  }

  @Test
  public void testNotCancelledAfterSet() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.set(42);
    assertFalse(f.isCancelled());
  }

  @Test
  public void testNotCancelledAfterSetException() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.setException(new Exception());
    assertFalse(f.isCancelled());
  }

  @Test
  public void testCancelledAfterCancel() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.cancel(true);
    assertTrue(f.isCancelled());
  }

  @Test
  public void testNotDoneInitially() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    assertFalse(f.isDone());
  }

  @Test
  public void testDoneAfterSet() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.set(42);
    assertTrue(f.isDone());
  }

  @Test
  public void testDoneAfterSetException() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.setException(new Exception());
    assertTrue(f.isDone());
  }

  @Test
  public void testDoneAfterCancel() {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.cancel(true);
    assertTrue(f.isDone());
  }

  @Test
  public void testGetAfterSet()
                              throws ExecutionException, InterruptedException {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.set(42);
    assertEquals(42, (int) f.get());
  }

  @Test(expected=ExecutionException.class)
  public void testGetAfterSetException()
                              throws ExecutionException, InterruptedException {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();

    final Exception ex = new Exception();
    f.setException(ex);

    try {
      f.get();
    }
    catch (ExecutionException e) {
      assertEquals(ex, e.getCause());
      throw e;
    }
  }

  @Test(expected=CancellationException.class)
  public void testGetAfterCancel()
                              throws ExecutionException, InterruptedException {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.cancel(true);
    f.get();
  }

  @Test
  public void testGetTimeoutAfterSet() throws ExecutionException,
                                              InterruptedException,
                                              TimeoutException {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.set(42);
    assertEquals(42, (int) f.get(1, TimeUnit.NANOSECONDS));
  }

  @Test(expected=ExecutionException.class)
  public void testGetTimeoutAfterSetException() throws ExecutionException,
                                                       InterruptedException,
                                                       TimeoutException {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();

    final Exception ex = new Exception();
    f.setException(ex);

    try {
      f.get(1, TimeUnit.NANOSECONDS);
    }
    catch (ExecutionException e) {
      assertEquals(ex, e.getCause());
      throw e;
    }
  }

  @Test(expected=CancellationException.class)
  public void testGetTimeoutAfterCancel() throws ExecutionException,
                                                 InterruptedException,
                                                 TimeoutException {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.cancel(true);
    f.get(1, TimeUnit.NANOSECONDS);
  }

  @Test(expected=TimeoutException.class)
  public void testGetTimeoutAfterTimeout() throws ExecutionException,
                                                  InterruptedException,
                                                  TimeoutException {
    final SimpleFuture<Integer> f = new SimpleFuture<Integer>();
    f.get(1, TimeUnit.NANOSECONDS);
  }
}
