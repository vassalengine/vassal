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

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class SimpleFutureTest {

  @Test
  public void testCancelReturnValueInitially() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    assertTrue(f.cancel(true));
  }

  @Test
  public void testCancelReturnValueAfterDone() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.set(42);
    assertFalse(f.cancel(true));
  }

  @Test
  public void testCancelReturnValueAfterCancel() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    assertTrue(f.cancel(true));
    assertFalse(f.cancel(true));
  }

  @Test
  public void testNotCancelledInitially() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    assertFalse(f.isCancelled());
  }

  @Test
  public void testNotCancelledAfterSet() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.set(42);
    assertFalse(f.isCancelled());
  }

  @Test
  public void testNotCancelledAfterSetException() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.setException(new Exception());
    assertFalse(f.isCancelled());
  }

  @Test
  public void testCancelledAfterCancel() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.cancel(true);
    assertTrue(f.isCancelled());
  }

  @Test
  public void testNotDoneInitially() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    assertFalse(f.isDone());
  }

  @Test
  public void testDoneAfterSet() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.set(42);
    assertTrue(f.isDone());
  }

  @Test
  public void testDoneAfterSetException() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.setException(new Exception());
    assertTrue(f.isDone());
  }

  @Test
  public void testDoneAfterCancel() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.cancel(true);
    assertTrue(f.isDone());
  }

  @Test
  public void testGetAfterSet()
                              throws ExecutionException, InterruptedException {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.set(42);
    assertEquals(42, (int) f.get());
  }

  @Test
  public void testGetAfterSetException() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();

    final Exception ex = new Exception();
    f.setException(ex);

    ExecutionException e = assertThrows(ExecutionException.class, () -> f.get());
    assertEquals(ex, e.getCause());
  }

  @Test
  public void testGetAfterCancel() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.cancel(true);

    assertThrows(CancellationException.class, () -> f.get());
  }

  @Test
  public void testGetTimeoutAfterSet() throws ExecutionException,
                                              InterruptedException,
                                              TimeoutException {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.set(42);
    assertEquals(42, (int) f.get(1, TimeUnit.NANOSECONDS));
  }

  @Test
  public void testGetTimeoutAfterSetException() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();

    final Exception ex = new Exception();
    f.setException(ex);

    ExecutionException e = assertThrows(ExecutionException.class, () -> f.get(1, TimeUnit.NANOSECONDS));
    assertEquals(ex, e.getCause());
  }

  @Test
  public void testGetTimeoutAfterCancel() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    f.cancel(true);
    assertThrows(CancellationException.class, () -> f.get(1, TimeUnit.NANOSECONDS));
  }

  @Test
  public void testGetTimeoutAfterTimeout() {
    final SimpleFuture<Integer> f = new SimpleFuture<>();
    assertThrows(TimeoutException.class, () -> f.get(1, TimeUnit.NANOSECONDS));
  }
}
