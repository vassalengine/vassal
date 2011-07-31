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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import javax.swing.SwingUtilities;

import VASSAL.tools.concurrent.SimpleFuture;

import org.junit.Test;

import static org.junit.Assert.*;

public class EDTExecutorServiceTest {
  @Test
  public void testIsShutdownFalse() {
    final EDTExecutorService ex = new EDTExecutorService();
    assertFalse(ex.isShutdown());
  }

  @Test
  public void testIsShutdownTrue() {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();
    assertTrue(ex.isShutdown());
    assertTrue(ex.isShutdown());
  }

  @Test
  public void testShutdownNow() {
    final EDTExecutorService ex = new EDTExecutorService();
    final List<Runnable> rl = ex.shutdownNow();

    assertTrue(rl.isEmpty());
    assertTrue(ex.isShutdown());
  }

  @Test
  public void testIsTerminatedTrue() throws InterruptedException {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();
    assertTrue(ex.awaitTermination(1L, TimeUnit.SECONDS));
    assertTrue(ex.isTerminated());
  }

  @Test
  public void testIsTerminatedFalse() throws InterruptedException {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.submit(new Runnable() {
      public void run() {
        try {
          // wait a lot longer than awaitTermination() below
          Thread.sleep(100L);
        }
        catch (InterruptedException e) {
        }
      }
    });

    ex.shutdown();
    assertFalse(ex.awaitTermination(1L, TimeUnit.NANOSECONDS));
    assertFalse(ex.isTerminated());
  }

  @Test
  public void testSubmitRunnable() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();
    final Future<?> f = ex.submit(new Runnable() {
      public void run() {
        assertTrue(SwingUtilities.isEventDispatchThread());
      }
    });

    f.get();
  }

  @Test
  public void testSubmitRunnableWithValue() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();
    final Future<Double> f = ex.submit(new Runnable() {
      public void run() {
        assertTrue(SwingUtilities.isEventDispatchThread());
      }
    }, Math.PI);

    assertEquals(Math.PI, f.get().doubleValue(), 0.0);
  }

  @Test
  public void testSubmitCallable() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();
    final Future<Byte> f = ex.submit(new Callable<Byte>() {
      public Byte call() {
        assertTrue(SwingUtilities.isEventDispatchThread());
        return (byte) 0xfe;
      }
    });

    assertEquals((byte) 0xfe, f.get().byteValue());
  }

  @Test
  public void testSubmitEDTRunnableFuture() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();
    final Future<Short> f = ex.submit(new EDTRunnableFuture<Short>((short) 3) {
      protected void runOnEDT() {
        assertTrue(SwingUtilities.isEventDispatchThread());
      }
    });

    assertEquals(3, f.get().shortValue());
  }

  @Test
  public void testExecute() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();

    final SimpleFuture<Boolean> f = new SimpleFuture<Boolean>();

    final Runnable r = new Runnable() {
      public void run() {
        f.set(SwingUtilities.isEventDispatchThread());
      }
    };

    ex.execute(r);
    assertTrue(f.get());
  }

  @Test
  public void testInvokeAll() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();

    final List<Callable<Character>> tasks =
      new ArrayList<Callable<Character>>();
    for (int i = 0; i < 10; ++i) {
      tasks.add(new Callable<Character>() {
        public Character call() {
          assertTrue(SwingUtilities.isEventDispatchThread());
          return 'x';
        }
      });
    }

    for (Future<Character> f : ex.invokeAll(tasks)) {
      assertEquals('x', f.get().charValue());
    }
  }

  @Test
  public void testInvokeAllTimeout() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();

    final List<Callable<Character>> tasks =
      new ArrayList<Callable<Character>>();
    for (int i = 0; i < 10; ++i) {
      tasks.add(new Callable<Character>() {
        public Character call() {
          assertTrue(SwingUtilities.isEventDispatchThread());
          return 'x';
        }
      });
    }

    for (Future<Character> f : ex.invokeAll(tasks, 1L, TimeUnit.NANOSECONDS)) {
      if (!f.isCancelled()) assertEquals('x', f.get().charValue());
    }
  }

  @Test(expected=RejectedExecutionException.class)
  public void testSubmitRunnableAfterShutdown() {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();
    ex.submit(new Runnable() {
      public void run() {}
    });
  }

  @Test(expected=RejectedExecutionException.class)
  public void testSubmitRunnableWithValueAfterShutdown() {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();
    ex.submit(new Runnable() {
      public void run() {}
    }, Boolean.TRUE);
  }

  @Test(expected=RejectedExecutionException.class)
  public void testSubmitCallableAfterShutdown() {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();
    ex.submit(new Callable<Float>() {
      public Float call() { return 1.0f; }
    });
  }

  @Test(expected=RejectedExecutionException.class)
  public void testSubmitEDTRunnableFutureAfterShutdown() {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();
    ex.submit(new EDTRunnableFuture<Void>() {
      protected void runOnEDT() {}
    });
  }

  @Test(expected=RejectedExecutionException.class)
  public void testExecuteAfterShutdown() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();
    ex.submit(new Runnable() {
      public void run() {}
    });
  }

  @Test(expected=RejectedExecutionException.class)
  public void testInvokeAllAfterShutdown() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();

    final List<Callable<Boolean>> tasks = new ArrayList<Callable<Boolean>>();
    for (int i = 0; i < 10; ++i) {
      tasks.add(new Callable<Boolean>() {
        public Boolean call() { return Boolean.TRUE; }
      });
    }

    ex.invokeAll(tasks);
  }

  @Test(expected=RejectedExecutionException.class)
  public void testInvokeAllTimeoutAfterShutdown() throws Exception {
    final EDTExecutorService ex = new EDTExecutorService();
    ex.shutdown();

    final List<Callable<Boolean>> tasks = new ArrayList<Callable<Boolean>>();
    for (int i = 0; i < 10; ++i) {
      tasks.add(new Callable<Boolean>() {
        public Boolean call() { return Boolean.TRUE; }
      });
    }

    ex.invokeAll(tasks, 1L, TimeUnit.NANOSECONDS);
  }
}
