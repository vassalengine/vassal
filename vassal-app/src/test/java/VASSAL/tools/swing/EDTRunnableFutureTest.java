/*
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

import java.util.concurrent.ExecutionException;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class EDTRunnableFutureTest {
  @Test
  public void testRunOk() throws Exception {
    final EDTRunnableFuture<Integer> r = new EDTRunnableFuture<>(42) {
      protected void runOnEDT() {}
    };

    r.run();
    assertEquals(42, r.get().intValue());
  }

  @Test
  public void testRunException() throws Exception {
    final EDTRunnableFuture<Boolean> r = new EDTRunnableFuture<>() {
      protected void runOnEDT() {
        throw new RuntimeException();
      }
    };

    r.run();
    assertThrows(ExecutionException.class, () -> r.get());
  }
}
