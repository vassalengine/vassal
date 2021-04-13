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

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class CountingReadWriteLockTest {
  @Test
  void testReadLockReadUnlock() {
    assertDoesNotThrow(() -> {
      ReadWriteLock rwl = new CountingReadWriteLock();
      Lock r = rwl.readLock();
      r.lock();
      r.unlock();
    });
  }

  @Test
  public void testReadUnlock() {
    final ReadWriteLock rwl = new CountingReadWriteLock();
    final Lock r = rwl.readLock();
    assertThrows(IllegalMonitorStateException.class, () -> r.unlock());
  }

  @Test
  public void testWriteLockWriteUnlock() {
    assertDoesNotThrow(() -> {
      final ReadWriteLock rwl = new CountingReadWriteLock();
      final Lock w = rwl.writeLock();
      w.lock();
      w.unlock();
    });
  }

  @Test
  public void testWriteUnlock() {
    final ReadWriteLock rwl = new CountingReadWriteLock();
    final Lock w = rwl.writeLock();
    assertThrows(IllegalMonitorStateException.class, () -> w.unlock());
  }

  @Test
  public void testWriteLockBlocksReadLock() {
    final ReadWriteLock rwl = new CountingReadWriteLock();
    final Lock w = rwl.writeLock();
    w.lock();
    final Lock r = rwl.readLock();
    assertFalse(r.tryLock());
  }

  @Test
  public void testWriteLockBlocksWriteLock() {
    final ReadWriteLock rwl = new CountingReadWriteLock();
    final Lock w = rwl.writeLock();
    w.lock();
    assertFalse(w.tryLock());
  }

  @Test
  public void testReadLockBlocksWriteLock() {
    final ReadWriteLock rwl = new CountingReadWriteLock();
    final Lock r = rwl.readLock();
    r.lock();
    final Lock w = rwl.writeLock();
    assertFalse(w.tryLock());
  }

  @Test
  public void testReadLockDoesNotBlockReadLock() {
    final ReadWriteLock rwl = new CountingReadWriteLock();
    final Lock r = rwl.readLock();
    r.lock();
    assertTrue(r.tryLock());
  }
}
