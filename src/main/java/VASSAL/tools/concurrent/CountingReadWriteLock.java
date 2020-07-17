/*
 *
 * Copyright (c) 2009-2010 by Joel Uckelman
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

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.AbstractQueuedSynchronizer;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

/**
 * A {@link ReadWriteLock} which counts locks held.
 *
 * Locks are not associated with particular threads as with
 * {@link ReentrantReadWriteLock}, so can be released from threads other
 * than the the ones which acquired them.
 *
 * @since 3.2.0
 * @author Joel Uckelman
 */
public class CountingReadWriteLock implements ReadWriteLock {
  /** {@inheritDoc} */
  @Override
  public Lock readLock()  { return r; }

  /** {@inheritDoc} */
  @Override
  public Lock writeLock() { return w; }

  protected final ReadLock r  = new ReadLock();
  protected final WriteLock w = new WriteLock();

  protected final Sync sync = new Sync();

  protected static abstract class BaseLock implements Lock {
    @Override
    public void lockInterruptibly() {
      throw new UnsupportedOperationException();
    }

    @Override
    public Condition newCondition() {
      throw new UnsupportedOperationException();
    }
  }

  protected class ReadLock extends BaseLock {
    @Override
    public void lock()    { sync.acquireShared(0); }
    @Override
    public void unlock()  { sync.releaseShared(0); }

    @Override
    public boolean tryLock() { return sync.tryAcquireShared(0) >= 0; }

    @Override
    public boolean tryLock(long time, TimeUnit unit)
                                                  throws InterruptedException {
      return sync.tryAcquireSharedNanos(0, unit.toNanos(time));
    }
  }

  protected class WriteLock extends BaseLock {
    @Override
    public void lock()   { sync.acquire(0); }
    @Override
    public void unlock() { sync.release(0); }

    @Override
    public boolean tryLock() { return sync.tryAcquire(0); }

    @Override
    public boolean tryLock(long time, TimeUnit unit)
                                                  throws InterruptedException {
      return sync.tryAcquireNanos(0, unit.toNanos(time));
    }
  }

  // Read states are positive, the write state is -1.
  // State 0 means that no locks are held.

  protected static class Sync extends AbstractQueuedSynchronizer {
    private static final long serialVersionUID = 1L;

    @Override
    protected boolean tryAcquire(int dummy) {
      return compareAndSetState(0, -1);
    }

    @Override
    protected boolean tryRelease(int dummy) {
      if (getState() != -1) throw new IllegalMonitorStateException();
      return compareAndSetState(-1, 0);
    }

    @Override
    protected int tryAcquireShared(int dummy) {
      final int s = getState();
      return s >= 0 && compareAndSetState(s, s+1) ? 1 : -1;
    }

    @Override
    protected boolean tryReleaseShared(int dummy) {
      final int s = getState();
      if (s < 1) throw new IllegalMonitorStateException();
      return compareAndSetState(s, s-1);
    }
  }
}
