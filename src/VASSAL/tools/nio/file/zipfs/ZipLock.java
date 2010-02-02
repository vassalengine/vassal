package VASSAL.tools.nio.file.zipfs;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.AbstractQueuedSynchronizer;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;

// Yellow and blue make green. Now it's sealed!
class ZipLock implements ReadWriteLock {
  public Lock readLock()  { return new ReadLock(); }
  public Lock writeLock() { return new WriteLock(); }
    
  private final Sync sync = new Sync();
  
  private class ReadLock implements Lock {
    public void lock()   { sync.acquireShared(0); }
    public void unlock() { sync.releaseShared(0); }

    public void lockInterruptibly() {
      throw new UnsupportedOperationException();
    }

    public Condition newCondition() {
      throw new UnsupportedOperationException();
    }
  
    public boolean tryLock() {
      throw new UnsupportedOperationException();
    }

    public boolean tryLock(long time, TimeUnit unit) {
      throw new UnsupportedOperationException();
    }
  }

  private class WriteLock implements Lock {
    public void lock()   { sync.acquire(0); }
    public void unlock() { sync.release(0); }

    public void lockInterruptibly() {
      throw new UnsupportedOperationException();
    }

    public Condition newCondition() {
      throw new UnsupportedOperationException();
    }
 
    public boolean tryLock() {
      throw new UnsupportedOperationException();
    }

    public boolean tryLock(long time, TimeUnit unit) {
      throw new UnsupportedOperationException();
    }
  }

  // Read states are positive, the write state is -1.
  // State 0 means that no locks are held.

  private static class Sync extends AbstractQueuedSynchronizer {
    private static final long serialVersionUID = 1L;

    @Override
    protected boolean tryAcquire(int dummy) {
      return compareAndSetState(0, -1);
    }

    @Override
    protected boolean tryRelease(int dummy) {
      final int s = getState();
      if (s != -1) throw new IllegalMonitorStateException("state == " + s);
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
      if (s < 1) throw new IllegalMonitorStateException("state == " + s);
      return compareAndSetState(s, s-1);
    }
  }
}
