package VASSAL.tools.ipc;

import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.List;
import java.util.Map;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicLong;

import VASSAL.tools.concurrent.listener.DefaultMultiEventListenerSupport;
import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.concurrent.listener.MultiEventListenerSupport;

import com.google.common.util.concurrent.ValueFuture;

public class IPCMessenger {

  protected final AtomicLong next_id = new AtomicLong(0L);

  protected final Map<Long,ValueFuture<IPCMessage>> waiting =
    new ConcurrentHashMap<Long,ValueFuture<IPCMessage>>();

  protected final BlockingQueue<IPCMessage> outqueue =
    new LinkedBlockingQueue<IPCMessage>();

  protected final ObjectInputStream in;
  protected final ObjectOutputStream out;

  protected final MultiEventListenerSupport lsup;

  public IPCMessenger(InputStream in, OutputStream out,
                      MultiEventListenerSupport lsup) throws IOException {
    if (in == null) throw new IllegalArgumentException("in == null");
    if (out == null) throw new IllegalArgumentException("out == null");
    if (lsup == null) throw new IllegalArgumentException("lsup == null");

    this.out = new ObjectOutputStream(out);
    this.in = new ObjectInputStream(in);
    this.lsup = lsup;
  }

  public IPCMessenger(Socket sock) throws IOException {
    this(sock.getInputStream(), sock.getOutputStream());
  }

  public IPCMessenger(InputStream in, OutputStream out) throws IOException {
    if (in == null) throw new IllegalArgumentException("in == null");
    if (out == null) throw new IllegalArgumentException("out == null");

    this.out = new ObjectOutputStream(out);
    this.in = new ObjectInputStream(in);
    this.lsup = new DefaultMultiEventListenerSupport(this);

    lsup.addEventListener(IPCMessage.class, new EventListener<IPCMessage>() {
      public void receive(Object src, IPCMessage msg) {
        if (msg.isReply()) {
          final ValueFuture<IPCMessage> f = waiting.remove(msg.getInReplyTo());
          if (f == null) throw new IllegalStateException(msg.toString());
          f.set(msg);
        }
      }
    });

    lsup.addEventListener(Halt.class, new EventListener<Halt>() {
      public void receive(Object src, Halt halt) {
        try {
          send(new Fin(halt));
        }
        catch (IOException e) {
// FIXME: communcate this to a handler?
        }
      }
    });
  }

  public void start() throws IOException {
    final IPCMessageReceiver mr = new IPCMessageReceiver(in, lsup);
    new Thread(mr, "IPC receiver for " + hashCode()).start();

    final IPCMessageDispatcher md = new IPCMessageDispatcher(outqueue, out);
    new Thread(md, "IPC dispatcher for " + hashCode()).start();
  }

  public void stop() throws IOException {
    final Future<IPCMessage> f = send(new Halt());

    try {
      f.get();
    }
    catch (CancellationException e) {
      throw new IllegalStateException(e);
    }
    catch (ExecutionException e) {
      throw new IllegalStateException(e);
    }
    catch (InterruptedException e) {
      throw new IllegalStateException(e);
    }
  }

  public Future<IPCMessage> send(IPCMessage msg) throws IOException {
    if (msg == null) throw new IllegalArgumentException("msg == null");

    msg.setId(next_id.getAndIncrement());

    final ValueFuture<IPCMessage> f = ValueFuture.<IPCMessage>create();
    if (msg.expectsReply()) {
      waiting.put(msg.getId(), f);
    }
    else {
      f.set(null);
    }

    outqueue.offer(msg);

    return f;
  }

  /**
   * Adds an {@link EventListener}.
   *
   * @param c the class to listen for
   * @param l the listener to add
   */
  public <T> void addEventListener(Class<T> c, EventListener<? super T> l) {
    lsup.addEventListener(c, l);
  }

  /**
   * Removes an {@link EventListener}.
   *
   * @param c the class to check
   * @param l the listener to remove
   */
  public <T> void removeEventListener(Class<T> c, EventListener<? super T> l) {
    lsup.removeEventListener(c, l);
  }

  /**
   * Checks whether there are any {@link EventListener}s.
   *
   * @param c the class to check
   * @return <code>true</code> if there are any listeners for the given class
   */
  public boolean hasEventListeners(Class<?> c) {
    return lsup.hasEventListeners(c);
  }

  /**
   * Notify the listeners of an event.
   *
   * @param event the event to send
   */
  public <T> List<EventListener<? super T>> getEventListeners(Class<T> c) {
    return lsup.getEventListeners(c);
  }
}
