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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.ipc;

import java.io.InputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

import VASSAL.tools.ThrowableUtils;
import VASSAL.tools.concurrent.SimpleFuture;
import VASSAL.tools.concurrent.listener.EventListener;
import VASSAL.tools.concurrent.listener.MultiEventListenerSupport;

import org.junit.Test;

import static org.junit.Assert.*;

public class IPCMessengerTest {
  @Test
  public void testSendNoReply() throws IOException, ClassNotFoundException {

    final PipedOutputStream out1 = new PipedOutputStream();
    final PipedInputStream in2 = new PipedInputStream(out1);

    final PipedOutputStream out2 = new PipedOutputStream();
    final PipedInputStream in1 = new PipedInputStream(out2);

    new Thread(new Runnable() {
      public void run() {
        try {
          final IPCMessenger ipc1 = new IPCMessenger(in1, out1);
          ipc1.start();
          for (int i = 0; i < 100; ++i) {
            ipc1.send(new SimpleIPCMessage());
          }
          ipc1.stop();
        }
        catch (Throwable e) {
          fail(ThrowableUtils.getStackTrace(e));
        }
      }
    }).start();

    final ObjectOutputStream out = new ObjectOutputStream(out2);
    final ObjectInputStream in = new ObjectInputStream(in2);

    Object o;

    // test message sending
    for (int i = 0; i < 100; ++i) {
      o = in.readObject();
      assertTrue(o instanceof SimpleIPCMessage);
      assertEquals(i, ((SimpleIPCMessage) o).getId());
    }

    // test shutdown
    o = in.readObject();
    assertTrue(o instanceof Halt);
    final Halt halt = new Halt();
    halt.setId(0);
    out.writeObject(halt);
    final Fin fin = new Fin((Halt) o);
    fin.setId(1);
    out.writeObject(fin);
    out.flush();
    o = in.readObject();
    assertTrue(o instanceof Fin);
    in.close();
  }

  @Test
  public void testSendWithReply() throws IOException, ClassNotFoundException {

    final PipedOutputStream out1 = new PipedOutputStream();
    final PipedInputStream in2 = new PipedInputStream(out1);

    final PipedOutputStream out2 = new PipedOutputStream();
    final PipedInputStream in1 = new PipedInputStream(out2);

    new Thread(new Runnable() {
      public void run() {
        try {
          final IPCMessenger ipc = new IPCMessenger(in1, out1);

          ipc.addEventListener(Ack.class, new EventListener<Ack>() {
            protected int irt = 0;

            public void receive(Object src, Ack ack) {
              assertEquals(irt++, ack.getInReplyTo());
            }
          });

          ipc.start();
          for (int i = 0; i < 100; ++i) {
            ipc.send(new SimpleIPCMessage());
          }
          ipc.stop();
        }
        catch (Throwable e) {
          fail(ThrowableUtils.getStackTrace(e));
        }
      }
    }).start();

    final ObjectOutputStream out = new ObjectOutputStream(out2);
    final ObjectInputStream in = new ObjectInputStream(in2);

    Object o;

    // test message sending
    for (int i = 0; i < 100; ++i) {
      o = in.readObject();
      assertTrue(o instanceof SimpleIPCMessage);
      final SimpleIPCMessage m = (SimpleIPCMessage) o;
      assertEquals(i, m.getId());
      out.writeObject(new Ack(m));
    }

    // test shutdown
    o = in.readObject();
    assertTrue(o instanceof Halt);
    final Halt halt = new Halt();
    halt.setId(0);
    out.writeObject(halt);
    final Fin fin = new Fin((Halt) o);
    fin.setId(1);
    out.writeObject(fin);
    out.flush();
    o = in.readObject();
    assertTrue(o instanceof Fin);
    in.close();
  }

  protected static class Msg extends SimpleIPCMessage {
    private static final long serialVersionUID = 1L;

    @Override
    public boolean expectsReply() { return true; }
  }

  protected Future<?> runIPCMessenger(String threadName,
                                      final InputStream in,
                                      final OutputStream out,
                                      final Future<IPCMessage>[] f) {
    final SimpleFuture<?> d = new SimpleFuture<Void>();

    new Thread(new Runnable() {
      public void run() {
        try {
          final IPCMessenger ipc = new IPCMessenger(in, out);

          ipc.addEventListener(Msg.class, new EventListener<Msg>() {
            public void receive(Object src, Msg msg) {
              try {
                ipc.send(new Ack(msg));
              }
              catch (IOException e) {
                fail(ThrowableUtils.getStackTrace(e));
              }
            }
          });

          ipc.start();
          for (int i = 0; i < f.length; ++i) {
            f[i] = ipc.send(new Msg());
          }
          ipc.stop();
          d.set(null);
        }
        catch (Throwable e) {
          fail(ThrowableUtils.getStackTrace(e));
        }
      }
    }, threadName).start();

    return d;
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testBidirectional() throws IOException,
                                         ClassNotFoundException,
                                         ExecutionException,
                                         InterruptedException {

    final Future<IPCMessage> f1[] = new Future[1000];
    final Future<IPCMessage> f2[] = new Future[1000];

    final PipedOutputStream out1 = new PipedOutputStream();
    final PipedInputStream in2 = new PipedInputStream(out1);

    final PipedOutputStream out2 = new PipedOutputStream();
    final PipedInputStream in1 = new PipedInputStream(out2);

    final Future<?> d1 = runIPCMessenger("ipc1", in1, out1, f1);
    final Future<?> d2 = runIPCMessenger("ipc2", in2, out2, f2);

    d1.get();
    d2.get();

    for (int i = 0; i < f1.length; ++i) {
      assertTrue(f1[i].get() instanceof Ack);
    }

    for (int i = 0; i < f2.length; ++i) {
      assertTrue(f2[i].get() instanceof Ack);
    }
  }
}
