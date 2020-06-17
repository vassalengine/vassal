/*
 * $Id$
 *
 * Copyright (c) 2008-2009 by Joel Uckelman
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

package VASSAL.launch;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.IOException;

import org.apache.commons.io.input.ClosedInputStream;

import VASSAL.tools.concurrent.listener.DummyEventListener;
import VASSAL.tools.concurrent.listener.EventListener;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;

@RunWith(JMock.class)
public class TileProgressPumpTest {
  protected final Mockery context = new JUnit4Mockery();

  protected static class TPP extends TileProgressPump {
    public TPP() {
      super(null, null, new DummyEventListener<IOException>());
    }

    @Override
    public void run() {
      // Use a dummy run() for testing stream setting
      running = true;
    }
  }

  @Test
  public void testSetInputStreamNotRunning() {
    final TileProgressPump p = new TPP();
    final InputStream in = new ClosedInputStream();
    p.setInputStream(in);
  }

  @Test(expected=UnsupportedOperationException.class)
  public void testSetInputStreamRunning() {
    final TileProgressPump p = new TPP();
    p.run();

    final InputStream in = new ClosedInputStream();
    p.setInputStream(in);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testRunOk() {
    final byte[] input = "xyzzy\n......\nfoo\n...\n\n".getBytes();
    final ByteArrayInputStream in = new ByteArrayInputStream(input);

    final EventListener<String> nl = context.mock(EventListener.class, "nl");
    final EventListener<Integer> pl = context.mock(EventListener.class, "pl");
    final EventListener<IOException> el =
      context.mock(EventListener.class, "el");

    context.checking(new Expectations() {
      {
        oneOf(nl).receive(with(aNonNull(TileProgressPumpStateMachine.class)),
                          with(equal("xyzzy")));
        oneOf(pl).receive(with(aNonNull(TileProgressPumpStateMachine.class)),
                          with(equal(6)));
        oneOf(nl).receive(with(aNonNull(TileProgressPumpStateMachine.class)),
                          with(equal("foo")));
        oneOf(pl).receive(with(aNonNull(TileProgressPumpStateMachine.class)),
                          with(equal(3)));
        never(el).receive(with(any(Object.class)),
                          with(any(IOException.class)));
      }
    });

    final TileProgressPump p = new TileProgressPump(nl, pl, el);
    p.setInputStream(in);
    p.run();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testRunShort() {
    final byte[] input = "x".getBytes();
    final ByteArrayInputStream in = new ByteArrayInputStream(input);

    final EventListener<String> nl = context.mock(EventListener.class, "nl");
    final EventListener<Integer> pl = context.mock(EventListener.class, "pl");
    final EventListener<IOException> el =
      context.mock(EventListener.class, "el");

    context.checking(new Expectations() {
      {
        never(nl).receive(with(any(Object.class)),
                          with(any(String.class)));
        never(pl).receive(with(any(Object.class)),
                          with(any(Integer.class)));
        oneOf(el).receive(with(aNonNull(TileProgressPumpStateMachine.class)),
                          with(aNonNull(IOException.class)));
      }
    });

    final TileProgressPump p = new TileProgressPump(nl, pl, el);
    p.setInputStream(in);
    p.run();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPumpInClosed() {
    final InputStream in = new ClosedInputStream();

    final EventListener<String> nl = context.mock(EventListener.class, "nl");
    final EventListener<Integer> pl = context.mock(EventListener.class, "pl");
    final EventListener<IOException> el =
      context.mock(EventListener.class, "el");

    context.checking(new Expectations() {
      {
        never(nl).receive(with(any(Object.class)),
                          with(any(String.class)));
        never(pl).receive(with(any(Object.class)),
                          with(any(Integer.class)));
        never(el).receive(with(any(Object.class)),
                          with(any(IOException.class)));
      }
    });

    final TileProgressPump p = new TileProgressPump(nl, pl, el);
    p.setInputStream(in);
    p.run();
  }
}
