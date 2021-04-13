/*
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
import java.io.InputStream;
import java.io.IOException;

import org.apache.commons.io.input.ClosedInputStream;

import VASSAL.tools.concurrent.listener.DummyEventListener;
import VASSAL.tools.concurrent.listener.EventListener;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

public class TileProgressPumpTest {

  protected static class TPP extends TileProgressPump {
    public TPP() {
      super(null, null, new DummyEventListener<>());
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

  @Test
  public void testSetInputStreamRunning() {
    assertThrows(UnsupportedOperationException.class, () -> {
      final TileProgressPump p = new TPP();
      p.run();

      final InputStream in = new ClosedInputStream();
      p.setInputStream(in);
    });
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testRunOk() {
    final byte[] input = "xyzzy\n......\nfoo\n...\n\n".getBytes();
    final ByteArrayInputStream in = new ByteArrayInputStream(input);

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");
    final EventListener<IOException> el = mock(EventListener.class, "el");

    final TileProgressPump p = new TileProgressPump(nl, pl, el);
    p.setInputStream(in);
    p.run();

    verify(nl).receive(any(TileProgressPumpStateMachine.class), eq("xyzzy"));
    verify(pl).receive(any(TileProgressPumpStateMachine.class), eq(6));
    verify(nl).receive(any(TileProgressPumpStateMachine.class), eq("foo"));
    verify(pl).receive(any(TileProgressPumpStateMachine.class), eq(3));
    verify(el, never()).receive(any(TileProgressPumpStateMachine.class), any(IOException.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testRunShort() {
    final byte[] input = "x".getBytes();
    final ByteArrayInputStream in = new ByteArrayInputStream(input);

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");
    final EventListener<IOException> el = mock(EventListener.class, "el");

    final TileProgressPump p = new TileProgressPump(nl, pl, el);
    p.setInputStream(in);
    p.run();

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
    verify(el).receive(any(Object.class), any(IOException.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testPumpInClosed() {
    final InputStream in = new ClosedInputStream();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");
    final EventListener<IOException> el = mock(EventListener.class, "el");

    final TileProgressPump p = new TileProgressPump(nl, pl, el);
    p.setInputStream(in);
    p.run();

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
    verify(el, never()).receive(any(Object.class), any(IOException.class));
  }

}
