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

import VASSAL.tools.concurrent.listener.EventListener;

import static VASSAL.launch.TileProgressPumpStateMachine.INIT;
import static VASSAL.launch.TileProgressPumpStateMachine.NAME;
import static VASSAL.launch.TileProgressPumpStateMachine.NAME_LF;
import static VASSAL.launch.TileProgressPumpStateMachine.DOTS;
import static VASSAL.launch.TileProgressPumpStateMachine.DOTS_LF;
import static VASSAL.launch.TileProgressPumpStateMachine.DONE;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class TileProgressPumpStateMachineTest {

  @Test
  @SuppressWarnings("unchecked")
  public void testINITtoNAME() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "xyzzy".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(NAME, sm.run(INIT, buf, 0, buf.length, sb));
    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testNAMEtoNAME() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "xyzzy".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(NAME, sm.run(NAME, buf, 0, buf.length, sb));
    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testNAMEtoNAME_LF() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "xyzzy\r".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(NAME_LF, sm.run(NAME, buf, 0, buf.length, sb));

    verify(nl).receive(any(TileProgressPumpStateMachine.class), eq("xyzzy"));
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testNAMEtoDOTS() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "xyzzy\n".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(DOTS, sm.run(NAME, buf, 0, buf.length, sb));

    verify(nl).receive(any(TileProgressPumpStateMachine.class), eq("xyzzy"));
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testNAMEtoDONE() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "\n".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(DONE, sm.run(NAME, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testNAME_LFtoDOTS() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "\n".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(DOTS, sm.run(NAME_LF, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testNAME_LFtoIllegal() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "x".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertThrows(IllegalStateException.class, () -> sm.run(NAME_LF, buf, 0, buf.length, sb));
    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDOTStoDOTS() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "....".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");
    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(DOTS, sm.run(DOTS, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl).receive(any(TileProgressPumpStateMachine.class), eq(4));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDOTStoDOTS_LF() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "..\r".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(DOTS_LF, sm.run(DOTS, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl).receive(any(Object.class), eq(2));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDOTStoNAME() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "..\n".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(NAME, sm.run(DOTS, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl).receive(any(Object.class), eq(2));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDOTStoIllegal() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "x".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertThrows(IllegalStateException.class, () -> sm.run(DOTS, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDOTS_LFtoNAME() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "\n".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertEquals(NAME, sm.run(DOTS_LF, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDOTS_LFtoIllegal() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "x".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertThrows(IllegalStateException.class, () -> sm.run(DOTS_LF, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testDONEtoIllegal() {
    final StringBuilder sb = new StringBuilder();
    final byte[] buf = "x".getBytes();

    final EventListener<String> nl = mock(EventListener.class, "nl");
    final EventListener<Integer> pl = mock(EventListener.class, "pl");

    final TileProgressPumpStateMachine sm = new TileProgressPumpStateMachine(nl, pl);

    assertThrows(IllegalArgumentException.class, () -> sm.run(DONE, buf, 0, buf.length, sb));

    verify(nl, never()).receive(any(Object.class), anyString());
    verify(pl, never()).receive(any(Object.class), anyInt());
  }

}
