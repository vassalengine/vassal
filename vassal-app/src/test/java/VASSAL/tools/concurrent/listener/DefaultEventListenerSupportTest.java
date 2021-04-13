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

package VASSAL.tools.concurrent.listener;

import java.util.Collections;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class DefaultEventListenerSupportTest {

  protected static final EventListener<Boolean> dummy = (src, event) -> {};

  @Test
  public void testHasEventListeners() {
    final EventListenerSupport<Boolean> lsup = new DefaultEventListenerSupport<>(this);

    lsup.addEventListener(dummy);
    assertTrue(lsup.hasEventListeners());
  }

  @Test
  public void testGetEventListeners() {
    final EventListenerSupport<Boolean> lsup = new DefaultEventListenerSupport<>(this);

    lsup.addEventListener(dummy);
    assertEquals(Collections.singletonList(dummy), lsup.getEventListeners());
  }

  @Test
  public void testRemoveEventListener() {
    final EventListenerSupport<Boolean> lsup = new DefaultEventListenerSupport<>(this);

    lsup.addEventListener(dummy);
    assertTrue(lsup.hasEventListeners());
    lsup.removeEventListener(dummy);
    assertFalse(lsup.hasEventListeners());
  }


  @Test
  @SuppressWarnings("unchecked")
  public void testNotify() {
    final EventListenerSupport<Boolean> lsup = new DefaultEventListenerSupport<>(this);
    final EventListener<Boolean> listener = mock(EventListener.class);

    lsup.addEventListener(listener);

    lsup.notify(true);
    lsup.notify(false);

    verify(listener, times(1)).receive(DefaultEventListenerSupportTest.this, true);
    verify(listener, times(1)).receive(DefaultEventListenerSupportTest.this, false);
  }

}
