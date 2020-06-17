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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.concurrent.listener;

import java.util.Collections;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;
import org.junit.runner.RunWith;

import static org.junit.Assert.*;

@RunWith(JMock.class)
public class DefaultEventListenerSupportTest {

  protected final Mockery context = new JUnit4Mockery();

  protected static final EventListener<Boolean> dummy =
                                                 new EventListener<Boolean>() {
    public void receive(Object src, Boolean event) {}
  };

  @Test
  public void testHasEventListeners() {
    final EventListenerSupport<Boolean> lsup =
      new DefaultEventListenerSupport<Boolean>(this);

    lsup.addEventListener(dummy);
    assertTrue(lsup.hasEventListeners());
  }

  @Test
  public void testGetEventListeners() {
    final EventListenerSupport<Boolean> lsup =
      new DefaultEventListenerSupport<Boolean>(this);

    lsup.addEventListener(dummy);
    assertEquals(Collections.singletonList(dummy), lsup.getEventListeners());
  }

  @Test
  public void testRemoveEventListener() {
    final EventListenerSupport<Boolean> lsup =
      new DefaultEventListenerSupport<Boolean>(this);

    lsup.addEventListener(dummy);
    assertTrue(lsup.hasEventListeners());
    lsup.removeEventListener(dummy);
    assertFalse(lsup.hasEventListeners());
  }


  @Test
  @SuppressWarnings("unchecked")
  public void testNotify() {
    final EventListenerSupport<Boolean> lsup =
      new DefaultEventListenerSupport<Boolean>(this);

    final EventListener<Boolean> listener = context.mock(EventListener.class);

    context.checking(new Expectations() {
      {
        oneOf(listener).receive(DefaultEventListenerSupportTest.this, true);
        oneOf(listener).receive(DefaultEventListenerSupportTest.this, false);
      }
    });

    lsup.addEventListener(listener);

    lsup.notify(true);
    lsup.notify(false);
  }
}
