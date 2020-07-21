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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.tools.ipc;

import java.io.IOException;
import java.io.ObjectInput;

import VASSAL.tools.concurrent.listener.MultiEventListenerSupport;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(JMock.class)
public class IPCMessageReceiverTest {

  public final Mockery context = new JUnit4Mockery();

  @Test
  public void testRun() throws ClassNotFoundException, IOException {
    final IPCMessage[] msg = new IPCMessage[100];
    for (int i = 0; i < msg.length; ++i) {
      msg[i] = new SimpleIPCMessage();
      msg[i].setId(i);
    }

    final Fin fin = new Fin();
    fin.setId(msg.length);

    final ObjectInput in = context.mock(ObjectInput.class);

    final MultiEventListenerSupport lsup =
      context.mock(MultiEventListenerSupport.class);

    context.checking(new Expectations() {
      {
        for (IPCMessage m : msg) {
          oneOf(in).readObject(); will(returnValue(m));
          oneOf(lsup).notify(with(equal(m)));
        }

        oneOf(in).readObject(); will(returnValue(fin));
        oneOf(lsup).notify(with(equal(fin)));

        exactly(1).of(in).close();
      }
    });

    final IPCMessageReceiver rec = new IPCMessageReceiver(in, lsup);
    rec.run();
  }
}
