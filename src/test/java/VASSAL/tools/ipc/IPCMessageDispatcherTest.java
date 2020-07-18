package VASSAL.tools.ipc;

import java.io.IOException;
import java.io.ObjectOutput;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(JMock.class)
public class IPCMessageDispatcherTest {

  protected final Mockery context = new JUnit4Mockery();

  @Test
  public void testRun() throws IOException, InterruptedException {
    final BlockingQueue<IPCMessage> queue =
      new LinkedBlockingQueue<IPCMessage>();

    final IPCMessage[] msg = new IPCMessage[100];
    for (int i = 0; i < msg.length; ++i) {
      msg[i] = new SimpleIPCMessage();
      msg[i].setId(i);
      queue.put(msg[i]);
    }

    final Fin fin = new Fin();
    fin.setId(msg.length);
    queue.put(fin);

    final ObjectOutput out = context.mock(ObjectOutput.class);

    context.checking(new Expectations() {
      {
        for (IPCMessage m : msg) {
          oneOf(out).writeObject(m);
        }

        exactly(msg.length+1).of(out).flush();

        oneOf(out).writeObject(fin);
        exactly(1).of(out).close();
      }
    });

    final IPCMessageDispatcher md = new IPCMessageDispatcher(queue, out);
    md.run();
  }
}
