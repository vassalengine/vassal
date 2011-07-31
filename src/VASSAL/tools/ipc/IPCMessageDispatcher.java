package VASSAL.tools.ipc;

import java.io.IOException;
import java.io.ObjectOutput;
import java.util.concurrent.BlockingQueue;

import VASSAL.tools.io.IOUtils;

public class IPCMessageDispatcher implements Runnable {

  protected final BlockingQueue<IPCMessage> queue;
  protected final ObjectOutput out;

  public IPCMessageDispatcher(BlockingQueue<IPCMessage> queue,
                              ObjectOutput out) {
    this.queue = queue;
    this.out = out;
  }

  public void run() {
    IPCMessage msg;

    try {
      do {
        msg = queue.take();
        out.writeObject(msg);
        out.flush();
      } while (!(msg instanceof Fin));

      out.close();
    }
    catch (IOException e) {
// FIXME
      e.printStackTrace();
    }
    catch (InterruptedException e) {
// FIXME
      e.printStackTrace();
    }
    finally {
      IOUtils.closeQuietly(out);
    }
  }
}
