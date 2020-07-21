package VASSAL.tools.ipc;

import java.io.IOException;
import java.io.ObjectOutput;
import java.util.concurrent.BlockingQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class IPCMessageDispatcher implements Runnable {

  private static final Logger log = LoggerFactory.getLogger(IPCMessageDispatcher.class);

  protected final BlockingQueue<IPCMessage> queue;
  protected final ObjectOutput out;

  public IPCMessageDispatcher(BlockingQueue<IPCMessage> queue,
                              ObjectOutput out) {
    this.queue = queue;
    this.out = out;
  }

  @Override
  public void run() {
    IPCMessage msg;

    try (out) {
      do {
        msg = queue.take();
        out.writeObject(msg);
        out.flush();
      } while (!(msg instanceof Fin));
    }
    catch (IOException | InterruptedException e) {
      // FIXME
      log.error("Error while writing into IPC channel", e);
    }
  }
}
