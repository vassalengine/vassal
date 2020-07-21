package VASSAL.tools.ipc;

import java.io.EOFException;
import java.io.IOException;
import java.io.InvalidClassException;
import java.io.ObjectInput;
import java.io.OptionalDataException;
import java.io.StreamCorruptedException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.concurrent.listener.MultiEventListenerSupport;

class IPCMessageReceiver implements Runnable {

  private static final Logger logger =
    LoggerFactory.getLogger(IPCMessageReceiver.class);

  protected final ObjectInput in;
  protected final MultiEventListenerSupport lsup;

  public IPCMessageReceiver(ObjectInput in,
                            MultiEventListenerSupport lsup) {
    this.in = in;
    this.lsup = lsup;
  }

  @Override
  public void run() {
    IPCMessage msg;

    try (in) {
      do {
        msg = (IPCMessage) in.readObject();

//System.err.println("received " + msg);

        lsup.notify(msg);
      } while (!(msg instanceof Fin));
    }
    catch (ClassCastException | OptionalDataException | StreamCorruptedException
      | InvalidClassException | ClassNotFoundException e) {
      throw new IllegalStateException(e);
    }
    catch (EOFException e) {
      // this is normal, happens on close
    }
    catch (IOException e) {
// FIXME: should communicate this outward somehow
      logger.error("Error while reading IPC message", e);
    }
  }
}
