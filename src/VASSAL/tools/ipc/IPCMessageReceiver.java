package VASSAL.tools.ipc;

import java.io.IOException;
import java.io.InvalidClassException;
import java.io.ObjectInput;
import java.io.OptionalDataException;
import java.io.StreamCorruptedException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.tools.concurrent.listener.MultiEventListenerSupport;
import VASSAL.tools.io.IOUtils;

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

  public void run() {
    IPCMessage msg;

    try {
      do {
        msg = (IPCMessage) in.readObject();

//System.err.println("received " + msg);

        lsup.notify(msg);
      } while (!(msg instanceof Fin));

      in.close();
    }
    catch (ClassCastException e) {
      throw new IllegalStateException(e);
    }
    catch (ClassNotFoundException e) {
      throw new IllegalStateException(e);
    }
    catch (InvalidClassException e) {
      throw new IllegalStateException(e);
    }
    catch (StreamCorruptedException e) {
      throw new IllegalStateException(e);
    }
    catch (OptionalDataException e) {
      throw new IllegalStateException(e);
    }
    catch (IOException e) {
// FIXME: should communicate this outward somehow
      logger.error("", e);
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }
}
