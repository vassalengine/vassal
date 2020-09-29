package VASSAL.tools.ipc;

import java.io.Serializable;

public interface IPCMessage extends Serializable {
  long getId();

  void setId(long id);

  boolean expectsReply();

  boolean isReply();

  long getInReplyTo();

  void setInReplyTo(long id);
}
