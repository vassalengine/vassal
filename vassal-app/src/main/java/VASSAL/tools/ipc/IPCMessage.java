package VASSAL.tools.ipc;

import java.io.Serializable;

public interface IPCMessage extends Serializable {
  public long getId();

  public void setId(long id);

  public boolean expectsReply();

  public boolean isReply();

  public long getInReplyTo();

  public void setInReplyTo(long id);
}
