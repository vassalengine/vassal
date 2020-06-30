package VASSAL.tools.ipc;

public class SimpleIPCMessage implements IPCMessage {
  private static final long serialVersionUID = 1L;

  protected long id;
  protected long inReplyTo;

  @Override
  public long getId() {
    return id;
  }

  @Override
  public void setId(long id) {
    this.id = id;
  }

  @Override
  public boolean expectsReply() {
    return false;
  }

  @Override
  public boolean isReply() {
    return false;
  }

  @Override
  public long getInReplyTo() {
    return inReplyTo;
  }

  @Override
  public void setInReplyTo(long inReplyTo) {
    this.inReplyTo = inReplyTo;
  }

  @Override
  public int hashCode() {
    return (int)(id ^ (id >>> 32));
  }

  @Override
  public boolean equals(Object o) {
    if (o == this) return true;
    if (!(o instanceof IPCMessage)) return false;

    return id == ((IPCMessage) o).getId();
  }

  @Override
  public String toString() {
    return getClass().getName() + "[id=" + id + ",inReplyTo=" + inReplyTo + "]";
  }
}
