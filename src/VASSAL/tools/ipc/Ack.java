package VASSAL.tools.ipc;

class Ack extends SimpleIPCMessage {
  private static final long serialVersionUID = 1L;

  public Ack() {}

  public Ack(IPCMessage msg) {
    setInReplyTo(msg.getId());
  }

  @Override
  public boolean isReply() {
    return true;
  }
}
