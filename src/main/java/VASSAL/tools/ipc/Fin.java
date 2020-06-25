package VASSAL.tools.ipc;

class Fin extends SimpleIPCMessage {
  private static final long serialVersionUID = 1L;

  public Fin() {}

  public Fin(Halt halt) {
    setInReplyTo(halt.getId());
  }

  @Override
  public boolean isReply() {
    return true;
  }
}
