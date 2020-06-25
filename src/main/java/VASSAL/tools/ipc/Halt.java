package VASSAL.tools.ipc;

class Halt extends SimpleIPCMessage {
  private static final long serialVersionUID = 1L;

  @Override
  public boolean expectsReply() {
    return true;
  }
}
