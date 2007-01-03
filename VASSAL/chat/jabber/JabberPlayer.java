package VASSAL.chat.jabber;

import VASSAL.chat.SimplePlayer;

public class JabberPlayer extends SimplePlayer {
  private String jid;

  public JabberPlayer() {
    super();
  }

  public JabberPlayer(String name, String jid) {
    super(name);
    this.jid = jid;
  }

  public String getJid() {
    return jid;
  }
  
  public boolean equals(Object o) {
    return o instanceof JabberPlayer && jid.equals(((JabberPlayer)o).jid);
  }
  
  public int hashCode() {
    return jid.hashCode();
  }
}
