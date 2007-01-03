package VASSAL.chat.peer2peer;

import VASSAL.chat.Player;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SimpleStatus;
import VASSAL.tools.PropertiesEncoder;
import org.litesoft.p2pchat.PeerInfo;

import java.io.IOException;
import java.util.Properties;

public class P2PPlayer extends SimplePlayer {
  private static final String NAME = "name";
  private static final String ID = "id";
  private static final String ROOM = "room";
  private static final String LOOKING = "looking";
  private static final String AWAY = "away";
  private static final String PROFILE = "profile";


  private PeerInfo info;
  private Properties props;

  public P2PPlayer(PeerInfo info) {
    this.info = info;
    if (info.getChatName() != null) {
      try {
          props = new PropertiesEncoder(info.getChatName()).getProperties();
        setStats();
      }
      catch (IOException ex) {
        props = new Properties();
        setProps();
      }
    }
    else {
      props = new Properties();
      setProps();
    }
  }

  public void setStats(Player p) {
    setName(p.getName());
    setStatus(p.getStatus());
    setProps();
  }

  private void setProps() {
    props.put(NAME, getName());
    props.put(LOOKING, "" + ((SimpleStatus)status).isLooking());
    props.put(AWAY, "" + ((SimpleStatus)getStatus()).isAway());
    props.put(PROFILE, ((SimpleStatus)status).getProfile());
    info.setChatName(new PropertiesEncoder(props).getStringValue());
  }

  private void setStats() {
    setName(props.getProperty(NAME, "???"));
    setStatus(new SimpleStatus("true".equals(props.getProperty(LOOKING)),"true".equals(props.getProperty(AWAY)),props.getProperty(PROFILE, "")));
  }

  public String getRoom() {
    return props.getProperty(ROOM);
  }

  public void setRoom(String name) {
    props.put(ROOM, name);
    setProps();
  }

  public String getId() {
    return props.getProperty(ID);
  }

  public void setId(String id) {
    props.put(ID,id);
    setProps();
  }

  public boolean equals(Object o) {
    if (o instanceof P2PPlayer) {
      P2PPlayer p = (P2PPlayer) o;
      return getId() == null ? info.equals(p.info) : getId().equals(p.getId());
    }
    else {
      return false;
    }
  }

  public PeerInfo getInfo() {
    return info;
  }

  public String summary() {
    return getName() + " [looking = " + ((SimpleStatus)status).isLooking() + ", away = " + ((SimpleStatus)getStatus()).isAway() + ", room = " + props.getProperty(ROOM) + ", host = " + getInfo().getAddresses() + "]";
  }
}
