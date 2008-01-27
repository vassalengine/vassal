package VASSAL.chat.peer2peer;

import org.litesoft.p2pchat.PeerInfo;
import org.litesoft.p2pchat.PendingPeerManager;

import java.io.IOException;

/**
 * Date: Mar 11, 2003
 */
public interface PeerPool {
  public void initialize(P2PPlayer myInfo, PendingPeerManager ppm) throws IOException;
  public void disconnect();
  public void connectFailed(PeerInfo peerInfo);
}
