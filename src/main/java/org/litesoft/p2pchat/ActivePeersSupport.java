package org.litesoft.p2pchat;

public interface ActivePeersSupport {
  void removeActivePeer(ActivePeer pRequester);

  PeerInfo[] getPeerInfos();
}

