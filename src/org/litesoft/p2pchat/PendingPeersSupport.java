package org.litesoft.p2pchat;

import java.io.*;

public interface PendingPeersSupport {
  boolean isAlreadyConnected(PeerInfo pPeerInfo);

  void addActivePeer(PeerInfo pPeerInfo, InputStream pInputStream, OutputStream pOutputStream);
  void addActivePeer(PeerInfo pPeerInfo, PeerReader reader, PeerWriter writer);
}

