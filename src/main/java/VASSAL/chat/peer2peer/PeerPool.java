/*
 * $Id:
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.chat.peer2peer;

import java.io.IOException;

import org.litesoft.p2pchat.PeerInfo;
import org.litesoft.p2pchat.PendingPeerManager;

/**
 * Date: Mar 11, 2003
 */
public interface PeerPool {
  public void initialize(P2PPlayer myInfo, PendingPeerManager ppm) throws IOException;
  public void disconnect();
  public void connectFailed(PeerInfo peerInfo);
}
