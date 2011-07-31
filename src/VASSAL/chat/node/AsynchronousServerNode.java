/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
/*
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: May 9, 2003
 */
package VASSAL.chat.node;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;

import VASSAL.chat.HttpRequestWrapper;
import VASSAL.tools.PropertiesEncoder;


/**
 * Root node in a hierarchical server.
 * Represents the server process itself.
 * Children represent modules.
 * Children of modules represent rooms.
 * Children of rooms represent players.
 */
public class AsynchronousServerNode extends ServerNode {
  private static Logger logger =
    Logger.getLogger(AsynchronousServerNode.class.getName());
  private StatusReporter statusReporter;
  private ReportContentsThread contentsReporter;

  public AsynchronousServerNode(String url) {
    super();
    init(url);
  }

  protected void init(String url) {
    statusReporter = new StatusReporter(
      url == null ? null : new HttpRequestWrapper(url), this);
    contentsReporter = new ReportContentsThread(this);
  }

  protected synchronized void sendContents(Node node) {
    contentsReporter.markChanged(node);
  }

  public static class ReportContentsThread extends Thread {
    private AsynchronousServerNode server;
    private Set<Node> changed;
    private long lastGlobalUpdate;
    private static final long GLOBAL_UPDATE_INTERVAL = 1000L * 120L;

    public ReportContentsThread(AsynchronousServerNode server) {
      this.server = server;
      changed = new HashSet<Node>();
      start();
    }

    public void run() {
      while (true) {
        try {
          synchronized (this) {
            wait();
            sendContents();
          }
        }
        catch (InterruptedException e) {
        }
      }
    }

    private synchronized void sendContents() {
      server.statusReporter.updateContents(server.getLeafDescendants());
      long time = System.currentTimeMillis();
      Iterator<Node> modules;
      if (time - lastGlobalUpdate < GLOBAL_UPDATE_INTERVAL) {
        modules = Arrays.asList(server.getChildren()).iterator();
        lastGlobalUpdate = time;
      }
      else {
        modules = changed.iterator();
      }
      while (modules.hasNext()) {
        Node module = modules.next();
        logger.fine("Sending contents of "+module.getId()); //$NON-NLS-1$
        Node[] players = module.getLeafDescendants();
        Node[] rooms = module.getChildren();

        // Check if any rooms have lost their first player
        for (int i = 1; i < rooms.length; i++) {
          Node[] c = rooms[i].getChildren();
          if (c.length > 0) {
            try {
              final Properties roomProps = new PropertiesEncoder(rooms[i].getInfo()).getProperties();
              final String roomOwner = roomProps.getProperty("owner");
              final String playerId = new PropertiesEncoder(c[0].getInfo()).getProperties().getProperty("id");
              if (roomOwner == null || (! roomOwner.equals(playerId))) {
                roomProps.setProperty("owner", playerId);
                rooms[i].setInfo(new PropertiesEncoder(roomProps).toString());
              }
            }
            catch (IOException e) {
              // Error encoding/decoding properties. Shouldn't happen.
              e.printStackTrace();
            }
          }
        }

        String listCommand = Protocol.encodeListCommand(players);
        logger.finer(listCommand);
        module.send(listCommand);
        String roomInfo = Protocol.encodeRoomsInfo(rooms);
        module.send(roomInfo);
      }
      changed.clear();
    }

    public synchronized void markChanged(Node module) {
      logger.fine(module+" has changed"); //$NON-NLS-1$
      changed.add(module);
      notifyAll();
    }
  }
}
