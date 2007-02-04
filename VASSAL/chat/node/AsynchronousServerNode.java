/*
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

import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.logging.Logger;

import VASSAL.chat.HttpRequestWrapper;


/**
 * Root node in a hierarchical server.
 * Represents the server process itself.
 * Children represent modules.
 * Children of modules represent rooms.
 * Children of rooms represent players.
 */
public class AsynchronousServerNode extends ServerNode {
  private static Logger logger = Logger.getLogger(AsynchronousServerNode.class.getName());
  private StatusReporter statusReporter;
  private ReportContentsThread contentsReporter;

  public AsynchronousServerNode(String url) {
    super();
    init(url);
  }

  protected void init(String url) {
    statusReporter = new StatusReporter(url == null ? null : new HttpRequestWrapper(url), this);
    contentsReporter = new ReportContentsThread(this);
  }

  protected synchronized void sendContents(Node node) {
    contentsReporter.markChanged(node);
  }

  public static class ReportContentsThread extends Thread {
    private AsynchronousServerNode server;
    private HashSet changed;
    private long lastGlobalUpdate;
    private static final long GLOBAL_UPDATE_INTERVAL = 1000L * 120L;

    public ReportContentsThread(AsynchronousServerNode server) {
      this.server = server;
      changed = new HashSet();
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
      Iterator modules;
      if (time - lastGlobalUpdate < GLOBAL_UPDATE_INTERVAL) {
        modules = Arrays.asList(server.getChildren()).iterator();
        lastGlobalUpdate = time;
      }
      else {
        modules =  changed.iterator();
      }
      while (modules.hasNext()) {
        Node module = (Node) modules.next();
        logger.fine("Sending contents of "+module.getId());
        Node[] players = module.getLeafDescendants();
        Node[] rooms = module.getChildren();
        String listCommand = Protocol.encodeListCommand(players);
        logger.finer(listCommand);
        module.send(listCommand);
        String roomInfo = Protocol.encodeRoomsInfo(rooms);
        module.send(roomInfo);
      }
      changed.clear();
    }

    public synchronized void markChanged(Node module) {
      logger.fine(module+" has changed");
      changed.add(module);
      notifyAll();
    }
  }

}
