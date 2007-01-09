package VASSAL.chat.node;

import VASSAL.chat.HttpRequestWrapper;
import VASSAL.chat.SimplePlayer;
import VASSAL.tools.PropertiesEncoder;

import java.io.IOException;
import java.util.Properties;
import java.util.Date;

/**
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Jun 7, 2003
 */
public class StatusReporter implements Runnable {
  private HttpRequestWrapper reportStatus;
  private String lastReportedContents;
  private String currentContents;
  private long sleepInterval = MIN_SLEEP;
  private static final long MIN_SLEEP = 2000;
  private static final long MAX_SLEEP = 1000 * 60 * 60 * 2;
  private AsynchronousServerNode server;

  public StatusReporter(HttpRequestWrapper reportStatus, AsynchronousServerNode server) {
    this.reportStatus = reportStatus;
    this.server = server;
    new Thread(this).start();
  }

  public void updateContents(Node[] players) {
    if (reportStatus == null) {
      return;
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < players.length; ++i) {
      Node mod = server.getModule(players[i]);
      try {
        String name = new PropertiesEncoder(players[i].getInfo()).getProperties().getProperty(NodePlayer.NAME);
        if (name != null) {
          buffer.append(mod.getId()).append('\t').append(players[i].getParent().getId()).append('\t').append(name).append('\n');
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    synchronized (this) {
      currentContents = buffer.toString();
    }
  }

  private synchronized void sendContents() {
    if (currentContents != null
        && !currentContents.equals(lastReportedContents)) {
      try {
        Properties props = new Properties();
        props.put("STATUS", currentContents);
        reportStatus.doPost("updateConnections", props);
        sleepInterval = MIN_SLEEP;
      }
      catch (IOException e) {
        sleepInterval = Math.min(2 * sleepInterval, MAX_SLEEP);
      }
      lastReportedContents = currentContents;
      System.err.println("----" + new Date());
      System.err.println(currentContents);
    }
  }

  public void run() {
    while (true) {
      try {
        Thread.sleep(sleepInterval);
        sendContents();
      }
      catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
  }
}
