/*
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
package VASSAL.chat.node;

import java.io.IOException;
import java.util.Date;
import java.util.Properties;

import VASSAL.chat.HttpRequestWrapper;
import VASSAL.chat.SimpleStatus;
import VASSAL.tools.PropertiesEncoder;

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
    new Thread(this, "status reporter").start();
  }

  public void updateContents(Node[] players) {
    if (reportStatus == null) {
      return;
    }
    final StringBuilder buffer = new StringBuilder();
    for (Node pl : players) {
      Node mod = server.getModule(pl);
      try {
        final String name = new PropertiesEncoder(pl.getInfo()).getProperties().getProperty(SimpleStatus.NAME);
        if (name != null) {
          buffer.append(mod.getId())
                .append('\t')
                .append(pl.getParent().getId())
                .append('\t')
                .append(name)
                .append('\n');
        }
      }
      // FIXME: review error message
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
        props.put("STATUS", currentContents); //$NON-NLS-1$
        reportStatus.doPost("updateConnections", props); //$NON-NLS-1$
        sleepInterval = MIN_SLEEP;
      }
      // FIXME: review error message
      catch (IOException e) {
        sleepInterval = Math.min(2 * sleepInterval, MAX_SLEEP);
      }
      lastReportedContents = currentContents;
      System.err.println("----" + new Date()); //$NON-NLS-1$
      System.err.println(currentContents);
    }
  }

  public void run() {
    while (true) {
      try {
        Thread.sleep(sleepInterval);
        sendContents();
      }
      // FIXME: review error message
      catch (InterruptedException e) {
        e.printStackTrace();
      }
    }
  }
}
