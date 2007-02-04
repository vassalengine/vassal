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

import java.util.Enumeration;
import java.util.Vector;

/**
 * Forwards messages to a list of child MsgSenders
 * @deprecated
 */
public class MsgSenderGroup implements MsgSender {
  protected Vector senders = new Vector();
  protected String id;

  public MsgSenderGroup(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  public synchronized void send(String msg) {
    for (Enumeration e = senders.elements(); e.hasMoreElements();) {
      MsgSender s = (MsgSender) e.nextElement();
      s.send(msg);
    }
  }

  public synchronized MsgSender[] getChildren() {
    MsgSender[] s = new MsgSender[senders.size()];
    for (int i = 0; i < s.length; ++i) {
      s[i] = (MsgSender) senders.elementAt(i);
    }
    return s;
  }

  private MsgSenderGroup combineChildren(String id) {
    MsgSenderGroup allChildren = new MsgSenderGroup(id);
    for (Enumeration e = senders.elements(); e.hasMoreElements();) {
      MsgSender s = (MsgSender) e.nextElement();
      if (s instanceof MsgSenderGroup) {
        for (Enumeration e2 = ((MsgSenderGroup) s).senders.elements(); e2.hasMoreElements();) {
          allChildren.add((MsgSender) e2.nextElement());
        }
      }
      else {
        allChildren.add(s);
      }
    }
    return allChildren;
  }

  public synchronized void add(MsgSender s) {
    if (!senders.contains(s)) {
      senders.addElement(s);
    }
  }

  public synchronized void remove(MsgSender s) {
    senders.removeElement(s);
  }

  protected MsgSender build(String id) {
    return null;
  }
}
