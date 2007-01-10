/*
 * $Id: HttpMessageServer.java,v 1.3 2007-01-03 04:42:24 rkinney Exp $
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.chat;

import VASSAL.chat.messageboard.Message;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.peer2peer.PeerPoolInfo;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.build.GameModule;
import VASSAL.tools.SequenceEncoder;

import java.util.*;
import java.io.IOException;


public class HttpMessageServer implements MessageBoard, WelcomeMessageServer {
  private HttpRequestWrapper welcomeURL;
  private HttpRequestWrapper getMessagesURL;
  private HttpRequestWrapper postMessageURL;
  private PeerPoolInfo info;

  public HttpMessageServer(String getMessagesURL, String postMessageURL, String welcomeURL, PeerPoolInfo info) {
    this.getMessagesURL = new HttpRequestWrapper(getMessagesURL);
    this.welcomeURL = new HttpRequestWrapper(welcomeURL);
    this.postMessageURL = new HttpRequestWrapper(postMessageURL);
    this.info = info;
  }

  public Command getWelcomeMessage() {
    Command motd = new NullCommand();
    try {
      if (GameModule.getGameModule() != null) {
        for (Enumeration e = welcomeURL.doGet(prepareInfo());
             e.hasMoreElements();) {
          motd = motd.append(GameModule.getGameModule().decode((String) e.nextElement()));
        }
      }
    }
    catch (IOException e) {
    }
    return motd;
  }

  public Message[] getMessages() {
    List msgList = new ArrayList();
    try {
      for (Enumeration e = getMessagesURL.doGet(prepareInfo());
           e.hasMoreElements();) {
        String msg = (String) e.nextElement();
        StringTokenizer st = new StringTokenizer(msg, "&");
        String s = st.nextToken();
        String sender = s.substring(s.indexOf("=") + 1);
        String date = st.nextToken();
        date = date.substring(date.indexOf("=") + 1);
        s = st.nextToken("");
        SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(s.substring(s.indexOf("=") + 1), '|');
        String content = "";
        while (st2.hasMoreTokens()) {
          content += st2.nextToken();
          if (st2.hasMoreTokens()) {
            content += "\n";
          }
        }
        content = restorePercent(content);
        Date created = null;
        try {
          long time = Long.parseLong(date);
          TimeZone t = TimeZone.getDefault();
          time += t.getOffset(Calendar.ERA, Calendar.YEAR, Calendar.MONTH, Calendar.DAY_OF_YEAR, Calendar.DAY_OF_WEEK, Calendar.MILLISECOND);
          created = new Date(time);
        }
        catch (NumberFormatException e1) {
          created = new Date();
        }
        msgList.add(new Message(sender, content, created));
      }
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }
    Message[] msgs = (Message[]) msgList.toArray(new Message[msgList.size()]);
    return msgs;
  }

  private Properties prepareInfo() {
    Properties p = new Properties();
    p.put("module", info.getModuleName());
    return p;
  }

  private String removePercent(String input) {
    StringBuffer buff = new StringBuffer();
    StringTokenizer st = new StringTokenizer(input, "%#", true);
    while (st.hasMoreTokens()) {
      String s = st.nextToken();
      switch (s.charAt(0)) {
        case '%':
          buff.append("/#/");
          break;
        case '#':
          buff.append("/##/");
          break;
        default:
          buff.append(s);
      }
    }
    return buff.toString();
  }

  private String restorePercent(String input) {
    for (int i = input.indexOf("/#/");
         i >= 0; i = input.indexOf("/#/")) {
      input = input.substring(0, i) + "%" + input.substring(i + 3);
    }
    for (int i = input.indexOf("/##/");
         i >= 0; i = input.indexOf("/##/")) {
      input = input.substring(0, i) + "#" + input.substring(i + 4);
    }
    return input;
  }

  public void postMessage(String content) {
    if (content == null
      || content.length() == 0) {
      return;
    }
    content = removePercent(content);
    SequenceEncoder se = new SequenceEncoder('|');
    StringTokenizer st = new StringTokenizer(content, "\n\r");
    while (st.hasMoreTokens()) {
      se.append(st.nextToken());
    }

    Properties p = prepareInfo();
    p.put("sender", info.getUserName());
    p.put("content", se.getValue());
    try {
      postMessageURL.doPost(p);
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }
  }

}
