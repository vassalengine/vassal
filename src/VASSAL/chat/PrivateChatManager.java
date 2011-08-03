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
package VASSAL.chat;

import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JOptionPane;

import VASSAL.i18n.Resources;
import VASSAL.tools.menu.MenuManager;

/**
 * Manages {@link PrivateChatter} instances
 */
public class PrivateChatManager {
  private ChatServerConnection client;

  private List<Entry> chatters;
  private List<Player> banned;

  public PrivateChatManager(ChatServerConnection client) {
    chatters = new ArrayList<Entry>();
    banned = new ArrayList<Player>();
    this.client = client;
  }

  public PrivateChatter getChatterFor(final Player sender) {
    if (banned.contains(sender)) {
      return null;
    }
    PrivateChatter chat = null;
    int index = chatters.indexOf(new Entry(sender, null));
    if (index >= 0) {
      chat = chatters.get(index).chatter;
    }
    if (chat == null) {
      chat = new PrivateChatter(sender, client);
      chatters.add(new Entry(sender, chat));

      final JFrame f = new JFrame();
      f.addWindowListener(new WindowAdapter() {
        public void windowClosing(WindowEvent e) {
          promptToBan(sender);
        }
      });

      f.setTitle(Resources.getString("Chat.private_channel", sender.getName())); //$NON-NLS-1$
      f.setJMenuBar(MenuManager.getInstance().getMenuBarFor(f));
      f.getContentPane().add(chat);
      f.pack();
      f.setLocation(Toolkit.getDefaultToolkit().getScreenSize().width / 2 -
                    f.getSize().width / 2, 0);
    }
    return chat;
  }

  private void promptToBan(Player p) {
    if (JOptionPane.YES_OPTION ==
      JOptionPane.showConfirmDialog
      (null,
       Resources.getString("Chat.ignore_messages", p.getName()), //$NON-NLS-1$
       null,
       JOptionPane.YES_NO_OPTION)) {
      banned.add(p);
    }
  }

  private static class Entry {
    private Player player;
    private PrivateChatter chatter;

    private Entry(Player p, PrivateChatter chat) {
      if (p == null) {
        throw new NullPointerException();
      }
      player = p;
      chatter = chat;
    }

    public boolean equals(Object o) {
      if (o instanceof Entry) {
        return player.equals(((Entry) o).player);
      }
      else {
        return false;
      }
    }
  }
}
