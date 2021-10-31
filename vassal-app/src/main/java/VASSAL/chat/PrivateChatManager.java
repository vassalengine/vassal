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

import VASSAL.build.GameModule;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;

import VASSAL.i18n.Resources;
import VASSAL.tools.menu.MenuManager;

/**
 * Manages {@link PrivateChatter} instances
 */
public class PrivateChatManager {
  private final ChatServerConnection client;

  private final List<Entry> chatters;
  private final List<Player> banned;

  public PrivateChatManager(ChatServerConnection client) {
    chatters = new ArrayList<>();
    banned = new ArrayList<>();
    this.client = client;
  }

  public PrivateChatter getChatterFor(final Player sender) {
    if (banned.contains(sender)) {
      return null;
    }
    PrivateChatter chat = null;
    final int index = chatters.indexOf(new Entry(sender, null));
    if (index >= 0) {
      chat = chatters.get(index).chatter;
    }
    if (chat == null) {
      chat = new PrivateChatter(sender, client);
      chatters.add(new Entry(sender, chat));

      final JFrame f = new JFrame();
      f.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
        }
      });

      final JButton closeButton  = new JButton(Resources.getString(Resources.CLOSE));
      closeButton.addActionListener(e -> {
        f.setVisible(false);
      });

      final JButton ignoreButton = new JButton(Resources.getString("Chat.ignore"));
      ignoreButton.addActionListener(e -> {
        promptToBan(sender);
        f.setVisible(false);
      });

      final Box box = Box.createHorizontalBox();
      box.add(ignoreButton, Box.LEFT_ALIGNMENT + Box.BOTTOM_ALIGNMENT);
      box.add(Box.createHorizontalGlue());
      box.add(closeButton, Box.RIGHT_ALIGNMENT + Box.BOTTOM_ALIGNMENT);

      f.setTitle(Resources.getString("Chat.private_channel", sender.getName())); //$NON-NLS-1$
      f.setJMenuBar(MenuManager.getInstance().getMenuBarFor(f));

      chat.add(box);

      f.add(chat);

      f.setSize(640, 320);
      f.setLocation(Toolkit.getDefaultToolkit().getScreenSize().width / 2 -
                    f.getSize().width / 2, 0);
    }
    return chat;
  }

  private void promptToBan(Player p) {
    if (JOptionPane.YES_OPTION ==
      JOptionPane.showConfirmDialog(GameModule.getGameModule().getPlayerWindow(),
        Resources.getString("Chat.ignore_messages", p.getName()), //$NON-NLS-1$
        null,
        JOptionPane.YES_NO_OPTION)) {
      banned.add(p);
    }
  }

  private static class Entry {
    private final Player player;
    private final PrivateChatter chatter;

    private Entry(Player p, PrivateChatter chat) {
      player = Objects.requireNonNull(p);
      chatter = chat;
    }

    @Override
    public int hashCode() {
      return player.hashCode();
    }

    @Override
    public boolean equals(Object o) {
      return o instanceof Entry && player.equals(((Entry) o).player);
    }
  }
}
