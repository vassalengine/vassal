/*
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
package VASSAL.chat.ui;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JTree;

import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.LockableChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.SimplePlayer;
import VASSAL.chat.SoundEncoder;
import VASSAL.configure.SoundConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;

/**
 * Send a wake-up sound to another player
 * - Can't wake-up oneself
 * - No wake-ups in the default room
 * - No wake-ups to people in different rooms
 * - No wake-up to the same person in the same room until at least 5 seconds has passed.
 */
public class SendSoundAction extends AbstractAction {
  private static final long serialVersionUID = 1L;
  private static Room lastRoom;
  private static Player lastPlayer;
  private static long lastSound = System.currentTimeMillis();

  private ChatServerConnection client;
  private Player target;
  private String soundKey;

  public SendSoundAction(String name, ChatServerConnection client, String soundKey, Player target) {
    super(name);
    this.client = client;
    this.soundKey = soundKey;
    this.target = target;

    // Find which room our target player is in
    Room targetRoom = null;
    for (Room room : client.getAvailableRooms()) {
      if (room.getPlayerList().contains(target)) {
        targetRoom = room;
      }
    }

    setEnabled(
      target != null
      && GameModule.getGameModule() != null
      && !target.equals(client.getUserInfo())
      && client.getRoom() != null
      && client.getRoom().equals(targetRoom)
      && (!targetRoom.equals(lastRoom) || !target.equals(lastPlayer) ||
      (System.currentTimeMillis() - lastSound) > SoundEncoder.Cmd.TOO_SOON));
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    client.sendTo(target, new SoundEncoder.Cmd(soundKey, client.getUserInfo()));
    lastPlayer = target;
    lastRoom = client.getRoom();
    lastSound = System.currentTimeMillis();
  }

  public static PlayerActionFactory factory(final ChatServerConnection client, final String name, final String soundKey, final String defaultSoundFile) {
    if (GameModule.getGameModule() != null) {
      Prefs.getGlobalPrefs().addOption(Resources.getString("Prefs.sounds_tab"), new SoundConfigurer(soundKey, name, defaultSoundFile)); //$NON-NLS-1$
    }
    return new PlayerActionFactory() {
      @Override
      public Action getAction(SimplePlayer p, JTree tree) {
        final Room r = client.getRoom();
        if (client instanceof LockableChatServerConnection && ((LockableChatServerConnection) client).isDefaultRoom(r)) {
          return null;
        }
        return new SendSoundAction(name, client, soundKey, p);
      }
    };
  }
}
