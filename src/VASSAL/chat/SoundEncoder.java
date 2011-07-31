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
package VASSAL.chat;

import java.util.ArrayList;

import javax.swing.JOptionPane;

import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.SoundConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.SequenceEncoder;

/**
 * Encodes commands that play sounds
 * This class is used exclusively by the 'Send wake-up' server feature.
 * Limit the number of wake-ups we will respond to in a row from the same player
 * before querying if we want to ignore them in future.
 * Wait at least 5 seconds before responding to a new wake-up.
 */
public class SoundEncoder implements CommandEncoder {
  public static final String COMMAND_PREFIX = "PLAY\t"; //$NON-NLS-1$
  private PlayerEncoder playerEncoder;

  public SoundEncoder(PlayerEncoder p) {
    playerEncoder = p;
  }

  public Command decode(String command) {
    if (command.startsWith(COMMAND_PREFIX)) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(command, '\t');
      sd.nextToken();
      final String soundKey = sd.nextToken();
      final Player sender = playerEncoder.stringToPlayer(sd.nextToken("")); //$NON-NLS-1$
      return new Cmd(soundKey, sender);
    }
    else {
      return null;
    }
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof Cmd) {
      Cmd cmd = (Cmd) c;
      SequenceEncoder se = new SequenceEncoder('\t');
      se.append(cmd.soundKey);
      se.append(playerEncoder.playerToString(cmd.getSender()));
      s = COMMAND_PREFIX+se.getValue();
    }
    return s;
  }

  public static class Cmd extends Command {
    private static final int TOO_MANY = 4;
    public static final int TOO_SOON = 10 * 1000; // 10s
    private static long lastTime = System.currentTimeMillis();
    private static Player lastSender;
    private static int sendCount;
    private static ArrayList<Player> banned = new ArrayList<Player>();
    private static boolean updating = false;

    private String soundKey;
    private Player sender;

    public Cmd(String soundKey, Player player) {
      this.soundKey = soundKey;
      this.sender = player;
    }

    protected void executeCommand() {
      /**
       * Ignore if we don't want to hear from this player anymore, we are
       * already processing a wake-up, or we have already processed a wake-up recently
       */
      final long now = System.currentTimeMillis();
      if (banned.contains(sender) || updating || (now - lastTime) < (TOO_SOON)) {
        updating = false;
        return;
      }

      updating = true;
      lastTime = now;
      final SoundConfigurer c = (SoundConfigurer) Prefs.getGlobalPrefs().getOption(soundKey);
      if (c != null) {
        c.play();
      }
      if (sender.equals(lastSender)) {
        if (sendCount++ >= TOO_MANY) {
          if (JOptionPane.YES_OPTION ==
            JOptionPane.showConfirmDialog
            (null,
             Resources.getString("Chat.ignore_wakeups", sender.getName()), //$NON-NLS-1$
             null,
             JOptionPane.YES_NO_OPTION)) {
            banned.add(sender);
          }
          else {
            sendCount = 1;
          }
        }
      }
      else {
        lastSender = sender;
        sendCount = 1;
      }
      updating = false;
    }

    protected Command myUndoCommand() {
      return null;
    }

    public Player getSender() {
      return sender;
    }
  }
}
