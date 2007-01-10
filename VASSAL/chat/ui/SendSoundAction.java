package VASSAL.chat.ui;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JTree;
import VASSAL.build.GameModule;
import VASSAL.chat.ChatServerConnection;
import VASSAL.chat.Player;
import VASSAL.chat.SoundEncoder;
import VASSAL.configure.SoundConfigurer;

/**
 * Copyright (c) 2003 by Rodney Kinney. All rights reserved. Date: Jul 29, 2003
 */
public class SendSoundAction extends AbstractAction {
  private ChatServerConnection client;
  private Player target;
  private String soundKey;

  public SendSoundAction(String name, ChatServerConnection client, String soundKey, Player target) {
    super(name);
    this.client = client;
    this.soundKey = soundKey;
    this.target = target;
  }

  public void actionPerformed(ActionEvent e) {
    client.sendTo(target, new SoundEncoder.Cmd(soundKey));
  }

  public static PlayerActionFactory factory(final ChatServerConnection client, final String name, final String soundKey, final String defaultSoundFile) {
    if (GameModule.getGameModule() != null) {
      GameModule.getGameModule().getGlobalPrefs().addOption("Sounds", new SoundConfigurer(soundKey, name, defaultSoundFile));
    }
    return new PlayerActionFactory() {
      public Action getAction(Player p, JTree tree) {
        return new SendSoundAction(name, client, soundKey, p);
      }
    };
  }
}
