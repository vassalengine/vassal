package VASSAL.chat;

import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;

/**
 * Copyright (c) 2003 by Rodney Kinney.  All rights reserved.
 * Date: Jul 29, 2003
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
}
