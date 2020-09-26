/*
 * Copyright (c) 2000-2020 by Rodney Kinney, Brent Easton, Joel Uckelman
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

import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.WelcomeMessageServer;
import VASSAL.chat.messageboard.MessageBoard;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.chat.ui.MessageBoardControlsInitializer;
import VASSAL.chat.ui.ServerStatusControlsInitializer;
import VASSAL.command.CommandEncoder;
import VASSAL.i18n.Resources;

public class OfficialNodeClient extends NodeClient {
  private final MessageBoardControlsInitializer messageBoardControls;
  private final ServerStatusControlsInitializer serverStatusControls;

  public OfficialNodeClient(String moduleName, String playerId, CommandEncoder encoder, String host, int port, MessageBoard msgSvr, WelcomeMessageServer welcomer) {
    super(moduleName, playerId, encoder, host, port, welcomer);

    messageBoardControls = new MessageBoardControlsInitializer(
      Resources.getString("Chat.messages"), msgSvr //$NON-NLS-1$
    );
    serverStatusControls = new ServerStatusControlsInitializer(new CgiServerStatus());
  }

  @Override
  public void initializeControls(ChatServerControls controls) {
    super.initializeControls(controls);
    messageBoardControls.initializeControls(controls);
    serverStatusControls.initializeControls(controls);
  }

  @Override
  public void uninitializeControls(ChatServerControls controls) {
    super.uninitializeControls(controls);
    messageBoardControls.uninitializeControls(controls);
    serverStatusControls.uninitializeControls(controls);
  }
}
