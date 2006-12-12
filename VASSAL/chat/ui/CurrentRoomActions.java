package VASSAL.chat.ui;

import javax.swing.*;
import VASSAL.chat.SimplePlayer;

/*
 * $Id: CurrentRoomActions.java,v 1.2 2006-12-10 06:34:54 rkinney Exp $
 *
 * Copyright (c) 2004 by Rodney Kinney
 */

/** Interface for user interactions with players in the current room */
public interface CurrentRoomActions {
  JPopupMenu buildPopupForPlayer(SimplePlayer p, JTree tree);
}
