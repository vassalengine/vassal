package VASSAL.chat.ui;

import javax.swing.JPopupMenu;
import javax.swing.JTree;
import VASSAL.chat.Room;

/*
 * $Id: ServerWindowActions.java,v 1.4 2006-12-09 22:49:24 rkinney Exp $
 *
 * Copyright (c) 2004 by Rodney Kinney
 *
 */

/** Interface for user interaction with {@link org.vassalengine.module.ServerWindow} */
public interface ServerWindowActions extends CurrentRoomActions {
  JPopupMenu buildPopupForRoom(Room r, JTree tree);
  void doubleClickRoom(Room r, JTree tree);
  void createRoom(String name);
}
