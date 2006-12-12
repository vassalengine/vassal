package VASSAL.chat.ui;

import javax.swing.JTextField;
import javax.swing.JToolBar;
import VASSAL.build.AbstractBuildable;

public abstract class ChatServerControls extends AbstractBuildable {

  public abstract JToolBar getToolbar();

  public abstract RoomTree getRoomTree();
  
  public abstract RoomTree getCurrentRoom();

  public abstract JTextField getNewRoom();
}
