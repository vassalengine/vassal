package VASSAL.chat;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeListenerProxy;
import java.beans.PropertyChangeSupport;
import VASSAL.chat.ui.ChatControlsInitializer;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.command.Command;

/**
 * Delegates calls to another SvrConnection instance, which can be changed programmatically
 * 
 * @author rkinney
 * 
 */
public class HybridClient implements ChatServerConnection, PlayerEncoder, ChatControlsInitializer {
  protected ChatServerConnection delegate;
  protected String defaultRoom = "Main Room";
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected PropertyChangeListener propForwarder;
  protected ChatServerControls controls;

  public HybridClient() {
    propForwarder = new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        propSupport.firePropertyChange(evt);
      }
    };
    this.delegate = new DummyClient();
  }

  public void addPropertyChangeListener(String propertyName, PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(propertyName, l);
  }

  public Room[] getAvailableRooms() {
    return delegate.getAvailableRooms();
  }

  public Room getRoom() {
    return delegate.getRoom();
  }

  public Player getUserInfo() {
    return delegate.getUserInfo();
  }

  public boolean isConnected() {
    return delegate.isConnected();
  }

  public void sendTo(Player recipient, Command c) {
    delegate.sendTo(recipient, c);
  }

  public void sendToOthers(Command c) {
    delegate.sendToOthers(c);
  }

  public void setConnected(boolean connect) {
    delegate.setConnected(connect);
  }

  public ChatServerConnection getDelegate() {
    return delegate;
  }

  public void setRoom(Room r) {
    delegate.setRoom(r);
  }

  public void setUserInfo(Player p) {
    delegate.setUserInfo(p);
  }

  public Player stringToPlayer(String s) {
    if (delegate instanceof PlayerEncoder) {
      return ((PlayerEncoder) delegate).stringToPlayer(s);
    }
    return null;
  }

  public String playerToString(Player p) {
    if (delegate instanceof PlayerEncoder) {
      return ((PlayerEncoder) delegate).playerToString(p);
    }
    return null;
  }

  protected void fireStatus(String msg) {
    propSupport.firePropertyChange(new PropertyChangeEvent(this, STATUS, null, msg));
  }

  public void setDelegate(ChatServerConnection newDelegate) {
    if (delegate.isConnected()) {
      throw new IllegalStateException("Cannot change server implementation while connected");
    }
    ChatServerConnection oldDelegate = delegate;
    newDelegate.setUserInfo(oldDelegate.getUserInfo());
    PropertyChangeListener[] listeners = propSupport.getPropertyChangeListeners();
    for (int i = 0; i < listeners.length; i++) {
      newDelegate.addPropertyChangeListener(((PropertyChangeListenerProxy) listeners[i]).getPropertyName(), listeners[i]);
    }
    if (delegate instanceof ChatControlsInitializer) {
      ((ChatControlsInitializer) delegate).uninitializeControls(controls);
    }
    if (newDelegate instanceof ChatControlsInitializer) {
      ((ChatControlsInitializer) newDelegate).initializeControls(controls);
    }
    delegate = newDelegate;
  }

  public void initializeControls(ChatServerControls controls) {
    this.controls = controls;
  }

  public void uninitializeControls(ChatServerControls controls) {
  }
}
