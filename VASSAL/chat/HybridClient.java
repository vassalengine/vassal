/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Sep 16, 2002
 * Time: 11:07:55 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.chat;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeListenerProxy;
import java.beans.PropertyChangeSupport;
import VASSAL.command.Command;

/**
 * Delegates calls to another SvrConnection instance, which can be changed programmatically
 * @author rkinney
 *
 */
public class HybridClient implements ChatServerConnection, PlayerEncoder {
  protected ChatServerConnection delegate;
  protected String defaultRoom = "Main Room";
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected PropertyChangeListener propForwarder;
  
  public HybridClient() {
    propForwarder = new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        propSupport.firePropertyChange(evt);
      }
    };
    this.delegate = new DummyClient();
  }

  public void addPropertyChangeListener(String propertyName, PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(propertyName,l);
  }
  
  public Room[] getAvailableRooms() {
    return delegate.getAvailableRooms();
  }

  public ServerStatus getStatusServer() {
    return delegate.getStatusServer();
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

  public void setDelegate(ChatServerConnection svr) {
    if (delegate.isConnected()) {
      throw new IllegalStateException("Cannot change server implementation while connected");
    }
    ChatServerConnection oldDelegate = delegate;
    svr.setUserInfo(oldDelegate.getUserInfo());
    ServerStatus oldStatus = getStatusServer();
    PropertyChangeListener[] listeners = propSupport.getPropertyChangeListeners();
    for (int i = 0; i < listeners.length; i++) {
      svr.addPropertyChangeListener(((PropertyChangeListenerProxy)listeners[i]).getPropertyName(), listeners[i]);
    }
    delegate = svr;
    propSupport.firePropertyChange(new PropertyChangeEvent(this, STATUS_SERVER, oldStatus, getStatusServer()));
  }
}
