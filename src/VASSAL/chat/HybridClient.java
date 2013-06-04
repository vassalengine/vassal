/*
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeListenerProxy;
import java.beans.PropertyChangeSupport;

import javax.swing.Icon;

import VASSAL.chat.ui.ChatControlsInitializer;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;

/**
 * Delegates calls to another SvrConnection instance, which can be changed programmatically
 *
 * @author rkinney
 *
 */
public class HybridClient implements ChatServerConnection, PlayerEncoder, ChatControlsInitializer {
  protected ChatServerConnection delegate;
  protected String defaultRoom = Resources.getString("Chat.main_room"); //$NON-NLS-1$
  protected PropertyChangeSupport propSupport = new PropertyChangeSupport(this);
  protected ChatServerControls controls;
  protected Icon currentIcon;
  protected String currentText;

  public HybridClient() {
    setDelegate(new DummyClient());
  }

  public void addPropertyChangeListener(String propertyName, PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(propertyName, l);
    if (delegate != null) {
      delegate.addPropertyChangeListener(propertyName, l);
    }
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
    if (delegate != null && delegate.isConnected()) {
      throw new IllegalStateException(Resources.getString("Server.error1")); //$NON-NLS-1$
    }
    ChatServerConnection oldDelegate = delegate;
    if (oldDelegate != null) {
      newDelegate.setUserInfo(oldDelegate.getUserInfo());
    }
    PropertyChangeListener[] listeners = propSupport.getPropertyChangeListeners();
    for (int i = 0; i < listeners.length; i++) {
      newDelegate.addPropertyChangeListener(((PropertyChangeListenerProxy) listeners[i]).getPropertyName(), listeners[i]);
    }
    if (controls != null) {
      if (delegate instanceof ChatControlsInitializer) {
        ((ChatControlsInitializer) delegate).uninitializeControls(controls);
      }
      if (newDelegate instanceof ChatControlsInitializer) {
        ((ChatControlsInitializer) newDelegate).initializeControls(controls);
      }
    }
    delegate = newDelegate;
   }

  public void initializeControls(ChatServerControls controls) {
    this.controls = controls;
    if (delegate instanceof ChatControlsInitializer) {
      ((ChatControlsInitializer) delegate).initializeControls(controls);
      controls.setRoomControlsVisible(true);
    }
    controls.updateClientDisplay(currentIcon, currentText);
  }

  public void uninitializeControls(ChatServerControls controls) {
    if (delegate instanceof ChatControlsInitializer) {
      ((ChatControlsInitializer) delegate).uninitializeControls(controls);
    }
  }

  public void updateDisplayControls(Icon icon, String text) {
    if (controls != null) {
      controls.updateClientDisplay(icon, text);
    }
    currentIcon = icon;
    currentText = text;
  }
}
