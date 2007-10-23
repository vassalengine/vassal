/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.chat.jabber;

import java.awt.Component;
import java.awt.Container;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.text.JTextComponent;

import org.jivesoftware.smack.util.StringUtils;

import VASSAL.build.GameModule;
import VASSAL.build.module.ServerConnection;
import VASSAL.configure.Configurer;

public class ModuleAccountInfo implements AccountInfo {
  private Configurer realNameConfig;
  private Configurer pwdConfig;

  public ModuleAccountInfo() {
    realNameConfig = GameModule.getGameModule().getPrefs().getOption(GameModule.REAL_NAME);
    pwdConfig = GameModule.getGameModule().getPrefs().getOption(GameModule.SECRET_NAME);
  }

  public String getPassword() {
    return pwdConfig.getValueString();
  }

  public String getUserName() {
    return StringUtils.escapeNode(realNameConfig.getValueString()).toLowerCase();
  }

  public String getRealName() {
    return realNameConfig.getValueString();
  }

  public void init(JabberClient client) {
    client.addPropertyChangeListener(ServerConnection.CONNECTED, new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        setControlsEnabled(!Boolean.TRUE.equals(evt.getNewValue()));
      }
    });
  }

  private void setControlsEnabled(boolean enabled) {
    setEditable(realNameConfig.getControls(), enabled);
    setEditable(pwdConfig.getControls(), enabled);
  }

  private void setEditable(Component c, boolean enabled) {
    if (c instanceof JTextComponent) {
      ((JTextComponent) c).setEnabled(enabled);
    }
    else if (c instanceof Container) {
      for (int i = 0, n = ((Container) c).getComponentCount(); i < n; i++) {
        setEditable(((Container) c).getComponent(i), enabled);
      }
    }
  }

  public String getModule() {
    return GameModule.getGameModule().getGameName();
  }
}
