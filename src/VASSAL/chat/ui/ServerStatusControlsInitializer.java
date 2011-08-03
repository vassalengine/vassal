/*
 *
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
package VASSAL.chat.ui;

import javax.swing.JButton;

import VASSAL.chat.ServerStatus;

public class ServerStatusControlsInitializer implements ChatControlsInitializer {
  protected JButton showStatusButton;
  protected ServerStatus status;

  public ServerStatusControlsInitializer(ServerStatus status) {
    super();
    this.status = status;
  }

  public void initializeControls(ChatServerControls controls) {
    showStatusButton = controls.getToolbar().add(new ShowServerStatusAction(status, getClass().getResource("/images/status.gif"))); //$NON-NLS-1$
  }

  public void uninitializeControls(ChatServerControls controls) {
    controls.getToolbar().remove(showStatusButton);
    controls.getToolbar().repaint();
  }
}
