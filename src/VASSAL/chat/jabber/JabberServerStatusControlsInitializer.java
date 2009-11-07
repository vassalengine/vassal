/*
 * $Id: JabberServerStatusControlsInitializer 6210 2009-11-01 11:16:06Z swampwallaby $
 *  
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

import VASSAL.chat.ServerStatus;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.chat.ui.ServerStatusControlsInitializer;
import VASSAL.chat.ui.ShowServerStatusAction;

public class JabberServerStatusControlsInitializer extends
    ServerStatusControlsInitializer {

  public JabberServerStatusControlsInitializer(ServerStatus status) {
    super(status);
  }
  
  public void initializeControls(ChatServerControls controls) {
    showStatusButton = controls.getToolbar().add(new ShowServerStatusAction(status, getClass().getResource("/images/status.gif"), false)); //$NON-NLS-1$
  }

}
