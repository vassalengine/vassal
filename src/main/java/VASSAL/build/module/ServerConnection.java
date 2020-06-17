package VASSAL.build.module;

import VASSAL.command.Command;

/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
/**
 * Represents the connection to a live server
 */
public interface ServerConnection {
  /** Name of the property fired when the connection is opened/clused. Value is Boolean.TRUE or Boolean.FALSE */
  public static final String CONNECTED = "Connected"; //$NON-NLS-1$

  /** Send a command to other players on the server */
  void sendToOthers(Command c);

  void setConnected(boolean connect);

  boolean isConnected();

  /**
   * Register a PropertyChangeListener. Changes to connection status triggers a PropertyChangeEvent, and concrete
   * implementations may define other properties
   */
  void addPropertyChangeListener(String propertyName, java.beans.PropertyChangeListener l);
}
