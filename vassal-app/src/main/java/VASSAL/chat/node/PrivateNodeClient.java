/*
 * Copyright (c) 2000-2020 by Rodney Kinney, Brent Easton, Joel Uckelman
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
package VASSAL.chat.node;

import VASSAL.chat.DummyMessageServer;
import VASSAL.command.CommandEncoder;

public class PrivateNodeClient extends NodeClient {
  public PrivateNodeClient(String moduleName, String playerId, CommandEncoder encoder, String host, int port) {
    super(moduleName, playerId, encoder, host, port, new DummyMessageServer());
  }
}
