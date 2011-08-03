/*
 * $Id$
 *
 * Copyright (c) 2009 by Joel Uckelman
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

package VASSAL.launch;

import java.io.Serializable;

import VASSAL.tools.ipc.IPCMessage;

/**
 * The interface for objects passed by {@link CommandClient} to
 * {@link CommandServer}.
 * @deprecated Use {@link IPCMessage} instead.
 */
@Deprecated
public interface Command extends Serializable {
  /**
   * Execute the command.
   *
   * @return the result
   */
  Object execute();
}
