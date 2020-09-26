/*
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
package VASSAL.chat.node;

import VASSAL.chat.ChatServerFactory;

/**
 * @author rkinney
 */
public abstract class NodeClientFactory extends ChatServerFactory {
  public static final String NODE_TYPE = "node";  //$NON-NLS-1$
  public static final String NODE_HOST = "nodeHost";  //$NON-NLS-1$
  public static final String NODE_PORT = "nodePort";  //$NON-NLS-1$
}
