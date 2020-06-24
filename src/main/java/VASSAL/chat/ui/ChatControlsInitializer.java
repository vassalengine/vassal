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


/**
 * Interface for registering event listeners with the Swing components in a ChatServerControls component
 * @author rkinney
 *
 */
public interface ChatControlsInitializer {
  /** Register all event listeners */
  void initializeControls(ChatServerControls controls);
  /** Remove all previously-registered event listeners */
  void uninitializeControls(ChatServerControls controls);
}
