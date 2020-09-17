/*
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
package VASSAL.command;

import VASSAL.build.module.GameComponent;

/**
 * Translates a {@link Command} to and from a string.
 *
 * A {@link Command} represents an action that needs to be transmitted from one client to another -- any action that
 * could change the game state in a multiplayer game needs to be encapsulated into a Command so that other players'
 * clients will reflect it. The CommandEncoders serialise ({@link #encode}) and deserialise ({@link #decode}) those
 * Java classes to and from an ascii based representation. Commands are encoded by the generated client prior to being
 * sent across network or saved in a log or save file. Commands are decoded by the receiving client on receipt from the
 * network or on reading from a log or save file. Save game creation is a special case where every {@link GameComponent} is
 * asked to generate a Command that when executed will cause itself to be recreated.
 *
 * The {@link Command#execute()} method implements the execution of the command and is called by the receiving client
 * after building the Command using <code>decode</code>. The execute() method is sometimes called on the generating client but does
 * not need to be if the Command is being created to encapsulate something that has already happened on the generating client.
 *
 * Although Commands can be linked into compound commands, each CommandEncoder need only handle single (not compound) commands.
 */
public interface CommandEncoder  {

  /** Translate a String into a {@link Command} */
  Command decode(String command);
  /** Translate a {@link Command} into a String */
  String encode(Command c);
}
