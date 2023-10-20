/*
 *
 * Copyright (c) 2000-2006 by Rodney Kinney
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

/**
 * Remove sub-commands from a composite command
 * @author rkinney
 */
public abstract class CommandFilter {
  /**
   * Apply the filter
   * @return the filtered command
   */
  public Command apply(Command c) {
    final Command comm = accept(c) ? c : new NullCommand();
    if (c != null) {
      final Command[] sub = c.getSubCommands();
      comm.stripSubCommands();
      for (final Command command : sub) {
        comm.append(apply(command));
      }
    }
    return comm;
  }

  protected abstract boolean accept(Command c);

}
