/*
 * $Id$
 * *
 * Copyright (c) 2000-2008 by Rodney Kinney
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
package VASSAL.build.module.properties;

import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.tools.SequenceEncoder;

/**
 * Encoder for {@link ChangePropertyCommand}s
 * 
 * @author rodneykinney
 * 
 */
public class ChangePropertyCommandEncoder implements CommandEncoder {
  protected static final String COMMAND_PREFIX = "MutableProperty\t";
  private MutablePropertiesContainer container;

  public ChangePropertyCommandEncoder(MutablePropertiesContainer container) {
    super();
    this.container = container;
  }

  public Command decode(String command) {
    Command c = null;
    if (command.startsWith(COMMAND_PREFIX)) {
      command = command.substring(COMMAND_PREFIX.length());
      SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
      String key = st.nextToken(null);
      String oldValue = st.nextToken(null);
      String newValue = st.nextToken(null);
      if (key != null) {
        MutableProperty p = container.getMutableProperty(key);
        if (p != null) {
          c = new ChangePropertyCommand(p, key, oldValue, newValue);
        }
      }
    }
    return c;
  }

  public String encode(Command c) {
    String s = null;
    if (c instanceof ChangePropertyCommand) {
      ChangePropertyCommand cpc = (ChangePropertyCommand) c;
      SequenceEncoder se = new SequenceEncoder('\t');
      se.append(cpc.getPropertyName()).append(cpc.getOldValue()).append(cpc.getNewValue());
      s = COMMAND_PREFIX+se.getValue();
    }
    return s;
  }
}
