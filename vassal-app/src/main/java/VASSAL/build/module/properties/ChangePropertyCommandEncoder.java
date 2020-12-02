/*
 * *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
  protected static final String COMMAND_PREFIX = "MutableProperty\t"; //NON-NLS

  private final MutablePropertiesContainer container;

  public ChangePropertyCommandEncoder(MutablePropertiesContainer container) {
    super();
    this.container = container;
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(COMMAND_PREFIX)) {
      return null;
    }

    command = command.substring(COMMAND_PREFIX.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, '\t');
    final String key = st.nextToken(null);
    if (key == null) {
      return null;
    }

    final String oldValue = st.nextToken(null);
    final String newValue = st.nextToken(null);
    final String containerId = st.nextToken("");

    /*
     * NB. If there is no containerID in the command, then it is a pre-bug fix command. Legacy
     * behaviour is to execute the change on the first matching property found in any container
     */
    if (containerId.length() != 0 && !containerId.equals(container.getMutablePropertiesContainerId())) {
      return null;
    }

    final MutableProperty p = container.getMutableProperty(key);
    if (p == null) {
      return null;
    }

    return new ChangePropertyCommand(p, key, oldValue, newValue);
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof ChangePropertyCommand)) {
      return null;
    }

    final ChangePropertyCommand cpc = (ChangePropertyCommand) c;
    final String our_cid = container.getMutablePropertiesContainerId();
    final String their_cid = cpc.getProperty().getParent().getMutablePropertiesContainerId();
    if (!our_cid.equals(their_cid)) {
      return null;
    }

    final SequenceEncoder se = new SequenceEncoder('\t');
    se.append(cpc.getPropertyName())
      .append(cpc.getOldValue())
      .append(cpc.getNewValue())
      .append(our_cid);
    return COMMAND_PREFIX + se.getValue();
  }
}
