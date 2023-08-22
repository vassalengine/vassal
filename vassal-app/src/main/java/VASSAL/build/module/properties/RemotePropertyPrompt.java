/*
 *
 * Copyright (c) 2006 by Rodney Kinney
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

import VASSAL.counters.DynamicProperty;
import VASSAL.script.expression.Auditable;

/**
 * Change for updating a remote DP by a prompted entry.
 * Only change from standard is that since Set Piece Property affects mulitiple pieces, we
 * can't define an Old Value.
 *
 */
public class RemotePropertyPrompt extends PropertyPrompt implements RemotePropertyChanger {

  public RemotePropertyPrompt(PropertyPrompt prompt) {
    super(prompt.constraints, prompt.promptText);
  }

  @Override
  public String getNewValue(DynamicProperty target, Auditable owner, PropertySource ps) {
    return super.getNewValue("");
  }
}