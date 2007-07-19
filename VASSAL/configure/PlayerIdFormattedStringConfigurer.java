/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.configure;

import VASSAL.build.module.GlobalOptions;

/** Utility subclass of {@link FormattedStringConfigurer} which includes variable
 * keys for player name, side, and id
 */
public class PlayerIdFormattedStringConfigurer extends FormattedStringConfigurer {
  public PlayerIdFormattedStringConfigurer(String key, String name, String[] options) {
    super(key, name);
    String[] allOptions = new String[options.length+3];
    allOptions[0] = GlobalOptions.PLAYER_NAME;
    allOptions[1] = GlobalOptions.PLAYER_SIDE;
    allOptions[2] = GlobalOptions.PLAYER_ID;
    System.arraycopy(options,0,allOptions,3,options.length);
    setOptions(allOptions);
  }
}
