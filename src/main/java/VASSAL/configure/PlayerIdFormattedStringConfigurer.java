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
import VASSAL.tools.ArrayUtils;

/** Utility subclass of {@link FormattedStringConfigurer} which includes variable
 * keys for player name, side, and id
 */
public class PlayerIdFormattedStringConfigurer extends FormattedStringConfigurer {
  public PlayerIdFormattedStringConfigurer(String key, String name, String[] options) {
    super(key, name);

    final String[] allOptions = ArrayUtils.append(
      new String[]{
        GlobalOptions.PLAYER_NAME,
        GlobalOptions.PLAYER_SIDE,
        GlobalOptions.PLAYER_ID
      },
      options
    );

    setOptions(allOptions);
  }
}
