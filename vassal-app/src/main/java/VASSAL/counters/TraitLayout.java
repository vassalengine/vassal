/*
 *
 * Copyright (c) 2020 by VASSAL Development Team
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

package VASSAL.counters;

import VASSAL.configure.ConfigurerLayout;

/**
 * A standardised MigLayout for use by Trait configurers
 */
public class TraitLayout extends ConfigurerLayout {

  public static final String DEFAULT_TRAIT_LAYOUT_CONSTRAINTS = "ins " + STANDARD_INSETS + ",gapy " + STANDARD_GAPY + ",hidemode 3,wrap 2"; // NON-NLS
  public static final String DEFAULT_TRAIT_COLUMN_CONSTRAINTS = "[right]rel[fill,grow]"; // NON-NLS

  public TraitLayout() {
    this(false);

  }
  public TraitLayout(boolean debug) {
    this(debug, DEFAULT_TRAIT_LAYOUT_CONSTRAINTS, DEFAULT_TRAIT_COLUMN_CONSTRAINTS);
  }

  public TraitLayout(boolean debug, String layourConstraints, String columnConstraimts) {
    super((debug ? "debug," : "") + DEFAULT_TRAIT_LAYOUT_CONSTRAINTS, DEFAULT_TRAIT_COLUMN_CONSTRAINTS);
  }
}
