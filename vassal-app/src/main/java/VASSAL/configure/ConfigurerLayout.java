/*
 *
 * Copyright (c) 2020 by VASSAl Development Team
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

import net.miginfocom.swing.MigLayout;

/**
 * A Standardised MigLayout for use by individual Configurers
 */
public class ConfigurerLayout extends MigLayout {
  private static final long serialVersionUID = 1L;

  public static final String STANDARD_INSETS = "ins 0"; // NON-NLS
  public static final String STANDARD_GAPY = "gapy 4"; // NON-NLS
  public static final String STANDARD_INSETS_GAPY = STANDARD_INSETS + "," + STANDARD_GAPY; // NON-NLS
  @Deprecated(since = "2023-10-16", forRemoval = true)
  public static final String STANDARD_INSERTS_GAPY = STANDARD_INSETS + "," + STANDARD_GAPY; // NON-NLS

  public static final String DEFAULT_CFG_LAYOUT_CONSTRAINTS = STANDARD_INSETS + ",hidemode 3"; // NON-NLS

  /**
   * Create a Layout with alternate Column constraints depending on whether or not a label is supplied for this configurer
   * New-style Configurers will always supply a blank label.
   * Legacy style Configurers will supply a text label and the column constraints must included a column for this.
   *
   * @param name The text of the supplied label
   * @param noNameColConstraints Column constraints to apply if the supplied name is null or empty
   * @param nameColConstraints Column constraints to apply if a label is supplied
   */
  public ConfigurerLayout(String name, String noNameColConstraints, String nameColConstraints) {
    this(DEFAULT_CFG_LAYOUT_CONSTRAINTS, (name == null || name.isEmpty()) ? noNameColConstraints : nameColConstraints);
  }

  public ConfigurerLayout(String name, String noNameColConstraints, String nameColConstraints, String rowConstraints) {
    this(name, noNameColConstraints, nameColConstraints);
    setRowConstraints(rowConstraints);
  }

  public ConfigurerLayout(String defaultLayoutConstraints, String defaultColumnConstraints) {
    super(defaultLayoutConstraints, defaultColumnConstraints);
  }

  public ConfigurerLayout() {
    super();
    setLayoutConstraints(DEFAULT_CFG_LAYOUT_CONSTRAINTS);
  }
}
