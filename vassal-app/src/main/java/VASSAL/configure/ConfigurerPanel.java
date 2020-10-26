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

import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * A standardised Panel to hold an individual Configurer
 */
public class ConfigurerPanel extends JPanel {
  private static final long serialVersionUID = 1L;

  /**
   * For older-style Configurer calls that supply a label (e.g. as used for Prefs),
   * keep a track of the internal label generated so that it can be hidden.
   */
  private JLabel label;

  /**
   * Create an alternate layout depending on whether or not a label is supplied for this configurer
   * New-style Configurers will always supply a blank label.
   * Legacy style Configurers will supply a text label and the column constraints must included a column for this. If a
   * label is supplied, then it will be added as a JLabel into the first column.
   * This option is supplied to provide support for custom coded Trait configurers.
   *
   * @param name The text of the supplied label
   * @param noNameColConstraints Column constraints to apply if the supplied name is null or empty
   * @param nameColConstraints Column constraints to apply if a label is supplied
   */
  public ConfigurerPanel(String name, String noNameColConstraints, String nameColConstraints) {
    this(name, noNameColConstraints, nameColConstraints, "");
  }

  public ConfigurerPanel(String name, String noNameColConstraints, String nameColConstraints, String rowConstraints) {
    super();
    setLayout(new ConfigurerLayout(name, noNameColConstraints, nameColConstraints, rowConstraints));
    if (name != null && ! name.isEmpty()) {
      label = new JLabel(name);
      add(label);
    }
  }

  /**
   * Show/hide the internal label if this Configurer has one.
   *
   * @param visible Hide label if true
   */
  public void setLabelVisibility(boolean visible) {
    if (label != null) {
      label.setVisible(visible);
    }
  }
}
