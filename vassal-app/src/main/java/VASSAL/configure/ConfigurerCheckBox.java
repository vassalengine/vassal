/*
 *
 * Copyright (c) 2020 by Vassal Development Team
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

import java.awt.Insets;
import javax.swing.Icon;
import javax.swing.JCheckBox;

/**
 * Create a standardised field for use as an input field in Configurers
 *  - Remove the margin at the left side of the Checkbox so they align vertically
 */
public class ConfigurerCheckBox extends JCheckBox {

  public ConfigurerCheckBox() {
    super();
    adjustMargin();
  }

  public ConfigurerCheckBox(Icon icon) {
    super(icon);
    adjustMargin();
  }

  public ConfigurerCheckBox(String text) {
    super(text);
    adjustMargin();
  }

  private void adjustMargin() {
    final Insets insets = getMargin();
    insets.left = -2;
    setMargin(insets);
  }
}
