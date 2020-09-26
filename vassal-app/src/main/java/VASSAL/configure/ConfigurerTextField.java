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
import javax.swing.JTextField;
import javax.swing.text.Document;

/**
 * Create a standardised field for use as an input field in Configurers
 *  - Set insets so there is a small gap between left edge of configurer and first text
 */
public class ConfigurerTextField extends JTextField {


  public ConfigurerTextField() {
    adjustMargin();
  }

  public ConfigurerTextField(String text) {
    super(text);
    adjustMargin();
  }

  public ConfigurerTextField(int columns) {
    super(columns);
    adjustMargin();
  }

  public ConfigurerTextField(String text, int columns) {
    super(text, columns);
    adjustMargin();
  }

  public ConfigurerTextField(Document doc, String text, int columns) {
    super(doc, text, columns);
    adjustMargin();
  }

  private void adjustMargin() {
    final Insets insets = getMargin();
    insets.left = 1;
    insets.top = 2;
    insets.right = 1;
    insets.bottom = 2;
    setMargin(insets);
  }
}
