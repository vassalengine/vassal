/*
 * Copyright (c) 2020 by the Vassal developers
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

import javax.swing.JPasswordField;
import javax.swing.JTextField;

public class PasswordConfigurer extends StringConfigurer {

  public PasswordConfigurer(String key, String name, String val) {
    super(key, name, val);
  }

  public PasswordConfigurer(String key, String name) {
    super(key, name);
  }

  @Override
  protected JTextField buildTextField() {
    return new JPasswordField(12);
  }
}
