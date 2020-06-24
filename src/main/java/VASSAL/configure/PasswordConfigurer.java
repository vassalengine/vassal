/*
 * Copyright (c) 2006 Amazon.com, Inc.
 * All rights reserved.
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
