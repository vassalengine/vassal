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
package VASSAL.configure.password;

import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.Component;

import VASSAL.configure.Configurer;

public class ToggleablePasswordConfigurer extends Configurer {

  private final ToggleablePasswordFieldPanel panel;

  public ToggleablePasswordConfigurer(String key, String name) {
    this(key, name, null);
  }

  public ToggleablePasswordConfigurer(String key, String name, Object val) {
    super(key, name, val);
    panel = new ToggleablePasswordFieldPanel(name, (String) val);
    panel.addPasswordFieldListener(new DocumentListener() {
      @Override
      public void insertUpdate(DocumentEvent e) {
        update();
      }

      @Override
      public void removeUpdate(DocumentEvent e) {
        update();
      }

      @Override
      public void changedUpdate(DocumentEvent e) {}

      private void update() {
        noUpdate = true;
        setValue(panel.getPassword());
        noUpdate = false;
      }
    });
  }

  @Override
  public String getValueString() {
    return panel.getPassword();
  }

  @Override
  public void setValue(String s) {
    super.setValue((Object) s);
    if (!noUpdate && panel != null) {
      panel.setPassword(s);
    }
  }

  @Override
  public Component getControls() {
    return panel.getPanel();
  }
}
