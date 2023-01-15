/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.event.FocusListener;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

/**
 * Configurer for Boolean values
 */
public class BooleanConfigurer extends Configurer {
  protected JPanel p;
  private JCheckBox box;

  /**
   * Create an old-style configurer with inbuilt label
   *
   * @param key Configurer key
   * @param name Label text
   * @param val Initial value
   */
  public BooleanConfigurer(String key, String name, Boolean val) {
    super(key, name, val);
  }

  /**
   * Create an old-style configurer with inbuilt label
   *
   * @param key Configurer key
   * @param name Label text
   * @param val Initial value
   */
  public BooleanConfigurer(String key, String name, boolean val) {
    super(key, name, val ? Boolean.TRUE : Boolean.FALSE);
  }

  /**
   * Create an old-style configurer with inbuilt label
   *
   * @param key Configurer key
   * @param name Label text
   */
  public BooleanConfigurer(String key, String name) {
    this(key, name, Boolean.FALSE);
  }

  /**
   * Create a new-style labeless configurer
   *
   * @param val Initial value
   */
  public BooleanConfigurer(Boolean val) {
    this(null, "", val);
  }

  /**
   * Create a new-style labeless configurer
   *
   * @param val Initial value
   */
  public BooleanConfigurer(boolean val) {
    this(null, "", val);
  }

  @Override
  public String getValueString() {
    return booleanValue().toString();
  }

  public boolean getValueBoolean() {
    return booleanValue();
  }

  @Override
  public void setValue(Object o) {
    super.setValue(o);
    if (box != null && !Boolean.valueOf(box.isSelected()).equals(o)) {
      box.setSelected(booleanValue());
    }
  }

  @Override
  public void setValue(String s) {
    setValue(Boolean.valueOf(s));
  }

  @Override
  public void setName(String s) {
    super.setName(s);
    if (box != null) {
      box.setText(s);
    }
  }

  @Override
  public java.awt.Component getControls() {
    if (p == null) {
      p = new JPanel(new MigLayout(ConfigurerLayout.DEFAULT_CFG_LAYOUT_CONSTRAINTS, (getName() == null || getName().isEmpty()) ? "[]0[0]" : "[][][]", "push[]push"));

      box = new JCheckBox();
      box.setSelected(booleanValue());
      box.addItemListener(e -> setValue(box.isSelected()));
      p.add(box);
    }

    return p;
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    getControls();
    box.addFocusListener(listener);
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    getControls();
    box.removeFocusListener(listener);
  }

  public Boolean booleanValue() {
    return (Boolean) value;
  }

  @Override
  public void setLabelVisible(boolean visible) {
    if (p instanceof ConfigurerPanel) {
      ((ConfigurerPanel) p).setLabelVisibility(visible);
    }
  }
}
