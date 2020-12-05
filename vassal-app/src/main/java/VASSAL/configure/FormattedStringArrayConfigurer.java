/*
 * $Id: FormattedStringArrayConfigurer.java 7861 2011-10-01 06:23:11Z swampwallaby $
 *
 * Copyright (c) 2011-2012 by Brent Easton
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

import VASSAL.build.module.properties.PropertyChangerConfigurer.Constraints;
import VASSAL.counters.GamePiece;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.awt.event.FocusListener;

public class FormattedStringArrayConfigurer extends StringArrayConfigurer {

  protected FormattedExpressionConfigurer config;
  protected GamePiece target;

  public FormattedStringArrayConfigurer(String key, String name, Object val) {
    super(key, name, val);
  }

  public FormattedStringArrayConfigurer(String key, String name) {
    super(key, name);
  }

  public FormattedStringArrayConfigurer(Constraints c) {
    this(null, "", c);
  }

  public FormattedStringArrayConfigurer(String key, String name, Constraints c, int minRows, int maxRows) {
    super(key, name, minRows, maxRows);
    if (c instanceof GamePiece) {
      target = (GamePiece) c;
    }
  }
  public FormattedStringArrayConfigurer(String key, String name, Constraints c) {
    super(key, name);
    if (c instanceof GamePiece) {
      target = (GamePiece) c;
    }
  }

  public FormattedStringArrayConfigurer(String key, String name, GamePiece target) {
    this(key, name);
    this.target = target;
  }

  @Override
  protected Component getTextComponent() {
    if (config == null) {
      super.getTextComponent(); // Initialise any superclass
      config = new FormattedExpressionConfigurer("", target);
    }
    return config.getControls();
  }

  @Override
  protected String getTextValue() {
    return config.getValueString();
  }

  @Override
  protected void setTextValue(String s) {
    config.setValue(s);
  }

  @Override
  protected void addTextActionListener(ActionListener a) {

  }

  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    getControls();
    config.setHighlighted(highlighted);
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    getControls();
    config.addFocusListener(listener);
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    getControls();
    config.removeFocusListener(listener);
  }
}