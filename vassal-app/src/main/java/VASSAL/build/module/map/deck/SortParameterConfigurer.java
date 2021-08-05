/*
 *
 * Copyright (c) 2021 by The VASSAL Development Team
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
package VASSAL.build.module.map.deck;

import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerLayout;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;

import java.awt.Component;
import java.awt.event.FocusListener;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

public class SortParameterConfigurer extends Configurer {

  private JPanel controls;
  private StringConfigurer propertyConfig;
  private BooleanConfigurer ascendingConfig;

  public SortParameterConfigurer(String key, String name, Object val) {
    super(key, name, val);
  }

  public SortParameterConfigurer(Object val) {
    this("", "",  val);
  }

  public SortParameterConfigurer() {
    this(null);
  }

  @Override
  public String getValueString() {
    return value == null ? "" : ((SortParameter) getValue()).toString();
  }

  @Override
  public void setValue(String s) {
    setValue(new SortParameter(s));
  }

  @Override
  public void setValue(Object o) {
    if (propertyConfig == null) {
      propertyConfig = new StringConfigurer("");
      propertyConfig.setHintKey("Editor.DeckSortKeyCommand.name_hint");
      ascendingConfig = new BooleanConfigurer(true);
    }
    setFrozen(true); // Prevent changes to the sub-configurers triggering further updates
    if (!noUpdate && o instanceof SortParameter) {
      final SortParameter param = (SortParameter) o;
      propertyConfig.setValue(param == null ? "" : param.getSortProperty());
      ascendingConfig.setValue(param == null || param.isAscendingSort()); // Default to true
    }
    setFrozen(false);
    super.setValue(o);
  }

  @Override
  public void setFrozen(boolean val) {
    super.setFrozen(val);
    propertyConfig.setFrozen(val);
    ascendingConfig.setFrozen(val);
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      controls = new JPanel(new MigLayout("ins 2," + ConfigurerLayout.STANDARD_GAPY + ",hidemode 3", "[]rel[][]rel[]")); // NON-NLS
      controls.setBorder(BorderFactory.createEtchedBorder());
      controls.add(new JLabel(Resources.getString("Editor.DeckSortKeyCommand.name")));
      controls.add(propertyConfig.getControls());
      controls.add(new JLabel(Resources.getString("Editor.DeckSortKeyCommand.sort_up")));
      controls.add(ascendingConfig.getControls());
      propertyConfig.addPropertyChangeListener(e -> updateValue());
      ascendingConfig.addPropertyChangeListener(e -> updateValue());
    }
    return controls;
  }
  private void updateValue() {
    noUpdate = true;
    setValue(new SortParameter(ascendingConfig.booleanValue(), propertyConfig.getValueString()));
    noUpdate = false;
  }

  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    propertyConfig.setHighlighted(highlighted);
    ascendingConfig.setHighlighted(highlighted);
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    propertyConfig.addFocusListener(listener);
    ascendingConfig.addFocusListener(listener);
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    propertyConfig.removeFocusListener(listener);
    ascendingConfig.removeFocusListener(listener);
  }

  @Override
  public void requestFocus() {
    if (propertyConfig != null) {
      propertyConfig.requestFocus();
    }
  }

}
