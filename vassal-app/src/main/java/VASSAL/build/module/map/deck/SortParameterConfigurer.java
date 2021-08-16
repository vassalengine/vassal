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
  private BooleanConfigurer descendingConfig;
  private BooleanConfigurer numericConfig;

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
      descendingConfig = new BooleanConfigurer(false);
      numericConfig = new BooleanConfigurer(false);
    }
    setFrozen(true); // Prevent changes to the sub-configurers triggering further updates
    if (!noUpdate && o instanceof SortParameter) {
      final SortParameter param = (SortParameter) o;
      propertyConfig.setValue(param == null ? "" : param.getSortProperty());
      descendingConfig.setValue(param != null && param.isDescendingSort()); // Default to false
      numericConfig.setValue(param != null && param.isNumericSort()); // Default to false
    }
    setFrozen(false);
    super.setValue(o);
  }

  @Override
  public void setFrozen(boolean val) {
    super.setFrozen(val);
    propertyConfig.setFrozen(val);
    descendingConfig.setFrozen(val);
    numericConfig.setFrozen(val);
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      controls = new JPanel(new MigLayout("ins 2," + ConfigurerLayout.STANDARD_GAPY + ",hidemode 3", "[]rel[][]rel[][]rel[]")); // NON-NLS
      controls.setBorder(BorderFactory.createEtchedBorder());

      final JLabel propertyLabel = new JLabel(Resources.getString("Editor.DeckSortKeyCommand.name"));
      propertyLabel.setLabelFor(propertyConfig.getControls());
      controls.add(propertyLabel);
      controls.add(propertyConfig.getControls());

      final JLabel descendingLabel = new JLabel(Resources.getString("Editor.DeckSortKeyCommand.descending"));
      descendingLabel.setLabelFor(descendingConfig.getControls());
      controls.add(descendingLabel);
      controls.add(descendingConfig.getControls());

      final JLabel numericLabel = new JLabel(Resources.getString("Editor.DeckSortKeyCommand.numeric"));
      numericLabel.setLabelFor(numericConfig.getControls());
      controls.add(numericLabel);
      controls.add(numericConfig.getControls());

      propertyConfig.addPropertyChangeListener(e -> updateValue());
      descendingConfig.addPropertyChangeListener(e -> updateValue());
      numericConfig.addPropertyChangeListener(e -> updateValue());
    }
    return controls;
  }

  private void updateValue() {
    noUpdate = true;
    setValue(new SortParameter(propertyConfig.getValueString(), descendingConfig.booleanValue(), numericConfig.booleanValue()));
    noUpdate = false;
  }

  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    propertyConfig.setHighlighted(highlighted);
    descendingConfig.setHighlighted(highlighted);
    numericConfig.setHighlighted(highlighted);
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    propertyConfig.addFocusListener(listener);
    descendingConfig.addFocusListener(listener);
    numericConfig.addFocusListener(listener);
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    propertyConfig.removeFocusListener(listener);
    descendingConfig.removeFocusListener(listener);
    numericConfig.removeFocusListener(listener);
  }

  @Override
  public void requestFocus() {
    if (propertyConfig != null) {
      propertyConfig.requestFocus();
    }
  }

}
