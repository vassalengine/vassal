/*
 *
 * Copyright (c) 2023 by The VASSAL Development Team
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

import VASSAL.build.AbstractBuildable;
import VASSAL.counters.DynamicProperty;
import VASSAL.i18n.Resources;

import net.miginfocom.swing.MigLayout;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import java.awt.Component;
import java.awt.event.FocusListener;

/**
 * A Configurer for the Parameter class
 * The value stored as the value in the Configurer is a Parameter Object
 */
public class ParameterConfigurer extends Configurer {

  protected StringConfigurer propertyNameConfig;
  protected FormattedExpressionConfigurer valueConfig;
  protected JPanel controls;

  public ParameterConfigurer() {
    this(null);
  }

  public ParameterConfigurer(Parameter parameter) {
    super("", "", parameter);
    propertyNameConfig = new StringConfigurer("");
    propertyNameConfig.setHint(Resources.getString("Editor.ParameterConfigurer.name_hint"));
    propertyNameConfig.addPropertyChangeListener(e -> updateValue());

    valueConfig = new FormattedExpressionConfigurer("");
    valueConfig.setHint(Resources.getString("Editor.ParameterConfigurer.value_hint"));
    valueConfig.addPropertyChangeListener(e -> updateValue());
  }

  @Override
  public String getValueString() {
    return getParameterValue().encode();
  }

  public Parameter getParameterValue() {
    return (Parameter) getValue();
  }

  /**
   * Freeze the Configurer from issuing PropertyChange Events.
   * Ensure the subsidiary Configurers are quiet also.
   *
   * @param val true to freeze
   */
  @Override
  public void setFrozen(boolean val) {
    super.setFrozen(val);
    valueConfig.setFrozen(val);
    propertyNameConfig.setFrozen(val);
  }

  @Override
  public void setValue(Object value) {
    if (!noUpdate && value instanceof Parameter && valueConfig != null) {
      final Parameter param = (Parameter) value;
      valueConfig.setValue(param.getValue());
      propertyNameConfig.setValue(param.getPropertyName());
    }
    super.setValue(value);
  }

  public DynamicProperty.DynamicKeyCommand getKeyCommand() {
    return (DynamicProperty.DynamicKeyCommand) getValue();
  }

  @Override
  public void setValue(String s) {
    final Parameter param = new Parameter(s);
    propertyNameConfig.setValue(param.getPropertyName());
    valueConfig.setValue(param.getValue());
    noUpdate = true;
    setValue(param);
    noUpdate = false;
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      buildControls();
    }
    return controls;
  }

  public void updateValue() {
    noUpdate = true;
    setValue(new Parameter(propertyNameConfig.getValueString(), valueConfig.getValueString()));
    noUpdate = false;
  }

  protected void buildControls() {
    controls = new JPanel(new MigLayout("ins panel," + ConfigurerLayout.STANDARD_GAPY + ",hidemode 3", "[]rel[][]rel[]")); // NON-NLS
    controls.setBorder(BorderFactory.createEtchedBorder());

    JLabel label = new JLabel(Resources.getString("Editor.property_name"));
    label.setLabelFor(propertyNameConfig.getControls());
    controls.add(label);
    controls.add(propertyNameConfig.getControls(), "grow"); // NON-NLS

    label = new JLabel(Resources.getString("Editor.value"));
    label.setLabelFor(valueConfig.getControls());
    controls.add(label);
    controls.add(valueConfig.getControls(), "grow,wrap"); // NON-NLS

  }

  public Component getPropertyNameControls() {
    return propertyNameConfig.getControls();
  }

  public Component getValueControls() {
    return valueConfig.getControls();
  }

  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    propertyNameConfig.setHighlighted(highlighted);
    valueConfig.setHighlighted(highlighted);
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    propertyNameConfig.addFocusListener(listener);
    valueConfig.addFocusListener(listener);
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    propertyNameConfig.removeFocusListener(listener);
    valueConfig.removeFocusListener(listener);
  }

  @Override
  public void requestFocus() {
    if (propertyNameConfig != null) {
      propertyNameConfig.requestFocus();
    }
  }

  @Override
  public void setContext(AbstractBuildable context) {
    super.setContext(context);
    propertyNameConfig.setContext(context);
    valueConfig.setContext(context);
  }

  @Override
  public void setContextLevel(ContextLevel contextLevel) {
    super.setContextLevel(contextLevel);
    propertyNameConfig.setContextLevel(contextLevel);
    valueConfig.setContextLevel(contextLevel);
  }
}
