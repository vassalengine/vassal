/*
 *
 * Copyright (c) 2020 by The VASSAL Development Team
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

import VASSAL.build.module.properties.PropertyChangerConfigurer;
import VASSAL.build.module.properties.PropertySetter;
import VASSAL.counters.Decorator;
import VASSAL.counters.DynamicProperty;
import VASSAL.i18n.Resources;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.event.FocusListener;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

public class DynamicKeyCommandConfigurer extends Configurer {
  protected final NamedHotKeyConfigurer keyConfig;
  protected PropertyChangerConfigurer propChangeConfig;
  protected StringConfigurer commandConfig;
  protected JPanel controls;
  protected DynamicProperty target;

  public DynamicKeyCommandConfigurer(DynamicProperty target) {
    super(target.getKey(), target.getKey(),
      new DynamicProperty.DynamicKeyCommand(
        "",
        NamedKeyStroke.NULL_KEYSTROKE,
        Decorator.getOutermost(target),
        target,
        new PropertySetter("", target)));

    commandConfig = new StringConfigurer("");
    commandConfig.setHint(Resources.getString("Editor.menu_command_hint"));
    keyConfig = new NamedHotKeyConfigurer(NamedKeyStroke.NULL_KEYSTROKE);
    propChangeConfig = new PropertyChangerConfigurer(null, target.getKey(), target);
    propChangeConfig.setValue(new PropertySetter("", target));

    commandConfig.addPropertyChangeListener(e -> updateValue());
    keyConfig.addPropertyChangeListener(e -> updateValue());
    propChangeConfig.addPropertyChangeListener(e -> {
      updateValue();
      repack(commandConfig.getControls());
    });
    this.target = target;
  }

  public DynamicProperty getTarget() {
    return target;
  }

  public void setTarget(DynamicProperty target) {
    this.target = target;
  }

  @Override
  public String getValueString() {
    final SequenceEncoder se = new SequenceEncoder(':');
    se.append(commandConfig.getValueString()).append(keyConfig.getValueString()).append(propChangeConfig.getValueString());
    return se.getValue();
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
    commandConfig.setFrozen(val);
    keyConfig.setFrozen(val);
    propChangeConfig.setFrozen(val);
  }

  @Override
  public void setValue(Object value) {
    if (!noUpdate && value instanceof DynamicProperty.DynamicKeyCommand && commandConfig != null) {
      final DynamicProperty.DynamicKeyCommand dkc = (DynamicProperty.DynamicKeyCommand) value;
      commandConfig.setValue(dkc.getName());
      keyConfig.setValue(dkc.getNamedKeyStroke());
      propChangeConfig.setValue(dkc.getPropChanger());
    }
    super.setValue(value);
  }

  public DynamicProperty.DynamicKeyCommand getKeyCommand() {
    return (DynamicProperty.DynamicKeyCommand) getValue();
  }

  @Override
  public void setValue(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s == null ? "" : s, ':');
    commandConfig.setValue(sd.nextToken(""));
    keyConfig.setValue(sd.nextNamedKeyStroke(null));
    propChangeConfig.setValue(sd.nextToken(""));
    updateValue();
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
    setValue(new DynamicProperty.DynamicKeyCommand(commandConfig.getValueString(), keyConfig.getValueNamedKeyStroke(), target, target, propChangeConfig.getPropertyChanger()));
    noUpdate = false;
  }

  protected void buildControls() {
    controls = new JPanel(new MigLayout("ins panel," + ConfigurerLayout.STANDARD_GAPY + ",hidemode 3", "[]rel[][]rel[]")); // NON-NLS
    controls.setBorder(BorderFactory.createEtchedBorder());
    JLabel label = new JLabel(Resources.getString("Editor.menu_command"));
    label.setLabelFor(commandConfig.getControls());
    controls.add(label);
    controls.add(commandConfig.getControls(), "grow"); // NON-NLS

    label = new JLabel(Resources.getString("Editor.keyboard_command"));
    label.setLabelFor(keyConfig.getControls());
    controls.add(label);
    controls.add(keyConfig.getControls(), "grow,wrap"); // NON-NLS

    controls.add(propChangeConfig.getTypeLabel());
    controls.add(propChangeConfig.getTypeControls(), "grow"); // NON-NLS

    controls.add(propChangeConfig.getChangerLabel());
    controls.add(propChangeConfig.getChangerControls(), "growx,aligny center,wrap"); // NON-NLS
    controls.add(propChangeConfig.getValuesControls(), "grow,span 4"); // NON-NLS
  }

  public Component getCommandControls() {
    return commandConfig.getControls();
  }

  public Component getKeyControls() {
    return keyConfig.getControls();
  }

  public Component getTypeControls() {
    return propChangeConfig.getTypeControls();
  }

  public Component getChangerControls() {
    return propChangeConfig.getChangerControls();
  }

  public Component getValuesControls() {
    return propChangeConfig.getValuesControls();
  }

  public boolean isEnumType() {
    return propChangeConfig.isEnumType();
  }

  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    commandConfig.setHighlighted(highlighted);
    keyConfig.setHighlighted(highlighted);
    propChangeConfig.setHighlighted(highlighted);
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    commandConfig.addFocusListener(listener);
    keyConfig.addFocusListener(listener);
    propChangeConfig.addFocusListener(listener);
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    commandConfig.removeFocusListener(listener);
    keyConfig.removeFocusListener(listener);
    propChangeConfig.removeFocusListener(listener);
  }

  @Override
  public void requestFocus() {
    if (commandConfig != null) {
      commandConfig.requestFocus();
    }
  }
}
