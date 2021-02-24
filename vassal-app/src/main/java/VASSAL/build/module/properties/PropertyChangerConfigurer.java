/*
 *
 * Copyright(c) 2006-2012 by Rodney Kinney, Brent Easton
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
package VASSAL.build.module.properties;

import VASSAL.configure.ConfigurableListEntry;
import VASSAL.configure.Configurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.FormattedStringArrayConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JLabel;
import javax.swing.JPanel;

import net.miginfocom.swing.MigLayout;

/**
 * Configurer instance that allows a module editor to specify a
 * PropertyChanger, i.e. the way in which a dynamic property will be
 * updated by a player during a game
 *
 * @author rkinney
 *
 */
public class PropertyChangerConfigurer extends Configurer {
  protected static final String PLAIN_TYPE = "Set value directly"; //NON-NLS (really)
  protected static final String INCREMENT_TYPE = "Increment numeric value"; //NON-NLS (really)
  protected static final String PROMPT_TYPE = "Prompt user"; //NON-NLS (really)
  protected static final String SELECT_TYPE = "Prompt user to select from list"; //NON-NLS (really)
  protected static final char PLAIN_CODE = 'P';
  protected static final char PROMPT_CODE = 'R';
  protected static final char ENUM_CODE = 'E';
  protected static final char INCR_CODE = 'I';
  protected static final Map<Class<? extends PropertyChanger>, Character> typeToCode = new HashMap<>();
  protected static final Map<Class<? extends PropertyChanger>, String> typeToDescription = new HashMap<>();
  protected static final Map<String, Character> descriptionToCode = new HashMap<>();

  static {
    typeToCode.put(PropertySetter.class, PLAIN_CODE);
    typeToCode.put(PropertyPrompt.class, PROMPT_CODE);
    typeToCode.put(NumericPropertyPrompt.class, PROMPT_CODE);
    typeToCode.put(IncrementProperty.class, INCR_CODE);
    typeToCode.put(EnumeratedPropertyPrompt.class, ENUM_CODE);
    typeToDescription.put(PropertySetter.class, PLAIN_TYPE);
    typeToDescription.put(PropertyPrompt.class, PROMPT_TYPE);
    typeToDescription.put(NumericPropertyPrompt.class, PROMPT_TYPE);
    typeToDescription.put(IncrementProperty.class, INCREMENT_TYPE);
    typeToDescription.put(EnumeratedPropertyPrompt.class, SELECT_TYPE);
    descriptionToCode.put(PLAIN_TYPE, PLAIN_CODE);
    descriptionToCode.put(INCREMENT_TYPE, INCR_CODE);
    descriptionToCode.put(PROMPT_TYPE, PROMPT_CODE);
    descriptionToCode.put(SELECT_TYPE, ENUM_CODE);
  }
  protected Constraints constraints;
  protected JPanel controls;
  protected JLabel typeLabel;
  protected TranslatingStringEnumConfigurer typeConfig;
  protected JLabel valueLabel;
  protected FormattedExpressionConfigurer valueConfig;
  private JPanel valueControlsPanel;
  protected JLabel promptLabel;
  protected StringConfigurer promptConfig;
  protected JLabel incrLabel;
  protected FormattedExpressionConfigurer incrConfig;
  protected StringArrayConfigurer validValuesConfig;
  protected JPanel changerLabelControls;
  protected JPanel changerControls;
  private List<Configurer> subConfigurers;

  public PropertyChangerConfigurer(String key, String name, Constraints constraints) {
    super(key, name);
    this.constraints = constraints;
    setValue(new PropertySetter("", null));
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      final PropertyChangeListener l = evt -> {
        // Do not remove either of these calls to updateValue(). Both are required for correct operation
        // and screen repacking.
        updateValue();
        updateControls();
        updateValue();
      };
      controls = new JPanel();
      controls.setLayout(new MigLayout("ins 0,hidemode 3", "[]rel[][]rel[fill,grow][fill,grow]")); // NON-NLS

      typeLabel = new JLabel(Resources.getString("Editor.PropertyChangeConfigurer.type"));
      typeConfig = new TranslatingStringEnumConfigurer(
        new String[]{
          PLAIN_TYPE,
          INCREMENT_TYPE,
          PROMPT_TYPE,
          SELECT_TYPE
        },
        new String[]{
          "Editor.PropertyChangeConfigurer.plain_type",
          "Editor.PropertyChangeConfigurer.increment_type",
          "Editor.PropertyChangeConfigurer.prompt_type",
          "Editor.PropertyChangeConfigurer.select_type",
        });
      typeConfig.addPropertyChangeListener(l);
      typeLabel.setLabelFor(typeConfig.getControls());

      valueLabel = new JLabel(Resources.getString("Editor.PropertyChangeConfigurer.new_value"));
      valueConfig = new FormattedExpressionConfigurer("", constraints);
      valueConfig.setHint(Resources.getString("Editor.PropertyChangeConfigurer.plain_hint"));
      valueLabel.setLabelFor(valueConfig.getControls());
      valueConfig.addPropertyChangeListener(l);

      promptLabel = new JLabel(Resources.getString("Editor.PropertyChangeConfigurer.prompt"));
      promptConfig = new StringConfigurer("");
      promptConfig.setHint(Resources.getString("Editor.PropertyChangeConfigurer.prompt_hint"));
      promptLabel.setLabelFor(promptConfig.getControls());
      promptConfig.addPropertyChangeListener(l);

      incrLabel = new JLabel(Resources.getString("Editor.PropertyChangeConfigurer.increment_by"));
      incrConfig = new FormattedExpressionConfigurer("", constraints);
      incrConfig.setHint(Resources.getString("Editor.PropertyChangeConfigurer.increment_hint"));
      incrLabel.setLabelFor(incrConfig.getControls());
      incrConfig.addPropertyChangeListener(l);

      validValuesConfig = new NoUpdateFormattedStringArrayConfigurer(null, Resources.getString("Editor.PropertyChangeConfigurer.valid_values"), constraints);
      validValuesConfig.setHint(Resources.getString("Editor.PropertyChangeConfigurer.valid_values_hint"));
      validValuesConfig.addPropertyChangeListener(l);

      valueControlsPanel = new JPanel(new MigLayout("ins 0,hidemode 3", "[]rel[grow,fill]rel[]")); // NON-NLS
      valueControlsPanel.add(new JLabel(Resources.getString("Editor.PropertyChangeConfigurer.values"))); // NON-NLS
      valueControlsPanel.add(validValuesConfig.getControls(), "grow"); // NON-NLS
      valueControlsPanel.add(validValuesConfig.getListController(), "aligny center"); // NON-NLS

      controls.add(typeLabel, "aligny center"); // NON-NLS
      controls.add(typeConfig.getControls(), "aligny center"); // NON-NLS

      changerLabelControls = new JPanel(new MigLayout("ins 0,hidemode 3", "[]", "[]")); // NON-NLS
      changerLabelControls.add(valueLabel, "aligny center"); // NON-NLS
      changerLabelControls.add(promptLabel, "aligny center"); // NON-NLS
      changerLabelControls.add(incrLabel, "aligny center"); // NON-NLS
      controls.add(changerLabelControls, "aligny center"); // NON-NLS

      changerControls = new JPanel(new MigLayout("ins 0,hidemode 3", "[fill,grow]")); // NON-NLS
      changerControls.add(valueConfig.getControls(), "growx,aligny center"); // NON-NLS
      changerControls.add(promptConfig.getControls(), "growx,aligny center"); // NON-NLS
      changerControls.add(incrConfig.getControls(), "growx,aligny center"); // NON-NLS
      controls.add(changerControls, "aligny center,growx"); // NON-NLS
      controls.add(valueControlsPanel, "aligny center,growx"); // NON-NLS

      subConfigurers = List.of(typeConfig, incrConfig, valueConfig, promptConfig, validValuesConfig);

      updateControls();
    }
    return controls;
  }

  public Component getTypeLabel() {
    getControls();
    return typeLabel;
  }

  public Component getTypeControls() {
    getControls();
    return typeConfig.getControls();
  }

  public Component getChangerLabel() {
    getControls();
    return changerLabelControls;
  }

  public Component getChangerControls() {
    getControls();
    return changerControls;
  }

  public Component getValuesControls() {
    getControls();
    return valueControlsPanel;
  }

  protected void updateControls() {
    final PropertyChanger pc = getPropertyChanger();
    typeConfig.setValue(typeToDescription.get(pc.getClass()));
    if (pc instanceof PropertySetter) {
      valueConfig.setValue(((PropertySetter) pc).getRawValue());
      valueConfig.getControls().setVisible(true);
      valueControlsPanel.setVisible(true);
    }
    else {
      valueConfig.getControls().setVisible(false);
      valueLabel.setVisible(false);
    }
    if (pc instanceof IncrementProperty) {
      incrConfig.setValue(String.valueOf(((IncrementProperty) pc).getIncrement()));
      incrConfig.getControls().setVisible(true);
      incrLabel.setVisible(true);
    }
    else {
      incrConfig.getControls().setVisible(false);
      incrLabel.setVisible(false);
    }
    if (pc instanceof PropertyPrompt) {
      promptConfig.setValue(((PropertyPrompt) pc).getPrompt());
      promptConfig.getControls().setVisible(true);
      promptLabel.setVisible(true);
    }
    else {
      promptConfig.getControls().setVisible(false);
      promptLabel.setVisible(false);
    }
    if (pc instanceof EnumeratedPropertyPrompt) {
      validValuesConfig.setValue(((EnumeratedPropertyPrompt) pc).getValidValues());
      valueControlsPanel.setVisible(true);
    }
    else {
      valueControlsPanel.setVisible(false);
    }
    repack();
  }

  protected void updateValue() {
    final PropertyChanger p;
    switch (descriptionToCode.get(typeConfig.getValueString())) {
    case PROMPT_CODE:
      p = new PropertyPrompt(constraints, promptConfig.getValueString());
      break;
    case INCR_CODE:
      p = new IncrementProperty(this, incrConfig.getValueString(), constraints);
      break;
    case ENUM_CODE:
      p = new EnumeratedPropertyPrompt(
        constraints, promptConfig.getValueString(),
        validValuesConfig.getStringArray(), constraints
      );
      break;
    case PLAIN_CODE:
    default:
      p = new PropertySetter(valueConfig.getValueString(), constraints);
    }
    setValue(p);
  }

  @Override
  public String getValueString() {
    final PropertyChanger propChanger = getPropertyChanger();
    final SequenceEncoder se = new SequenceEncoder(',');
    if (propChanger != null) {
      switch (typeToCode.get(propChanger.getClass())) {
      case PROMPT_CODE:
        se.append(PROMPT_CODE)
          .append(((PropertyPrompt) propChanger).getPrompt());
        break;
      case INCR_CODE:
        se.append(INCR_CODE)
          .append(((IncrementProperty) propChanger).getIncrement());
        break;
      case ENUM_CODE:
        se.append(ENUM_CODE)
          .append(((PropertyPrompt) propChanger).getPrompt())
          .append(((EnumeratedPropertyPrompt) propChanger).getValidValues());
        break;
      case PLAIN_CODE:
        se.append(PLAIN_CODE)
          .append(((PropertySetter) propChanger).getRawValue());
      }
    }
    return se.getValue();
  }

  public boolean isEnumType() {
    return typeToCode.get(getPropertyChanger().getClass()) == ENUM_CODE;
  }

  public PropertyChanger getPropertyChanger() {
    return (PropertyChanger) getValue();
  }

  @Override
  public void setValue(String s) {
    final PropertyChanger p;
    if (s == null || s.length() == 0) {
      s = Character.toString(PLAIN_CODE);
    }
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    switch (sd.nextChar(PLAIN_CODE)) {
    case PROMPT_CODE:
      p = new PropertyPrompt(constraints, sd.nextToken(Resources.getString("Editor.PropertyChangeConfigurer.enter_new_value")));
      break;
    case INCR_CODE:
      p = new IncrementProperty(this, sd.nextToken("1"), constraints);
      break;
    case ENUM_CODE:
      p = new EnumeratedPropertyPrompt(constraints, sd.nextToken(Resources.getString("Editor.PropertyChangeConfigurer.select_new_value")), sd.nextStringArray(0), constraints);
      break;
    case PLAIN_CODE:
    default:
      p = new PropertySetter(sd.nextToken("new value"), constraints); //NON-NLS
    }
    setValue(p);
  }

  public interface Constraints extends PropertyPrompt.Constraints, IncrementProperty.Constraints, PropertySource {
  }

  @Override
  public void setHighlighted(boolean highlighted) {
    super.setHighlighted(highlighted);
    getControls();
    subConfigurers.forEach(c -> c.setHighlighted(highlighted));
  }

  @Override
  public void addFocusListener(FocusListener listener) {
    super.addFocusListener(listener);
    getControls();
    subConfigurers.forEach(c -> c.addFocusListener(listener));
  }

  @Override
  public void removeFocusListener(FocusListener listener) {
    super.removeFocusListener(listener);
    getControls();
    subConfigurers.forEach(c -> c.removeFocusListener(listener));
  }

  private static class NoUpdateFormattedStringArrayConfigurer extends FormattedStringArrayConfigurer {

    public NoUpdateFormattedStringArrayConfigurer(String key, String name, Constraints c) {
      super(key, name, c);
    }

    @Override
    public void entryChanged(ConfigurableListEntry entry) {
      getStringArray()[getEntries().indexOf(entry)] = entry.getConfigurer().getValueString();
    }
  }

}
