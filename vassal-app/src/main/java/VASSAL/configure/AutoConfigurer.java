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

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.script.expression.FormattedStringExpression;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ReflectionUtils;
import VASSAL.tools.swing.SwingUtils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Image;
import java.awt.Window;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import net.miginfocom.swing.MigLayout;

/**
 * A Configurer for configuring Configurable components
 * (Is that as redundant as it sounds?)
 * Automatically builds a property editor with controls for setting all
 * of the attributes of the target Configurable component
 */
public class AutoConfigurer extends Configurer
  implements PropertyChangeListener {
  protected JPanel p;
  protected AutoConfigurable target;
  protected List<Configurer> configurers = new ArrayList<>();
  protected Map<String, VisibilityCondition> conditions;
  protected Map<String, JComponent> labels = new HashMap<>();

  public AutoConfigurer(AutoConfigurable c) {
    super(null, c.getConfigureName());

    target = c;
    setValue(target);
    target.addPropertyChangeListener(evt -> {
      if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
        setName((String) evt.getNewValue());
      }
    });

    p = new JPanel();
    p.setLayout(new MigLayout("ins panel," + ConfigurerLayout.STANDARD_GAPY + ", hidemode 3", "[align right]rel[fill,grow]")); // NON-NLS

    final String[] name = c.getAttributeNames();
    final String[] prompt = c.getAttributeDescriptions();
    final Class<?>[] type = c.getAttributeTypes();

    final int n = Math.min(name.length, Math.min(prompt.length, type.length));
    for (int i = 0; i < n; ++i) {
      if (type[i] == null) {
        continue;
      }
      final Configurer config;
      config = createConfigurer(type[i], name[i], "", target);
      if (config != null) {
        config.addPropertyChangeListener(this);
        config.setValue(target.getAttributeValueString(name[i]));
        final JLabel label = new JLabel(prompt[i]);
        label.setLabelFor(config.getControls());
        labels.put(name[i], label);
        p.add(label);
        p.add(config.getControls(), "wrap,grow"); // NON-NLS
        configurers.add(config);
      }
      setVisibility(name[i], c.getAttributeVisibility(name[i]));
    }
  }

  public static Configurer createConfigurer(Class<?> type,
                                            String key,
                                            String prompt,
                                            AutoConfigurable target) {
    Configurer config = null;

    if (String.class.isAssignableFrom(type)) {
      config = new StringConfigurer(key, prompt);
    }
    else if (Integer.class.isAssignableFrom(type)) {
      config = new IntConfigurer(key, prompt);
    }
    else if (Double.class.isAssignableFrom(type)) {
      config = new DoubleConfigurer(key, prompt);
    }
    else if (Boolean.class.isAssignableFrom(type)) {
      config = new BooleanConfigurer(key, prompt);
    }
    else if (Image.class.isAssignableFrom(type)) {
      config = new ImageConfigurer(key, prompt,
        GameModule.getGameModule().getArchiveWriter());
    }
    else if (Color.class.isAssignableFrom(type)) {
      config = new ColorConfigurer(key, prompt);
    }
    else if (KeyStroke.class.isAssignableFrom(type)) {
      config = new HotKeyConfigurer(key, prompt);
    }
    else if (NamedKeyStroke.class.isAssignableFrom(type)) {
      config = new NamedHotKeyConfigurer(key, prompt);
    }
    else if (File.class.isAssignableFrom(type)) {
      config = new FileConfigurer(key, prompt,
        GameModule.getGameModule().getArchiveWriter());
    }
    else if (String[].class.isAssignableFrom(type)) {
      config = new StringArrayConfigurer(key, prompt);
    }
    else if (Icon.class.isAssignableFrom(type)) {
      config = new IconConfigurer(key, prompt, null);
    }
    else if (PropertyExpression.class.isAssignableFrom(type)) {
      config = new PropertyExpressionConfigurer(key, prompt);
    }
    else if (FormattedStringExpression.class.isAssignableFrom(type)) {
      config = new FormattedExpressionConfigurer(key, prompt);
    }
    else if (TranslatableStringEnum.class.isAssignableFrom(type)) {
      TranslatableStringEnum se = null;
      try {
        se = (TranslatableStringEnum) type.getConstructor().newInstance();
      }
      catch (Throwable t) {
        ReflectionUtils.handleNewInstanceFailure(t, type);
        config = new StringConfigurer(key, prompt);
      }

      if (se != null) {
        final String[] validValues = se.getValidValues(target);
        final String[] i18nKeys = se.getI18nKeys(target);
        config = new TranslatingStringEnumConfigurer(key, prompt, validValues, i18nKeys, se.isDisplayNames());
      }
    }
    else if (StringEnum.class.isAssignableFrom(type)) {
      StringEnum se = null;
      try {
        se = (StringEnum) type.getConstructor().newInstance();
      }
      catch (Throwable t) {
        ReflectionUtils.handleNewInstanceFailure(t, type);
        config = new StringConfigurer(key, prompt);
      }

      if (se != null) {
        final String[] validValues = se.getValidValues(target);
        config = new StringEnumConfigurer(key, prompt, validValues);
      }
    }
    else if (ConfigurerFactory.class.isAssignableFrom(type)) {
      ConfigurerFactory cf = null;
      try {
        cf = (ConfigurerFactory) type.getConstructor().newInstance();
      }
      catch (Throwable t) {
        ReflectionUtils.handleNewInstanceFailure(t, type);
      }

      if (cf != null) {
        config = cf.getConfigurer(target, key, prompt);
      }
    }
    else {
      throw new IllegalArgumentException("Invalid class " + type.getName());
    }
    return config;
  }

  public void reset() {
    final String[] s = target.getAttributeNames();
    for (final String item : s) {
      final Configurer config = getConfigurer(item);
      if (config != null) {
        config.setValue(target.getAttributeValueString(item));
      }
    }
  }

  @Override
  public String getValueString() {
    return target.getConfigureName();
  }

  @Override
  public void setValue(String s) {
    throw new UnsupportedOperationException(
      "Can't set Configurable from String");
  }

  @Override
  public Component getControls() {
    return p;
  }

  @Override
  public void propertyChange(final PropertyChangeEvent evt) {
    target.setAttribute(evt.getPropertyName(), evt.getNewValue());
    checkVisibility();
  }

  public void setVisibility(String attribute, VisibilityCondition c) {
    if (c != null) {
      if (conditions == null) {
        conditions = new HashMap<>();
      }
      conditions.put(attribute, c);
      checkVisibility();
    }
  }

  protected void checkVisibility() {
    boolean visChanged = false;
    if (conditions != null) {
      for (final Configurer c : configurers) {
        final VisibilityCondition cond = conditions.get(c.getKey());
        if (cond != null) {
          if (c.getControls().isVisible() != cond.shouldBeVisible()) {
            visChanged = true;
            c.getControls().setVisible(cond.shouldBeVisible());
            final JComponent label = labels.get(c.getKey());
            if (label != null) {
              label.setVisible(cond.shouldBeVisible());
            }
          }
        }
      }
      // Only repack the configurer if an item visibility has changed.
      // And ensure the dialog still fits on the screen
      if (visChanged && p.getTopLevelAncestor() instanceof Window) {
        SwingUtils.repack((Window) p.getTopLevelAncestor());
      }
    }
  }

  public Configurer getConfigurer(String attribute) {
    for (final Configurer c : configurers) {
      if (attribute.equals(c.getKey())) {
        return c;
      }
    }
    return null;
  }

  public JComponent getLabel(String key) {
    return labels.get(key);
  }
}
