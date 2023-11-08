/*
 *
 * Copyright (c) 2006 by Rodney Kinney
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

import VASSAL.counters.TraitLayout;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.UIManager;

import net.miginfocom.swing.MigLayout;

/**
 * Configures a variable-length list of objects
 *
 * @author rkinney
 *
 */
public abstract class ListConfigurer extends Configurer implements PropertyChangeListener {
  protected JPanel controls;
  protected JPanel configControls;
  protected JPanel panel;
  protected Dimension fixedSize;
  protected List<Configurer> configurers = new ArrayList<>();

  public ListConfigurer(String key, String name) {
    super(key, name, new ArrayList<>());
  }

  public ListConfigurer(String key, String name, List<?> val) {
    super(key, name, val);
  }

  @Override
  public String getValueString() {
    if (getListValue().isEmpty()) {
      return "";
    }
    final Configurer c = buildChildConfigurer();
    final SequenceEncoder se = new SequenceEncoder(',');
    for (final Object value : getListValue()) {
      c.setFrozen(true); // Prevent subsidiary Configurers from firing PropertyChange Events
      c.setValue(value);
      c.setFrozen(false);
      se.append(c.getValueString());
    }
    return se.getValue();
  }

  @Override
  public void setValue(String s) {
    getListValue().clear();
    if (s.length() > 0) {
      final Configurer c = buildChildConfigurer();
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
      while (sd.hasMoreTokens()) {
        c.setValue(sd.nextToken());
        getListValue().add(c.getValue());
      }
    }
    updateControls();
  }

  protected void updateValue() {
    noUpdate = true;
    final ArrayList<Object> newArray = new ArrayList<>();
    for (final Configurer c : configurers) {
      newArray.add(c.getValue());
    }
    setValue(newArray);
    noUpdate = false;
  }

  @Override
  public void setValue(Object o) {
    if (o == null) {
      o = new ArrayList<>();
    }
    super.setValue(o);
    if (!noUpdate) {
      updateControls();
    }
  }

  @Override
  public Component getControls() {
    if (panel == null) {
      panel = new JPanel(new MigLayout(TraitLayout.STANDARD_GAPY, "[grow,fill]", "[]")); // NON-NLS
      panel.setBorder(BorderFactory.createEtchedBorder());
      controls = new JPanel(new MigLayout("ins 2", "[grow,fill]")); // NON-NLS

      configControls = new JPanel(new MigLayout(ConfigurerLayout.STANDARD_INSETS_GAPY, "[grow]")); // NON-NLS

      final JButton addButton = new JButton(Resources.getString("Editor.ListConfigurer.new"));
      addButton.addActionListener(e -> {
        final Configurer c = buildChildConfigurer();
        getListValue().add(c.getValue());
        updateControls();
      });
      final JPanel addPanel = new JPanel(new MigLayout("ins 0", "push[]push")); // NON-NLS
      addPanel.add(addButton);
      controls.add(addPanel, "grow,wrap"); // NON-NLS
      controls.add(configControls, "grow"); // NON-NLS
      panel.add(controls, "grow"); // NON-NLS
      updateControls();
    }
    return panel;
  }

  @SuppressWarnings("unchecked")
  public List<Object> getListValue() {
    return (List<Object>) getValue();
  }

  /**
   * The objects in the list are specified by the Configurer returned here
   *
   * @return objects in the list
   */
  protected abstract Configurer buildChildConfigurer();

  @Override
  public void propertyChange(PropertyChangeEvent evt) {
    updateValue();
  }

  protected void updateControls() {
    if (controls != null) {
      for (final Configurer c : configurers) {
        c.removePropertyChangeListener(this);
      }
      configurers.clear();
      configControls.removeAll();

      for (final Object value : getListValue()) {
        final Configurer c = buildChildConfigurer();
        c.setValue(value);
        c.addPropertyChangeListener(this);
        configurers.add(c);
        final JPanel b = new JPanel(new MigLayout(ConfigurerLayout.STANDARD_INSETS_GAPY, "[fill,grow][]")); // NON-NLS
        final JButton delButton = new JButton(Resources.getString("Editor.ListConfigurer.remove"));
        delButton.addActionListener(e -> {
          getListValue().remove(c.getValue());
          updateControls();
          repack();
        });
        b.add(c.getControls(), "grow"); // NON-NLS
        b.add(delButton, "aligny center"); // NON-NLS
        configControls.add(b, "grow,wrap"); // NON-NLS
      }
      resize();
    }
  }

  /**
   * Resize the overall Configurer to only show Scroll bars if getting close to full screen height
   */
  public void resize() {
    if (controls != null) {
      final Dimension s = controls.getPreferredSize();
      final int t = (Integer) UIManager.get("ScrollBar.width");
      panel.setPreferredSize(
        new Dimension(
          s.width + t + 2,
          Math.min(controls.getPreferredSize().height + 5, (int) getScreenSize().getHeight() - 300)
        )
      );
      repack();
    }
  }

  @Override
  public void repack() {
    repack(panel);
  }

  @Override
  public int hashCode() {
    final String valueString = getValueString();
    final int prime = 31;
    int result = 1;
    result = prime * result
        + ((valueString == null) ? 0 : valueString.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    final ListConfigurer other = (ListConfigurer) obj;
    final String valueString = getValueString();
    final String otherValueString = other.getValueString();
    if (valueString == null) {
      return otherValueString == null;
    }
    else return valueString.equals(otherValueString);
  }
}
