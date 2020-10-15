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

import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;

/**
 * Configures a variable-length list of objects
 *
 * @author rkinney
 *
 */
public abstract class ListConfigurer extends Configurer implements
    PropertyChangeListener {
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
    Configurer c = buildChildConfigurer();
    SequenceEncoder se = new SequenceEncoder(',');
    for (Object value : getListValue()) {
      c.setValue(value);
      se.append(c.getValueString());
    }
    return se.getValue();
  }

  @Override
  public void setValue(String s) {
    getListValue().clear();
    if (s.length() > 0) {
      Configurer c = buildChildConfigurer();
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
      while (sd.hasMoreTokens()) {
        c.setValue(sd.nextToken());
        getListValue().add(c.getValue());
      }
    }
    updateControls();
  }

  protected void updateValue() {
    noUpdate = true;
    ArrayList<Object> newArray = new ArrayList<>();
    for (Configurer c : configurers) {
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
      panel = new JPanel(new BorderLayout());
      controls = new JPanel(new MigLayout("ins 2", "[]")); // NON-NLS
      final JScrollPane scroll = new JScrollPane(controls);
      controls.setBorder(BorderFactory.createTitledBorder(getName()));
      configControls = new JPanel(new MigLayout("ins 0,gapy 2", "[]")); // NON-NLS

      JButton addButton = new JButton(Resources.getString("Editor.ListConfigurer.new"));
      addButton.addActionListener(e -> {
        Configurer c = buildChildConfigurer();
        getListValue().add(c.getValue());
        updateControls();
      });
      controls.add(addButton, "center,wrap"); // NON-NLS
      controls.add(configControls);
      panel.add(scroll, BorderLayout.CENTER);
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
      for (Configurer c : configurers) {
        c.removePropertyChangeListener(this);
      }
      configurers.clear();
      configControls.removeAll();

      for (Object value : getListValue()) {
        final Configurer c = buildChildConfigurer();
        c.setValue(value);
        c.addPropertyChangeListener(this);
        configurers.add(c);
        final JPanel b = new JPanel(new MigLayout("ins 0,gapy 2", "[][fill,grow]")); // NON-NLS
        JButton delButton = new JButton(Resources.getString("Editor.ListConfigurer.remove"));
        delButton.addActionListener(e -> {
          getListValue().remove(c.getValue());
          updateControls();
          repack();
        });
        b.add(delButton, "aligny center"); // NON-NLS
        b.add(c.getControls(), "grow"); // NON-NLS
        configControls.add(b, "grow,wrap"); // NON-NLS
        if (configurers.size() > 5) {
          if (fixedSize == null) {
            fixedSize = new Dimension(
                panel.getPreferredSize().width + 20, 210);
          }
          panel.setPreferredSize(fixedSize);
        }
        else {
          panel.setPreferredSize(null);
        }
      }
      repack();
    }
  }

  public void repack() {
    Window w = SwingUtilities.getWindowAncestor(controls);
    if (w != null) {
      w.setMinimumSize(w.getSize());
      w.pack();
      w.setMinimumSize(null);
    }
  }

  @Override
  public int hashCode() {
    String valueString = getValueString();
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
    ListConfigurer other = (ListConfigurer) obj;
    String valueString = getValueString();
    String otherValueString = other.getValueString();
    if (valueString == null) {
      return otherValueString == null;
    }
    else return valueString.equals(otherValueString);
  }
}
