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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import VASSAL.tools.SequenceEncoder;

/**
 * Configures a variable-length list of objects
 *
 * @author rkinney
 *
 */
public abstract class ListConfigurer extends Configurer implements
    PropertyChangeListener {
  protected Box controls;
  protected Box configControls;
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
      controls = Box.createVerticalBox();
      final JScrollPane scroll = new JScrollPane(controls);
      controls.setBorder(BorderFactory.createTitledBorder(getName()));
      configControls = Box.createVerticalBox();

      JButton addButton = new JButton("New");
      addButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          Configurer c = buildChildConfigurer();
          getListValue().add(c.getValue());
          updateControls();
        }
      });
      controls.add(addButton);
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
   * @return
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
        final Box b = Box.createHorizontalBox();
        JButton delButton = new JButton("Remove");
        delButton.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            getListValue().remove(c.getValue());
            updateControls();
            repack();
          }
        });
        b.add(delButton);
        b.add(c.getControls());
        configControls.add(b);
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
      w.pack();
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
      if (otherValueString != null)
        return false;
    }
    else if (!valueString.equals(otherValueString))
      return false;
    return true;
  }
}
