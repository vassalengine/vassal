/*
 * $Id$
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

import java.awt.Component;
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
import javax.swing.SwingUtilities;
import VASSAL.tools.SequenceEncoder;

/**
 * Configures a variable-length list of objects
 * 
 * @author rkinney
 * 
 */
public abstract class ListConfigurer extends Configurer
                                     implements PropertyChangeListener {
  protected Box controls;
  protected Box configControls;
  protected List<Configurer> configurers = new ArrayList<Configurer>();

  public ListConfigurer(String key, String name) {
    super(key, name, new ArrayList<Object>());
  }

  public ListConfigurer(String key, String name, List val) {
    super(key, name, val);
  }

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
    ArrayList<Object> newArray = new ArrayList<Object>();
    for (Configurer c : configurers) {
      newArray.add(c.getValue());
    }
    setValue(newArray);
    noUpdate = false;
  }

  public void setValue(Object o) {
    if (o == null) {
      o = new ArrayList<Object>();
    }
    super.setValue(o);
    if (!noUpdate) {
      updateControls();
    }
  }

  public Component getControls() {
    if (controls == null) {
      controls = Box.createVerticalBox();
      controls.setBorder(BorderFactory.createTitledBorder(getName()));
      configControls = Box.createVerticalBox();
      controls.add(configControls);
      JButton addButton = new JButton("New");
      addButton.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Configurer c = buildChildConfigurer();
          getListValue().add(c.getValue());
          updateControls();
        }
      });
      controls.add(addButton);
      updateControls();
    }
    return controls;
  }

  public List<Object> getListValue() {
    return (List<Object>) getValue();
  }

  /**
   * The objects in the list are specified by the Configurer returned here
   * 
   * @return
   */
  protected abstract Configurer buildChildConfigurer();

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
          public void actionPerformed(ActionEvent e) {
            getListValue().remove(c.getValue());
            updateControls();
            repack();
          }
        });
        b.add(delButton);
        b.add(c.getControls());
        configControls.add(b);
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
}
