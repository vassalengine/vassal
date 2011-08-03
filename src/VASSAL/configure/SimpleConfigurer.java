/*
 * $Id$
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

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import VASSAL.build.Configurable;

/**
 * A {@link Configurer} for configuring {@link Configurable} components
 * (Is that as redundant as it sounds?)
 * The invoking class must provide an array of Configurers,
 * one for each attribute of the target Configurable object.
 * It is usually easier for the target to implement AutoConfigurable
 * and use the AutoConfigurer class.
 */
public class SimpleConfigurer extends Configurer
                              implements PropertyChangeListener {
  private javax.swing.JPanel p;
  private Configurer attConfig[];
  private Configurable target;

  public SimpleConfigurer(Configurable c, Configurer attConfigurers[]) {
    super(null,c.getConfigureName());

    attConfig = attConfigurers;
    target = c;
    setValue(target);
    target.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
          setName((String) evt.getNewValue());
        }
      }
    });
  }

  public String getValueString() {
    return target.getConfigureName();
  }

  public void setValue(String s) {
    throw new UnsupportedOperationException(
      "Can't set Configurable from String");
  }

  public Component getControls() {
    if (p == null) {
      p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
      for (int i = 0; i < attConfig.length; ++i) {
        p.add(attConfig[i].getControls());
        // attConfig[i].addPropertyChangeListener(this);
      }
    }
    return p;
  }

  public void propertyChange(final PropertyChangeEvent p1) {
    for (int i = 0; i < attConfig.length; ++i) {
      // System.err.println(attConfig[i].getName()+" = "+attConfig[i].getValue());
      if (attConfig[i].getValue() == null) {
        setValue((Object)null);
        return;
      }
      setValue(target);
    }
  }
}
