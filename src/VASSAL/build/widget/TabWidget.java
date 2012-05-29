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
package VASSAL.build.widget;

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.Widget;

/**
 * A Widget that corresponds to a JTabbedPane.
 * Adding a Widget to a BoxWidget adds the child Widget's component
 * to the JTabbedPane, setting the tab's name to the child's name
 * (via {@link Configurable#getConfigureName})
 */
public class TabWidget extends Widget
    implements ChangeListener, PropertyChangeListener {
  private JTabbedPane tab = null;
  private List<Widget> widgets = new ArrayList<Widget>();

  public TabWidget() { }

  public static String getConfigureTypeName() {
    return "Tabbed Panel";
  }

  public void stateChanged(ChangeEvent e) {
    int index = tab.getSelectedIndex();
    if (index >= 0) {
      tab.setComponentAt(index, widgets.get(index).getComponent());
    }
  }

  public void add(Buildable b) {
    if (b instanceof Widget) {
      final Widget w = (Widget) b;
      widgets.add(w);
      if (tab != null) {
        tab.removeChangeListener(this);
        if (widgets.size() > 1) {
          tab.addTab(w.getConfigureName(), new JPanel());
        }
        else {
          tab.addTab(w.getConfigureName(), w.getComponent());
        }
        w.addPropertyChangeListener(this);
        tab.addChangeListener(this);
      }
    }
    super.add(b);
  }

  public void remove(Buildable b) {
    if (b instanceof Widget) {
      final Widget w = (Widget) b;
      if (tab != null) {
        tab.removeChangeListener(this);   // prevent bad recursion
        tab.removeTabAt(widgets.indexOf(w));
        w.removePropertyChangeListener(this);
        tab.addChangeListener(this);      // restore listener
      }
      widgets.remove(w);
    }
    super.remove(b);
  }

  public void propertyChange(PropertyChangeEvent evt) {
    if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
      final Widget src = (Widget) evt.getSource();
      final int index = widgets.indexOf(src);
      final Object name = evt.getNewValue();
      tab.setTitleAt(index, name == null ? "" : name.toString());
    }
  }

  public Component getComponent() {
    if (tab == null) {
      rebuild();
      tab = new JTabbedPane();
      for (int i = 0; i < widgets.size(); ++i) {
        final Widget w = widgets.get(i);
        w.addPropertyChangeListener(this);
        tab.addTab(w.getConfigureName(), new JPanel());
      }
      tab.addChangeListener(this);
      if (widgets.size() > 0) {
        tab.setSelectedIndex(0);
      }
      stateChanged(null);
    }
    return tab;
  }

  public String[] getAttributeNames() {
    return new String[]{NAME};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name:  "};
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class};
  }

  public void setAttribute(String name, Object value) {
    if (NAME.equals(name)) {
      setConfigureName((String) value);
    }
  }

  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    return null;
  }

}
