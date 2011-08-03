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

import java.awt.CardLayout;
import java.awt.Dimension;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import javax.swing.JPanel;

import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.Widget;

/**
 * A Widget that corresponds to a panel with a {@link JComboBox} above
 * a {@link JPanel} with a {@link CardLayout} layout.
 * Adding a Widget to a BoxWidget adds the child Widget's component
 * to the JPanel and add's the child's name
 * (via {@link Configurable#getConfigureName}) to the JComboBox.  Changing
 * the selection of the JComboBox shows the corresponding child's component
 */
public class BoxWidget extends Widget
    implements ItemListener, PropertyChangeListener {
  private JPanel panel;
  private JComboBox box;
  private DefaultComboBoxModel widgets = new DefaultComboBoxModel();
  private CardLayout layout = new CardLayout();
  private JPanel multiPanel = new JPanel();
  private List<Widget> built = new ArrayList<Widget>();
  private Dimension size = new Dimension();

  private Map<Object,String> keys = new HashMap<Object,String>();
  private int count = 0;

  public BoxWidget() {
  }

  public static String getConfigureTypeName() {
    return "Pull-down Menu";
  }

  public void add(Buildable b) {
    if (b instanceof Widget) {
      Widget w = (Widget) b;
      widgets.addElement(w);
      w.addPropertyChangeListener(this);
      //      w.setAllowableConfigureComponents(getAllowableConfigureComponents());
      if (panel != null) {
        multiPanel.add(getKey(w), w.getComponent());
      }
    }
    super.add(b);
  }

  public void remove(Buildable b) {
    if (b instanceof Widget) {
      widgets.removeElement(b);
    }
    super.remove(b);
  }

  public void propertyChange
      (java.beans.PropertyChangeEvent evt) {
    if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())
        && box != null) {
      box.revalidate();
    }
  }

  public java.awt.Component getComponent() {
    if (panel == null) {
      rebuild();
      box = new JComboBox();
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      multiPanel.setLayout(layout);
      if (size.width > 0
        && size.height > 0) {
        multiPanel.setPreferredSize(size);
      }

      box.setModel(widgets);
      box.setRenderer(new Widget.MyCellRenderer());
      box.addItemListener(this);
      panel.add(box);
      panel.add(multiPanel);
      itemStateChanged(null);
    }
    return panel;
  }

  private String getKey(Object o) {
    String s = keys.get(o);
    if (s == null) {
      s = String.valueOf(count++);
      keys.put(o, s);
    }
    return s;
  }

  public void itemStateChanged(ItemEvent e) {
    if (box.getSelectedItem() != null) {
      Widget w = (Widget) box.getSelectedItem();
      if (!built.contains(w)) {
        multiPanel.add(getKey(w), w.getComponent());
        built.add(w);
      }
      layout.show(multiPanel, getKey(w));
    }
  }

  private boolean allChildrenBuilt() {
    return box != null && box.getModel().getSize() == built.size();
  }

  public String[] getAttributeNames() {
    return new String[]{NAME,WIDTH,HEIGHT};
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
    else if (WIDTH.equals(name)) {
      size.width = Integer.parseInt(value.toString());
    }
    else if (HEIGHT.equals(name)) {
      size.height = Integer.parseInt(value.toString());
    }
  }

  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else if (WIDTH.equals(name)) {
      final int w = allChildrenBuilt() ?
        multiPanel.getLayout().preferredLayoutSize(multiPanel).width :
        size.width;
      return String.valueOf(w);
    }
    else if (HEIGHT.equals(name)) {
      final int h = allChildrenBuilt() ?
        multiPanel.getLayout().preferredLayoutSize(multiPanel).height :
        size.height;
      return String.valueOf(h);
    }
    return null;
  }
}
