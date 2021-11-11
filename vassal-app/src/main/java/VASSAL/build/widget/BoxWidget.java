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
import VASSAL.i18n.Resources;

/**
 * A Widget that corresponds to a panel with a {@link JComboBox} above
 * a {@link JPanel} with a {@link CardLayout} layout.
 * Adding a Widget to a BoxWidget adds the child Widget's component
 * to the JPanel and adds the child's name
 * (via {@link Configurable#getConfigureName}) to the JComboBox.  Changing
 * the selection of the JComboBox shows the corresponding child's component
 */
public class BoxWidget extends Widget
    implements ItemListener, PropertyChangeListener {
  private JPanel panel;
  private JComboBox<Widget> box;
  private final DefaultComboBoxModel<Widget> widgets = new DefaultComboBoxModel<>();
  private final CardLayout layout = new CardLayout();
  private final JPanel multiPanel = new JPanel();
  private final List<Widget> built = new ArrayList<>();
  private final Dimension size = new Dimension();

  private final Map<Object, String> keys = new HashMap<>();
  private int count = 0;

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.BoxWidget.component_type");
  }

  @Override
  public void add(Buildable b) {
    if (b instanceof Widget) {
      final Widget w = (Widget) b;
      widgets.addElement(w);
      w.addPropertyChangeListener(this);
      //      w.setAllowableConfigureComponents(getAllowableConfigureComponents());
      if (panel != null) {
        multiPanel.add(getKey(w), w.getComponent());
      }
    }
    super.add(b);
  }

  @Override
  public void remove(Buildable b) {
    if (b instanceof Widget) {
      widgets.removeElement(b);
    }
    super.remove(b);
  }

  @Override
  public void propertyChange(java.beans.PropertyChangeEvent evt) {
    if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())
        && box != null) {
      box.revalidate();
    }
  }

  @Override
  public java.awt.Component getComponent() {
    if (panel == null) {
      rebuild();
      box = new JComboBox<>();
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      multiPanel.setLayout(layout);
      if (size.width > 0 && size.height > 0) {
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

  @Override
  public void itemStateChanged(ItemEvent e) {
    if (box.getSelectedItem() != null) {
      final Widget w = (Widget) box.getSelectedItem();
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

  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, DESCRIPTION, WIDTH, HEIGHT};
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{Resources.getString("Editor.name_label", Resources.getString(Resources.DESCRIPTION))};
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, String.class};
  }

  @Override
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
    else if (DESCRIPTION.equals(name)) {
      description = (String)value;
    }
  }

  @Override
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
    else if (DESCRIPTION.equals(name)) {
      return description;
    }
    return null;
  }
}
