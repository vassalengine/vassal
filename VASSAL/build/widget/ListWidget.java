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
import java.awt.Component;
import java.util.Enumeration;
import java.util.Hashtable;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.Widget;
import VASSAL.tools.ScrollPane;

/**
 * A Widget that corresponds to a panel with a {@link JList} sitting
 * to the right of a {@link JPanel} with a {@link CardLayout} layout.
 * Adding a Widget to a ListWidget adds the child Widget's component to
 * the JPanel and add's the child's name (via {@link
 * Configurable#getConfigureName}) to the JList.  Changing the
 * selection of the JList shows the corresponding child's
 * component */
public class ListWidget extends Widget
    implements ListSelectionListener {
  private JPanel panel;
  private JSplitPane split;
  private JList list;
  private DefaultListModel widgets = new DefaultListModel();
  private CardLayout layout;
  private JPanel multiPanel;

  private Hashtable keys = new Hashtable();
  private int count = 0;

  public ListWidget() {
  }

  public static String getConfigureTypeName() {
    return "Scrollable List";
  }

  public Component getComponent() {
    if (panel == null) {
      rebuild();
      panel = new JPanel();
      split = new JSplitPane();
      split.setResizeWeight(1.0);
      list = new JList(widgets);
      layout = new CardLayout();
      multiPanel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      multiPanel.setLayout(layout);

      for (Enumeration e = getBuildComponents();
           e.hasMoreElements();) {
        Widget w = (Widget) e.nextElement();
        multiPanel.add(getKey(w), w.getComponent());
      }

      list.setModel(widgets);
      list.addListSelectionListener(this);
      list.setCellRenderer(new Widget.MyCellRenderer());
      list.setFixedCellWidth(80);
      list.setVisibleRowCount(3);
      split.setLeftComponent(multiPanel);
      split.setRightComponent(new ScrollPane(list));
    }
    return split;
  }

  public void add(Buildable b) {
    if (b instanceof Widget) {
      Widget w = (Widget) b;
      widgets.addElement(w);
      if (panel != null) {
        multiPanel.add(getKey(w), w.getComponent());
        list.revalidate();
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

  private String getKey(Object o) {
    String s = (String) keys.get(o);
    if (s == null) {
      s = "" + new Integer(count++);
      keys.put(o, s);
    }
    return s;
  }

  public void valueChanged(ListSelectionEvent e) {
    Object selected = list.getSelectedValue();
    if (selected != null) {
      layout.show(multiPanel, getKey(selected));
    }
  }

  public String[] getAttributeNames() {
    String s[] = {NAME,WIDTH,HEIGHT};
    return s;
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name"};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class};
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
