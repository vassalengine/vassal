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

import VASSAL.build.Buildable;
import VASSAL.build.Widget;
import VASSAL.build.module.PieceWindow;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.ScrollPane;

import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import java.awt.CardLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.util.HashMap;
import java.util.Map;

/**
 * A Widget that corresponds to a panel with a {@link JList} sitting
 * to the right of a {@link JPanel} with a {@link CardLayout} layout.
 * Adding a Widget to a ListWidget adds the child Widget's component to
 * the JPanel and adds the child's name (via {@link
 * Configurable#getConfigureName}) to the JList.  Changing the
 * selection of the JList shows the corresponding child's
 * component */
public class ListWidget extends Widget
    implements ListSelectionListener {
  private JPanel panel;
  private JSplitPane split;
  private JList<Widget> list;
  private final DefaultListModel<Widget> widgets = new DefaultListModel<>();
  private CardLayout layout;
  private JPanel multiPanel;
  private int width, height, divider;
  private static final String DIVIDER = "divider"; //NON-NLS
  public static final String SCALE = "scale"; //$NON-NLS-1$
  protected double scale;

  private final Map<Object, String> keys = new HashMap<>();
  private int count = 0;

  public ListWidget() {
    scale = 1.0;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ListWidget.component_type");
  }

  @Override
  public boolean hasScale() {
    return true;
  }

  @Override
  public double getScale() {
    return scale;
  }

  // True if we're part of main (docked) Piece Palette
  protected boolean isMainPiecePalette() {
    Widget w = this;
    while ((w = w.getParent()) != null) {
      if (w instanceof PieceWindow) {
        return ((PieceWindow)w).shouldDockIntoMainWindow();
      }
    }
    return false;
  }


  @Override
  public Component getComponent() {
    if (panel == null) {
      rebuild();
      panel = new JPanel();
      split = new JSplitPane();
      split.setResizeWeight(1.0);
      list = new JList<>(widgets);
      layout = new CardLayout();
      multiPanel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
      multiPanel.setLayout(layout);

      for (final Buildable b : getBuildables()) {
        final Widget w = (Widget) b;
        multiPanel.add(getKey(w), w.getComponent());
      }

      list.setModel(widgets);
      list.addListSelectionListener(this);
      list.setCellRenderer(new Widget.MyCellRenderer());
      list.setFixedCellWidth(80);
      list.setVisibleRowCount(3);
      split.setLeftComponent(multiPanel);
      split.setRightComponent(new ScrollPane(list));

      if (width > 0 && height > 0) {
        // This was causing bad behavior in piece palettes - Jlist aggressively grabs space. This lets the size of the rest of the Piece Palette and/or Chatter govern.
        final int grabWidth  = isMainPiecePalette() ? width / 2 : width;
        final int grabHeight = isMainPiecePalette() ? height / 2 : height;
        split.setPreferredSize(new Dimension(grabWidth, grabHeight));
      }
      if (divider > 0) {
        split.setDividerLocation(divider);
      }
    }
    return split;
  }

  @Override
  public void add(Buildable b) {
    if (b instanceof Widget) {
      final Widget w = (Widget) b;
      widgets.addElement(w);
      if (panel != null) {
        multiPanel.add(getKey(w), w.getComponent());
        list.revalidate();
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

  private String getKey(Object o) {
    String s = keys.get(o);
    if (s == null) {
      s = String.valueOf(count++);
      keys.put(o, s);
    }
    return s;
  }

  @Override
  public void valueChanged(ListSelectionEvent e) {
    final Object selected = list.getSelectedValue();
    if (selected != null) {
      layout.show(multiPanel, getKey(selected));
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {NAME, DESCRIPTION, SCALE, WIDTH, HEIGHT, DIVIDER};
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{Resources.getString("Editor.name_label"), Resources.getString(Resources.DESCRIPTION), Resources.getString("Editor.ListWidget.image_scale")};
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, String.class, Double.class};
  }

  @Override
  public void setAttribute(String name, Object value) {
    if (NAME.equals(name)) {
      setConfigureName((String) value);
    }
    else if (WIDTH.equals(name)) {
      width = Integer.parseInt((String)value);
    }
    else if (HEIGHT.equals(name)) {
      height = Integer.parseInt((String)value);
    }
    else if (DIVIDER.equals(name)) {
      divider = Integer.parseInt((String)value);
    }
    else if (SCALE.equals(name)) {
      if (value instanceof String) {
        value = Double.valueOf((String) value);
      }
      scale = (Double) value;
      if (scale < 0.01) { //BR// Just gonna go with some sanity.
        scale = 0.01;
      }
      else if (scale >= 4) {
        scale = 4.0;
      }
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
      return String.valueOf(split == null ? width : split.getWidth());
    }
    else if (HEIGHT.equals(name)) {
      return String.valueOf(split == null ? height : split.getHeight());
    }
    else if (DIVIDER.equals(name)) {
      return String.valueOf(split == null ? divider : split.getDividerLocation());
    }
    else if (SCALE.equals(name)) {
      return String.valueOf(scale);
    }
    else if (DESCRIPTION.equals(name)) {
      return description;
    }
    return null;
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (SCALE.equals(name)) {
      return this::hasScalablePieces;
    }
    return super.getAttributeVisibility(name);
  }
}
