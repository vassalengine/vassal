/*
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

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.LayoutManager;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;

import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.Widget;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

/**
 * A Widget that corresponds to a JPanel with a
 * GridLayout or BoxLayout layout.  Adding a Widget to a
 * PanelWidget adds the child Widget's component to the JPanel.  The
 * attributes of the PanelWidget determine the layout parameters
 */
public class PanelWidget extends Widget {
  private JPanel panel;
  private final List<Widget> widgets = new ArrayList<>();
  private int nColumns = 3;
  private boolean vertical = false;
  private boolean fixed = false;
  protected double scale;
  public static final String SCALE = "scale"; //$NON-NLS-1$

  public PanelWidget() {
    scale = 1.0;
  }

  @Override
  public boolean hasScale() {
    return true;
  }

  @Override
  public double getScale() {
    return scale;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PanelWidget.component_type");
  }

  @Override
  public Component getComponent() {
    if (panel == null) {
      rebuild();
      panel = new JPanel();
      panel.setLayout(getLayout(panel));
      for (final Widget w : widgets) {
        final Component c = w.getComponent();
        if (c instanceof JComponent) {
          ((JComponent) c).setAlignmentX(0.0F);
          ((JComponent) c).setAlignmentY(0.0F);
        }

        if (w instanceof PieceSlot) {
          // prevents grey rectangles when number of
          // pieces is not a multiple of the row length
          panel.setBackground(Color.WHITE);
        }

        panel.add(c);
      }
    }
    return panel;
  }

  private LayoutManager getLayout(Container c) {
    if (fixed) {
      return new GridLayout(0, nColumns);
    }
    else {
      return new BoxLayout(c, vertical ? BoxLayout.Y_AXIS : BoxLayout.X_AXIS);
    }
  }

  @Override
  public void add(Buildable b) {
    if (b instanceof Widget) {
      final Widget w = (Widget) b;
      widgets.add(w);
      if (panel != null) {
        final Component c = w.getComponent();
        if (c instanceof JComponent) {
          ((JComponent) c).setAlignmentX(0.0F);
          ((JComponent) c).setAlignmentY(0.0F);
        }

        panel.add(c);
        panel.revalidate();
      }
    }
    super.add(b);
  }

  @Override
  public void remove(Buildable b) {
    if (b instanceof Widget) {
      final Widget w = (Widget) b;
      if (panel != null) {
        panel.remove(w.getComponent());
      }
      widgets.remove(w);
    }
    super.remove(b);
  }

  public static final String FIXED = "fixed"; //NON-NLS
  public static final String COLS = "nColumns"; //NON-NLS
  public static final String VERTICAL = "vert"; //NON-NLS

  /**
   * The attributes of a PanelWidget are:
   * <code>NAME</code> for the name of the Widget
   * <code>FIXED</code> uses GridLayout if <code>true</code>.  Otherwise uses BoxLayout
   * <code>COLS</code> for the number of columns.  Ignored unless FIXED is true
   * <code>VERTICAL</code> Uses a vertical BoxLayout if <code>true</code>.  otherwise uses a horizontal layout.  Ignored unless FIXED is false
   */
  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      DESCRIPTION,
      FIXED,
      COLS,
      VERTICAL,
      SCALE,
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.name_label"),
      Resources.getString(Resources.DESCRIPTION),
      Resources.getString("Editor.PanelWidget.fixed_cell_size"),
      Resources.getString("Editor.PanelWidget.number_of_columns"),
      Resources.getString("Editor.PanelWidget.vertical_layout"),
      Resources.getString("Editor.PanelWidget.scale")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      Boolean.class,
      Integer.class,
      Boolean.class,
      Double.class
    };
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLS.equals(name)) {
      return () -> fixed;
    }
    else if (VERTICAL.equals(name)) {
      return () -> !fixed;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  @Override
  public void setAttribute(String name, Object value) {
    if (NAME.equals(name)) {
      setConfigureName((String) value);
    }
    else if (FIXED.equals(name)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      fixed = (Boolean) value;
    }
    else if (COLS.equals(name)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }

      nColumns = (Integer) value;

      if (nColumns < 1) {
        // FIXME: also dialog should not permit values < 1 to be entered
        ErrorDialog.dataWarning(
          new BadDataReport("Panel has &lt; 1 column:", getConfigureName()));  //NON-NLS

        nColumns = 1;
      }
    }
    else if (VERTICAL.equals(name)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      vertical = (Boolean) value;
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

    if (panel != null) {
      panel.setLayout(getLayout(panel));
      panel.revalidate();
    }
  }

  @Override
  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else if (FIXED.equals(name)) {
      return String.valueOf(fixed);
    }
    else if (COLS.equals(name)) {
      return String.valueOf(nColumns);
    }
    else if (VERTICAL.equals(name)) {
      return String.valueOf(vertical);
    }
    else if (SCALE.equals(name)) {
      return String.valueOf(scale);
    }
    else if (DESCRIPTION.equals(name)) {
      return description;
    }
    return null;
  }
}

