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

import java.awt.Component;
import java.awt.Dimension;
import java.awt.Image;
import java.io.File;
import java.util.Collection;

import javax.swing.JLabel;
import javax.swing.JScrollPane;

import VASSAL.build.Buildable;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.DataArchive;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OpIcon;
import VASSAL.tools.imageop.SourceOp;

/**
 * A Chart is used for displaying charts and tables for the module.
 * The charts are loaded as images stored in the DataArchive. As a subclass
 * of Widget, a Chart may be added to any Widget, but it may not contain
 * children of its own.
 */
public class Chart extends Widget {
  public static final String NAME = "chartName"; //NON-NLS
  public static final String FILE = "fileName"; //NON-NLS
  private Component chart;
  private String fileName;
  private SourceOp srcOp;
  private JLabel label;

  @Override
  public Component getComponent() {
    if (chart == null) {
      label = new JLabel();
      srcOp = (fileName == null || fileName.isBlank()) ? null : Op.load(fileName);
      if (srcOp != null) {
        label.setIcon(new OpIcon(srcOp));
      }
      final Dimension d = label.getPreferredSize();
      if (d.width > 300 || d.height > 300) {
        final JScrollPane scroll = new AdjustableSpeedScrollPane(label);
        scroll.getViewport().setPreferredSize(label.getPreferredSize());
        scroll.getViewport().setAlignmentY(0.0F);
        chart = scroll;
      }
      else {
        chart = label;
      }
    }
    return chart;
  }

  public String getFileName() {
    return fileName;
  }

  @Override
  public void addTo(Buildable parent) {
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChartWindow.html", "Chart"); //NON-NLS
  }

  @Override
  public void setAttribute(String key, Object val) {
    if (NAME.equals(key)) {
      setConfigureName((String) val);
    }
    else if (FILE.equals(key)) {
      if (val instanceof File) {
        val = ((File) val).getName();
      }
      fileName = (String) val;
      if (label != null) {
        srcOp = (fileName == null || fileName.isBlank()) ? null : Op.load(fileName);
        if (srcOp != null) {
          label.setIcon(new OpIcon(srcOp));
          label.revalidate();
        }
      }
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String)val;
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  /**
   * The Attributes of a Chart are:
   *
   * <pre>
   * <code>
   * NAME
   * </code>
   *  for the name of the chart
   * <code>
   * FILE
   * </code>
   *  for the name of the image in the {@link DataArchive}
   * </pre>
   */
  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, DESCRIPTION, FILE};
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{Resources.getString("Editor.name_label"), Resources.getString(Resources.DESCRIPTION), Resources.getString("Editor.image_label")};
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, String.class, Image.class};
  }

  @Override
  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else if (FILE.equals(name)) {
      return fileName;
    }
    else if (DESCRIPTION.equals(name)) {
      return description;
    }
    return null;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Chart.component_type");
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (fileName != null) s.add(fileName);
  }
}
