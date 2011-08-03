/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.build.module.gamepieceimage;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.event.ItemListener;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JTable;
import javax.swing.ListCellRenderer;
import javax.swing.table.TableCellRenderer;

import VASSAL.tools.image.ImageUtils;

public class SwatchComboBox extends JComboBox {
  private static final long serialVersionUID = 1L;

  public SwatchComboBox() {
    String[] s = ColorManager.getColorManager().getColorNames();
    for (int i = 0; i < s.length; ++i) {
      addItem(s[i]);
    }
    SwatchRenderer renderer = new SwatchRenderer();
    setRenderer(renderer);
  }

  public SwatchComboBox(ItemListener l) {
    this();
    addItemListener(l);
  }

  public SwatchComboBox(ItemListener l, String colorName) {
    this();
    setSelectedItem(colorName);
    addItemListener(l);
  }

  public class SwatchRenderer extends JLabel implements ListCellRenderer {
    private static final long serialVersionUID = 1L;

    public SwatchRenderer() {
      setOpaque(true);
      setHorizontalAlignment(LEFT);
      setVerticalAlignment(CENTER);
    }

    /*
     * This method finds the image and text corresponding to the selected
     * value and returns the label, set up to display the text and image.
     */
    public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected,
        boolean cellHasFocus) {

      ColorSwatch swatch = ColorManager.getColorManager().getColorSwatch((String) value);

      if (isSelected) {
        setBackground(list.getSelectionBackground());
        setForeground(list.getSelectionForeground());
      }
      else {
        setBackground(list.getBackground());
        setForeground(list.getForeground());
      }

      //Set the icon and text. If icon was null, say so.
      //String name = (String) list.get
      final BufferedImage img = ImageUtils.createCompatibleImage(25, 12);
      final Graphics2D g = img.createGraphics();
      g.setColor(swatch.getColor());
      g.fillRect(0, 0, 25, 12);
      g.setColor(Color.black);
      g.drawRect(0, 0, 24, 11);
      g.dispose();

      setIcon(new ImageIcon(img));
      setText((String) value);
      setFont(list.getFont());

      return this;
    }
  }

  class SwatchTableRenderer extends JLabel implements TableCellRenderer  {
    private static final long serialVersionUID = 1L;

    public SwatchTableRenderer() {
      setOpaque(true);
      setHorizontalAlignment(LEFT);
      setVerticalAlignment(CENTER);
    }

    /*
     * This method finds the image and text corresponding to the selected
     * value and returns the label, set up to display the text and image.
     */
    public Component getTableCellRendererComponent(
        JTable table, Object value,
        boolean isSelected, boolean hasFocus,
        int row, int column) {

      ColorSwatch swatch = (ColorSwatch) value;

      if (isSelected) {
        setBackground(table.getSelectionBackground());
        setForeground(table.getSelectionForeground());
      }
      else {
        setBackground(table.getBackground());
        setForeground(table.getForeground());
      }

      //Set the icon and text. If icon was null, say so.
      //String name = (String) list.get
      final BufferedImage img = ImageUtils.createCompatibleImage(25, 12);
      final Graphics2D g = img.createGraphics();
      g.setColor(swatch.getColor());
      g.fillRect(0, 0, 25, 12);
      g.setColor(Color.black);
      g.drawRect(0, 0, 24, 11);
      g.dispose();

      setIcon(new ImageIcon(img));
      setText(swatch.getConfigureName());
      setFont(table.getFont());

      return this;
    }
  }
}
