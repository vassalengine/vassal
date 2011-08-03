/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package VASSAL.build.module.gamepieceimage;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.event.ItemListener;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.SwingConstants;

import VASSAL.tools.image.ImageUtils;

public class SymbolConfigurer extends StringEnumConfigurer {

  public SymbolConfigurer(String key, String name) {
    super(key, name, Symbol.NatoUnitSymbolSet.getSymbolNames());
  }

  public JComboBox getComboBox() {
    return (JComboBox) new SymbolComboBox();
  }

  public class SymbolComboBox extends JComboBox {
    private static final long serialVersionUID = 1L;

    static final int sample_w = 20;
    static final int sample_h = 13;

    public SymbolComboBox() {
      String[] s = Symbol.NatoUnitSymbolSet.getSymbolNames();
      for (int i = 0; i < s.length; ++i) {
        addItem(s[i]);
      }
      SymbolRenderer renderer = new SymbolRenderer();
      setRenderer(renderer);
    }

    public SymbolComboBox(ItemListener l) {
      this();
      addItemListener(l);
    }

    public SymbolComboBox(ItemListener l, String symbolName) {
      this();
      setSelectedItem(symbolName);
      addItemListener(l);
    }

    public class SymbolRenderer extends JLabel implements ListCellRenderer {
      private static final long serialVersionUID = 1L;

      public SymbolRenderer() {
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

        if (isSelected) {
          setBackground(list.getSelectionBackground());
          setForeground(list.getSelectionForeground());
        }
        else {
          setBackground(list.getBackground());
          setForeground(list.getForeground());
        }

        final BufferedImage img =
          ImageUtils.createCompatibleTranslucentImage(sample_w, sample_h);
        final Graphics2D g = img.createGraphics();

        final String symbol1 = (String) value;
        final String symbol2 = Symbol.NatoUnitSymbolSet.NONE;
        final Rectangle bounds = new Rectangle(0, 0, sample_w-1, sample_h-1);
        Symbol.NatoUnitSymbolSet.draw(
          symbol1, symbol2, g, bounds,
          Color.BLACK, Color.WHITE, Color.BLACK, 1.0f, ""); //$NON-NLS-1$
        g.dispose();

        setIcon(new ImageIcon(img));
        setText((String) value);
        this.setHorizontalTextPosition(SwingConstants.LEFT);
        this.setHorizontalAlignment(SwingConstants.RIGHT);
        setFont(list.getFont());

        return this;
      }
    }
  }
}
