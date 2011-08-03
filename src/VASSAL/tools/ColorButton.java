/*
 * $Id$
 *
 * Copyright (c) 2008 by Joel Uckelman
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Insets;

import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.UIManager;

/**
 * A {@link JButton} which displays a color swatch.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ColorButton extends JButton {
  private static final long serialVersionUID = 1L;

  private static Font FONT = new Font("Dialog", 0, 10);

  private Color color;

  /**
   * Create a button with no color.
   */
  public ColorButton() {
    this(null);
  }

 /**
  * Create a button with the specified color.
  *
  * @param c the color to set
  */
  public ColorButton(Color c) {
    super();
    color = c;
    setIcon(new SwatchIcon(30,15));
    setMargin(new Insets(2,2,2,2));
  }

  /**
   * Set the color of the button.
   *
   * @param c the color to set
   */
  public void setColor(Color c) {
    color = c;
  }

  private class SwatchIcon implements Icon {
    private final int swatchWidth;
    private final int swatchHeight;

    public SwatchIcon(int width, int height) {
      swatchWidth = width;
      swatchHeight = height;
    }

    public void paintIcon(Component c, Graphics g, int x, int y) {
      g.setColor(Color.black);
      g.drawRect(x, y, swatchWidth-1, swatchHeight-1);

      if (color != null) {
        g.setColor(color);
        g.fillRect(x+1, y+1, swatchWidth-2, swatchHeight-2);
      }
      else {
        // paint no color and a "nil" if the color is null
        g.setColor(UIManager.getColor("controlText"));
        g.setFont(FONT);
        g.drawString("nil",
          x+(swatchWidth - g.getFontMetrics(FONT).stringWidth("nil"))/ 2,
          y+(swatchHeight + g.getFontMetrics(FONT).getAscent())/2);
      }
    }

    public int getIconWidth() {
      return swatchWidth;
    }

    public int getIconHeight() {
      return swatchHeight;
    }
  }
}
