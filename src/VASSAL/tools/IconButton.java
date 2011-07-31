package VASSAL.tools;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

import javax.swing.ImageIcon;
import javax.swing.JButton;

import VASSAL.tools.image.ImageUtils;

public class IconButton extends JButton {
  private static final long serialVersionUID = 1L;

  public static final int PLUS_ICON = 0;
  public static final int MINUS_ICON = 1;
  public static final int TICK_ICON = 2;
  public static final int CROSS_ICON = 3;

  public IconButton(int type) {
    this(type, 22);
  }

  public IconButton(int type, int size) {
    this(type, size, getDefaultColor(type), 2.0f);
  }

  public static Color getDefaultColor(int type) {
    switch (type) {
    case TICK_ICON:
      return Color.green;
    case CROSS_ICON:
      return Color.red;
    default:
      return Color.black;
    }
  }

  public IconButton(int type, int size, Color color, float width) {
    super();

    setMinimumSize(new Dimension(size, size));
    setPreferredSize(new Dimension(size, size));

    final BufferedImage image =
      ImageUtils.createCompatibleTranslucentImage(size, size);

    final Graphics2D g = image.createGraphics();
    g.setStroke(new BasicStroke(width));
    g.setColor(color);

    switch (type) {
    case PLUS_ICON:
      g.drawLine(5, size / 2, size - 5, size / 2);
      g.drawLine(size / 2, 5, size / 2, size - 5);
      break;
    case MINUS_ICON:
      g.drawLine(5, size / 2, size - 5, size / 2);
      break;
    case TICK_ICON:
      g.drawLine(5, size/2, size/2, size-5);
      g.drawLine(size/2, size-5, 5, size-5);
      break;
    case CROSS_ICON:
      g.drawLine(5, 5, size-5, size-5);
      g.drawLine(5, size-5, size-5, 5);
      break;
    }
    setIcon(new ImageIcon(image));
  }
}
