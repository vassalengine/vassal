/*
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

import VASSAL.configure.TranslatableStringEnum;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;

public class GamePieceLayout extends AbstractConfigurable implements Visualizable {

  protected static final String NAME = "name"; //$NON-NLS-1$
  protected static final String WIDTH = "width"; //$NON-NLS-1$
  protected static final String HEIGHT = "height"; //$NON-NLS-1$
  protected static final String BORDER = "border"; //$NON-NLS-1$
  protected static final String ITEMS = "layout"; //$NON-NLS-1$

  protected static final String N = "Top"; //NON-NLS
  protected static final String S = "Bottom"; //NON-NLS
  protected static final String E = "Right"; //NON-NLS
  protected static final String W = "Left"; //NON-NLS
  protected static final String NE = "Top Right"; //NON-NLS
  protected static final String NW = "Top Left"; //NON-NLS
  protected static final String SE = "Bottom Right"; //NON-NLS
  protected static final String SW = "Bottom Left"; //NON-NLS
  protected static final String CENTER = "Center"; //NON-NLS

  protected static final int POS_L = 15;
  protected static final int POS_R = 85;
  protected static final int POS_T = 15;
  protected static final int POS_B = 85;
  protected static final int POS_C = 50;

  protected static final String BORDER_PLAIN = "Plain"; //NON-NLS
  protected static final String BORDER_FANCY = "Fancy"; //NON-NLS
  protected static final String BORDER_3D = "3D"; //NON-NLS
  protected static final String BORDER_NONE = "None"; //NON-NLS

  public static final String[] LOCATIONS = {CENTER, N, S, E, W, NE, NW, SE, SW};
  public static final String[] LOCATION_I18N_KEYS = {
    "Editor.center",
    "Editor.top",
    "Editor.bottom",
    "Editor.right",
    "Editor.left",
    "Editor.top_right",
    "Editor.top_left",
    "Editor.bottom_right",
    "Editor.bottom_left"
  };

  public static String getDisplayLocation(String location) {
    for (int i = 0; i < LOCATIONS.length; i++) {
      if (LOCATIONS[i].equals(location)) {
        return Resources.getString(LOCATION_I18N_KEYS[i]);
      }
    }
    return location;
  }

  public static final int[] X_POS = {POS_C, POS_C, POS_C, POS_R, POS_L, POS_R, POS_L, POS_R, POS_L};
  public static final int[] Y_POS = {POS_C, POS_T, POS_B, POS_C, POS_C, POS_T, POS_T, POS_B, POS_B};
  protected static final Map<String, String> compass = new HashMap<>();
  static {
    compass.put(N, "N"); //NON-NLS
    compass.put(S, "S"); //NON-NLS
    compass.put(E, "E"); //NON-NLS
    compass.put(W, "W"); //NON-NLS
    compass.put(NE, "NE"); //NON-NLS
    compass.put(NW, "NW"); //NON-NLS
    compass.put(SE, "SE"); //NON-NLS
    compass.put(SW, "SW"); //NON-NLS
    compass.put(CENTER, "CENTER"); //NON-NLS
  }

  public static String getCompassPoint(String location) {
    return compass.get(location);
  }

  public Point getPosition(Item item) {
    final String s = getCompassPoint(item.getLocation());

    final int x;
    final int y;
    final Dimension d = item.getSize();
    switch (s.charAt(s.length() - 1)) {
    case 'E':
      x = getLayoutWidth() - d.width;
      break;
    case 'W':
      x = 0;
      break;
    default:
      x = getLayoutWidth() / 2 - d.width / 2;
    }
    switch (s.charAt(0)) {
    case 'N':
      y = 0;
      break;
    case 'S':
      y = getLayoutHeight() - d.height;
      break;
    default:
      y = getLayoutHeight() / 2 - d.height / 2;
    }

    return new Point(x + item.getXoffset(), y + item.getYoffset());
  }

  protected int width = 54;
  protected int height = 54;
  protected String border = BORDER_3D;
  protected GamePieceImage imageDefn;
  protected Image visImage;
  protected List<Item> items = new ArrayList<>();

  public GamePieceLayout() {
    super();
    name = ""; //$NON-NLS-1$
    imageDefn = new GamePieceImage(this);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.GamePieceLayout.counter_width"),
      Resources.getString("Editor.GamePieceLayout.counter_height"),
      Resources.getString("Editor.GamePieceLayout.counter_style"),
      ""  //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      Integer.class,
      Integer.class,
      BorderConfig.class,
      LayoutConfig.class
    };
  }

  public static class LayoutConfig implements ConfigurerFactory {
    protected static LayoutConfigurer configurer;

    @Override
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      configurer = new LayoutConfigurer(key, name, (GamePieceLayout) c);
      return configurer;
    }

    public static void refresh() {
      if (configurer != null) {
        configurer.repack();
      }
    }
  }

  public static class BorderConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{BORDER_PLAIN, BORDER_FANCY, BORDER_3D, BORDER_NONE};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.GamePieceLayout.plain_border",
        "Editor.GamePieceLayout.fancy_border",
        "Editor.GamePieceLayout.3D_border",
        "Editor.GamePieceLayout.no_border"
      };
    }
  }

  public boolean isColoredBorder() {
    return border.equals(BORDER_PLAIN) || border.equals(BORDER_FANCY);
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, WIDTH, HEIGHT, BORDER, ITEMS};
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (WIDTH.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      setWidth((Integer) value);
    }
    else if (HEIGHT.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      setHeight((Integer) value);
    }
    else if (BORDER.equals(key)) {
      border = (String) value;
    }
    else if (ITEMS.equals(key)) {
      decodeItemList((String) value);
    }
    invalidate();
    LayoutConfig.refresh();
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (WIDTH.equals(key)) {
      return Integer.toString(getLayoutWidth());
    }
    else if (HEIGHT.equals(key)) {
      return Integer.toString(getLayoutHeight());
    }
    else if (BORDER.equals(key)) {
      return border;
    }
    else if (ITEMS.equals(key)) {
      return encodeItemList();
    }
    else
      return null;
  }

  @Override
  public void removeFrom(Buildable parent) {

  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceLayouts.html"); //$NON-NLS-1$
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{GamePieceImage.class};
  }

  @Override
  public void addTo(Buildable parent) {
    setAllAttributesUntranslatable();
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GamePieceLayout.component_type"); //$NON-NLS-1$
  }

  public void setWidth(int width) {
    this.width = width;
  }

  public int getLayoutWidth() {
    return width;
  }

  public void setHeight(int height) {
    this.height = height;
  }

  public int getLayoutHeight() {
    return height;
  }

  public String getBorder() {
    return border;
  }

  public List<Item> getItems() {
    return items;
  }

  protected int getItemCount() {
    return items.size();
  }

  protected Item getItem(int n) {
    return items.get(n);
  }

  protected Item getItem(String name) {
    for (final Item item : items) {
      if (item.getConfigureName().equals(name)) {
        return item;
      }
    }
    return null;
  }

  protected void removeItem(int n) {
    items.remove(n);
  }

  public void addItem(Item i) {
    items.add(i);
  }

  public void moveItem(int from, int to) {
    final Item temp = items.get(to);
    items.set(to, items.get(from));
    items.set(from, temp);
  }

  @Override
  public Image getVisualizerImage() {
    if (visImage == null) {
      rebuildVisualizerImage();
    }
    return visImage;
  }

  public Image buildImage(GamePieceImage defn) {
    // Create our base image
    final BufferedImage image = ImageUtils.createCompatibleTranslucentImage(
      Math.max(width, 1),
      Math.max(height, 1)
    );
    final Graphics2D g = image.createGraphics();

    // Fill in the sample Background color
    final Color bgColor = defn.getBgColor().getColor();
    g.setColor(bgColor);

    if (getBorder().equals(BORDER_3D)) {
      if (bgColor != null) g.fill3DRect(0, 0, width, height, true);
    }
    else {
      if (bgColor != null) g.fillRect(0, 0, width, height);

      // Add Border
      if (getBorder().equals(BORDER_PLAIN) || getBorder().equals(BORDER_FANCY)) {
        final Color bg = bgColor == null ? Color.WHITE : bgColor;
        g.setColor(defn.getBorderColor().getColor());
        g.setStroke(new BasicStroke(1.0f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
        g.drawRect(0, 0, width - 1, height - 1);
        if (getBorder().equals(BORDER_FANCY)) {
          final Color lt = new Color(bg.getRed()   / 2,
                               bg.getGreen() / 2,
                               bg.getBlue()  / 2);
          final Color dk = new Color(bg.getRed()   + (255 - bg.getRed())   / 2,
                               bg.getGreen() + (255 - bg.getGreen()) / 2,
                               bg.getBlue()  + (255 - bg.getBlue())  / 2);
          g.setColor(dk);
          g.drawLine(1, 1, width - 3, 1);
          g.drawLine(1, 2, 1, height - 3);
          g.setColor(lt);
          g.drawLine(width - 2, 2, width - 2, height - 2);
          g.drawLine(2, height - 2, width - 3, height - 2);
        }
      }
    }

    // layer each item over the top
    for (final Item item : items) {
      if (item != null) {
        item.draw(g, defn);
      }
    }

    g.dispose();

    return image;
  }

  public void refresh() {
    invalidate();
    LayoutConfig.refresh();
  }

  protected void invalidate() {
    for (final GamePieceImage i : getComponentsOf(GamePieceImage.class)) {
      i.rebuildVisualizerImage();
    }
  }

  protected void decodeItemList(String string) {
    items.clear();
    if (string.length() > 0) {
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(string, ',');
      while (sd.hasMoreTokens()) {
        final Item item = Item.decode(this, sd.nextToken());
        addItem(item);
      }
    }

  }

  protected String encodeItemList() {
    final String[] list = new String[getItemCount()];
    int i = 0;
    for (final Item item : items) {
      list[i++] = item.encode();
    }
    final SequenceEncoder se = new SequenceEncoder('#');
    se.append(list);
    return se.getValue();
  }

  @Override
  public int getVisualizerHeight() {
    return getLayoutHeight();
  }

  @Override
  public int getVisualizerWidth() {
    return getLayoutWidth();
  }


  @Override
  public void rebuildVisualizerImage() {
    visImage = buildImage(imageDefn);
  }

  public void setImageDefn(GamePieceImage d) {
    imageDefn = d;
  }

  public GamePieceImage getGenericDefn(String defnName) {
    for (final Buildable b : getBuildables()) {
      if (b instanceof GamePieceImage) {
        final GamePieceImage gpi = (GamePieceImage) b;
        if (gpi.getConfigureName().equals(defnName)) {
          return gpi;
        }
      }
    }
    return null;
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    for (final Item item : items) {
      if (item instanceof ImageItem) {
        s.add(((ImageItem) item).getAttributeValueString(ImageItem.IMAGE));
      }
    }
  }
}
