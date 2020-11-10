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
import java.awt.Dimension;
import java.awt.Graphics;
import java.util.List;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.SequenceEncoder;

/**
 * The base portion of a Counter Layout component.  Contains the draw() method, but may override specific values from an associated (via the name attribute) {@link ItemInstance}
 */
public abstract class Item extends AbstractConfigurable {

  public static final String TYPE = ""; //$NON-NLS-1$

  protected static final String NAME = "name"; //$NON-NLS-1$
  protected static final String LOCATION = "location"; //$NON-NLS-1$
  protected static final String ADVANCED = "advanced"; //$NON-NLS-1$
  protected static final String ROTATION = "rotation"; //$NON-NLS-1$
  protected static final String X_OFFSET = "xoffset"; //$NON-NLS-1$
  protected static final String Y_OFFSET = "yoffset"; //$NON-NLS-1$
  protected static final String ANTIALIAS = "antialias"; //$NON-NLS-1$

  String location = GamePieceLayout.CENTER;
  protected int xoffset, yoffset;
  protected boolean advanced = false;
  protected int rotation = 0;
  protected boolean antialias = true;

  protected GamePieceLayout layout;

  public Item() {
    super();
    setConfigureName(""); //$NON-NLS-1$
  }

  public Item(GamePieceLayout l) {
    this();
    layout = l;
  }

  public Item(String name) {
    this();
    setConfigureName(name);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.Item.location"),
      Resources.getString("Editor.Item.advanced_options"),
      Resources.getString("Editor.x_offset"),
      Resources.getString("Editor.y_offset"),
      Resources.getString("Editor.Item.rotation_degrees"),
      Resources.getString("Editor.Item.anti_alias")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      String.class,
      LocationConfig.class,
      Boolean.class,
      Integer.class,
      Integer.class,
      Integer.class,
      Boolean.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ""); //$NON-NLS-1$
    }
  }

  public static class LocationConfig extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return GamePieceLayout.LOCATIONS;
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return GamePieceLayout.LOCATION_I18N_KEYS;
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] { NAME, LOCATION, ADVANCED, X_OFFSET, Y_OFFSET, ROTATION, ANTIALIAS };
  }

  @Override
  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (LOCATION.equals(key)) {
      location = (String) o;
    }
    else if (X_OFFSET.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      xoffset = (Integer) o;
    }
    else if (Y_OFFSET.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      yoffset = (Integer) o;
    }
    else if (ADVANCED.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      advanced = (Boolean) o;
    }
    else if (ROTATION.equals(key)) {
      if (o instanceof String) {
        o = Integer.valueOf((String) o);
      }
      rotation = (Integer) o;
    }
    else if (ANTIALIAS.equals(key)) {
      if (o instanceof String) {
        o = Boolean.valueOf((String) o);
      }
      antialias = (Boolean) o;
    }

  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (LOCATION.equals(key)) {
      return location;
    }
    else if (X_OFFSET.equals(key)) {
      return Integer.toString(xoffset);
    }
    else if (Y_OFFSET.equals(key)) {
      return Integer.toString(yoffset);
    }
    else if (ADVANCED.equals(key)) {
      return Boolean.toString(advanced);
    }
    else if (ROTATION.equals(key)) {
      return Integer.toString(rotation);
    }
    else if (ANTIALIAS.equals(key)) {
      return Boolean.toString(antialias);
    }
    else
      return null;
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (List.of(ROTATION, X_OFFSET, Y_OFFSET, ANTIALIAS).contains(name)) {
      return advancedCond;
    }
    else {
      return null;
    }
  }

  @Override
  public void removeFrom(Buildable parent) {

  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void addTo(Buildable parent) {
    setAllAttributesUntranslatable();
  }

  private final VisibilityCondition advancedCond = () -> advanced;


  /**
   * Implemented by subclass to draw itself.
   */
  public abstract void draw(Graphics g, GamePieceImage defn);
  public abstract String getType();
  public abstract Dimension getSize();

  public String getDisplayName() {
    return getType();
  }

  public String getDisplayLocation() {
    return GamePieceLayout.getDisplayLocation(location);
  }

  public String getLocation() {
    return location;
  }

  public int getXoffset() {
    return xoffset;
  }

  public int getYoffset() {
    return yoffset;
  }

  public int getRotation() {
    return rotation;
  }

  public boolean isAntialias() {
    return antialias;
  }

  protected GamePieceLayout getLayout() {
    return layout;
  }

  public static Item decode(GamePieceLayout layout, String s) {
    final SequenceEncoder.Decoder sd1 = new SequenceEncoder.Decoder(s, '|');
    final String t1 = sd1.nextToken(""); //$NON-NLS-1$
    final String t2 = sd1.nextToken(""); //$NON-NLS-1$

    final Item item;

    if (t1.startsWith(SymbolItem.TYPE)) {
      item = SymbolItem.decode(layout, t1);
    }
    else if (t1.startsWith(TextBoxItem.TYPE)) {
      item = TextBoxItem.decode(layout, t1);
    }
    else if (t1.startsWith(TextItem.TYPE)) {
      item = TextItem.decode(layout, t1);
    }
    else if (t1.startsWith(ImageItem.TYPE)) {
      item = ImageItem.decode(layout, t1);
    }
    else if (t1.startsWith(ShapeItem.TYPE)) {
      item = ShapeItem.decode(layout, t1);
    }
    else
      return null;

    final SequenceEncoder.Decoder sd2 = new SequenceEncoder.Decoder(t2, ';');
    item.setConfigureName(sd2.nextToken());
    item.location = sd2.nextToken();
    item.xoffset = sd2.nextInt(0);
    item.yoffset = sd2.nextInt(0);
    item.rotation = sd2.nextInt(0);
    item.antialias = sd2.nextBoolean(true);

    return item;
  }

  public String encode() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(getConfigureName());
    se.append(location);
    se.append(xoffset);
    se.append(yoffset);
    se.append(rotation);
    se.append(antialias);
    return se.getValue();
  }
}
