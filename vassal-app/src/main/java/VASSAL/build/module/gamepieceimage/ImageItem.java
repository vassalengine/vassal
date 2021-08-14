/*
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

import VASSAL.build.AutoConfigurable;
import VASSAL.configure.StringEnum;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import VASSAL.i18n.Resources;
import org.apache.commons.lang3.ArrayUtils;

import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.AbstractTileOpImpl;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;

public class ImageItem extends Item {

  public static final String TYPE = "Image"; //$NON-NLS-1$

  public static final String SRC_VARIABLE = "Specified in individual images"; // NON-NLS - Yes really!
  public static final String SRC_FIXED = "Fixed for this layout"; // NON-NLS - Yes really!

  protected static final String IMAGE = "image"; //$NON-NLS-1$
  public static final String SOURCE = "source"; //$NON-NLS-1$

  protected String imageSource = SRC_FIXED;
  protected String imageName = ""; //$NON-NLS-1$
  protected ImageOp srcOp;
  protected Rectangle imageBounds = new Rectangle();

  public ImageItem() {
    super();
  }

  public ImageItem(GamePieceLayout l) {
    super(l);
  }

  public ImageItem(GamePieceLayout l, String n) {
    this(l);
    setConfigureName(n);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.insert(
      2, super.getAttributeDescriptions(),
      Resources.getString("Editor.image_label"),
      Resources.getString("Editor.ImageItem.image_option")
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.insert(
      2, super.getAttributeTypes(),
      Image.class,
      TextItem.TextSource.class);
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.insert(
      2, super.getAttributeNames(),
      IMAGE,
      SOURCE
    );
  }

  @Override
  public void setAttribute(String key, Object o) {
    if (IMAGE.equals(key)) {
      if (o instanceof String) {
        imageName = (String) o;
      }
      else {
        if (o == null) {
          imageName = null;
        }
        else {
          imageName = ((File) o).getName();
        }
      }
    }
    else if (SOURCE.equals(key)) {
      imageSource = (String) o;
    }
    else {
      super.setAttribute(key, o);
    }

    if (layout != null) {
      layout.refresh();
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (IMAGE.equals(key)) {
      return imageName;
    }
    else if (SOURCE.equals(key)) {
      return imageSource;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (ROTATION.equals(name)) {
      return falseCond;
    }
    else if (IMAGE.equals(name)) {
      return fixedCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private final VisibilityCondition falseCond = () -> false;

  private final VisibilityCondition fixedCond = () -> imageSource.equals(SRC_FIXED);

  public static class TextSource extends StringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { SRC_VARIABLE, SRC_FIXED };
    }
  }

  @Override
  public void draw(Graphics g, GamePieceImage defn) {
    loadImage(defn);

    if (srcOp != null) {
      final BufferedImage img = srcOp.getImage();
      if (img != null) {

        final Graphics2D g2d = (Graphics2D) g;
        final Object aa = g2d.getRenderingHint(RenderingHints.KEY_ANTIALIASING);

        g2d.setRenderingHint(
          RenderingHints.KEY_ANTIALIASING,
          isAntialias() ? RenderingHints.VALUE_ANTIALIAS_ON :
                          RenderingHints.VALUE_ANTIALIAS_OFF
        );

        final Point origin = layout.getPosition(this);
        g.drawImage(img, origin.x, origin.y, null);

        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, aa);
      }
    }
  }

  @Override
  public String getType() {
    return TYPE;
  }

  @Override
  public String getDisplayName() {
    return Resources.getString("Editor.ImageItem.component_type");
  }

  @Override
  public Dimension getSize() {
    return imageBounds.getSize();
  }

  public boolean isFixed() {
    return imageSource.equals(SRC_FIXED);
  }

  protected void loadImage(GamePieceImage defn) {
    ImageItemInstance Ii = null;
    if (defn != null) {
      Ii = defn.getImageInstance(getConfigureName());
    }
    if (Ii == null) {
      Ii = new ImageItemInstance();
    }

    final String iName;
    if (imageSource.equals(SRC_FIXED)) {
      iName = imageName;
    }
    else {
      iName = Ii.getImageName();
    }

    if (iName != null) {
      if (iName.isBlank()) {
        srcOp = BaseOp.op;
      }
      else {
        srcOp = Op.load(iName);
      }
      imageBounds = ImageUtils.getBounds(srcOp.getSize());
    }
    else {
      imageBounds = new Rectangle();
    }
  }

  protected static final class BaseOp extends AbstractTileOpImpl {
    private BaseOp() {

    }

    private static final BaseOp op = new BaseOp();

    @Override
    public BufferedImage eval() {
      final BufferedImage img =
        ImageUtils.createCompatibleTranslucentImage(10, 10);
      final Graphics2D bg = img.createGraphics();
      bg.setColor(Color.black);
      bg.drawRect(0, 0, 9, 9);
      bg.drawLine(0, 0, 9, 9);
      bg.drawLine(0, 9, 9, 0);
      bg.dispose();
      return img;
    }

    @Override
    protected void fixSize() { }

    @Override
    public Dimension getSize() {
      return new Dimension(10, 10);
    }

    @Override
    public int getWidth() {
      return 10;
    }

    @Override
    public int getHeight() {
      return 10;
    }

    @Override
    public List<VASSAL.tools.opcache.Op<?>> getSources() {
      return Collections.emptyList();
    }

    // NB: This ImageOp doesn't need custom equals() or hashCode()
    // because it's a singleton.
  }

  public static Item decode(GamePieceLayout l, String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    final ImageItem item = new ImageItem(l);

    sd.nextToken();
    item.imageName = sd.nextToken(""); //$NON-NLS-1$
    item.imageSource = sd.nextToken(SRC_FIXED);

    return item;
  }

  @Override
  public String encode() {
    final SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');

    se1.append(imageName)
       .append(imageSource);

    final SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());

    return se2.getValue();
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (imageName != null) s.add(imageName);
  }
}
