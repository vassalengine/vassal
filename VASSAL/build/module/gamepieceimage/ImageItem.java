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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.GameModule;
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

public class ImageItem extends Item {

  public static final String TYPE = "Image"; //$NON-NLS-1$
  
  public static final String SRC_VARIABLE = "Specified in individual images";
  public static final String SRC_FIXED = "Fixed for this layout";
  
  protected static final String IMAGE = "image"; //$NON-NLS-1$
  public static final String SOURCE = "source"; //$NON-NLS-1$

  protected String imageSource = SRC_FIXED;
  protected String imageName = ""; //$NON-NLS-1$
  protected Image image = null;
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
  
  public String[] getAttributeDescriptions() {
    String a[] = new String[] { "Image:  ", "Image is:  " };
    String b[] = super.getAttributeDescriptions();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public Class[] getAttributeTypes() {
    Class a[] = new Class[] { Image.class, TextSource.class };
    Class b[] = super.getAttributeTypes();
    Class c[] = new Class[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }

  public String[] getAttributeNames() {
    String a[] = new String[] { IMAGE, SOURCE };
    String b[] = super.getAttributeNames();
    String c[] = new String[a.length + b.length];
    System.arraycopy(b, 0, c, 0, 2);
    System.arraycopy(a, 0, c, 2, a.length);
    System.arraycopy(b, 2, c, a.length+2, b.length-2);
    return c;
  }
  
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
  
  public String getAttributeValueString(String key) {
    
    if (IMAGE.equals(key)) {
      return imageName;
    }  
    else if (SOURCE.equals(key)) {
      return imageSource + ""; //$NON-NLS-1$
    }
    else {
      return super.getAttributeValueString(key);
    }
  }
  
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

  private VisibilityCondition falseCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return false;
    }
  };
  
  private VisibilityCondition fixedCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return imageSource.equals(SRC_FIXED);
    }
  };
  
  public static class TextSource extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[] { SRC_VARIABLE, SRC_FIXED };
    }
  }
  public void draw(Graphics g, GamePieceImage defn) {
    loadImage(defn);
    Point origin = layout.getPosition(this);
    
    if (isAntialias()) {    
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
    } 
    else {
      ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_OFF);
    }
    if (image != null) {
      g.drawImage(image, origin.x, origin.y, null);
    }
  }
  
  public String getType() {
    return TYPE;
  }

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
    
    String iName;
    if (imageSource.equals(SRC_FIXED)) {
      iName = imageName;
    }
    else {
      iName = Ii.getImageName();
    }
    
    image = null;
    imageBounds = new Rectangle();
    
    if (iName == null) {
      
    }
    else if(iName.trim().length() == 0) {
      image = new BufferedImage(10, 10, BufferedImage.TYPE_4BYTE_ABGR);
      Graphics2D bg = (Graphics2D) image.getGraphics();
      bg.setColor(Color.black);
      bg.drawRect(0, 0, 9, 9);
      bg.drawLine(0, 0, 9, 9);
      bg.drawLine(0, 9, 9, 0);
      bg.dispose();
      imageBounds = new Rectangle(-5, -5, 10, 10);
    }
    else {
      try {
        image = GameModule.getGameModule().getDataArchive().getCachedImage(iName);
        imageBounds = DataArchive.getImageBounds(image);
      }
      catch (IOException e) {
      }
    }
  }
  
  public static Item decode(GamePieceLayout l, String s) {
    
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    
    ImageItem item = new ImageItem(l);
    
    sd.nextToken();
    item.imageName = sd.nextToken(""); //$NON-NLS-1$
    item.imageSource = sd.nextToken(SRC_FIXED);
    
    return item;
  }
  
  public String encode() {
   
    SequenceEncoder se1 = new SequenceEncoder(TYPE, ';');
    
    se1.append(imageName+""); //$NON-NLS-1$
    se1.append(imageSource+""); //$NON-NLS-1$
   
    SequenceEncoder se2 = new SequenceEncoder(se1.getValue(), '|');
    se2.append(super.encode());
    
    return se2.getValue();
  }
  
  
}
