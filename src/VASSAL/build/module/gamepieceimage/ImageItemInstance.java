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

import java.awt.Image;
import java.io.File;

import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.SequenceEncoder;

public class ImageItemInstance extends ItemInstance {

  protected static final String IMAGE = "image"; //$NON-NLS-1$

  protected String imageName = ""; //$NON-NLS-1$

  public ImageItemInstance() {
    super();
  }

  public ImageItemInstance(String code, GamePieceImage defn) {
    super(defn);
    decode(code);
  }


  public ImageItemInstance(String name, String type, String location) {
    super(name, type, location);
  }

  public ImageItemInstance(String nam, String typ, String loc, String iName) {
    super(nam, typ, loc);
    imageName = iName;
  }

  public String encode() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(getType())
      .append(getName())
      .append(getLocation())
      .append(imageName);
    return se.getValue();
  }

  public void decode(String code) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(code, ';');
    setType(sd.nextToken("")); //$NON-NLS-1$
    setName(sd.nextToken("")); //$NON-NLS-1$
    setLocation(sd.nextToken("")); //$NON-NLS-1$
    imageName = sd.nextToken(""); //$NON-NLS-1$
  }

  public String getImageName() {
    return imageName;
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Image:  " };
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] { Image.class, };
  }

  public String[] getAttributeNames() {
    return new String[] { IMAGE };
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
    else {
      super.setAttribute(key, o);
    }
    if (myConfig != null) {
      myConfig.rebuildViz();
    }
  }

  public String getAttributeValueString(String key) {

    if (IMAGE.equals(key)) {
      return imageName;
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (IMAGE.equals(name)) {
       return imageCond;
     }
     else {
       return super.getAttributeVisibility(name);
     }
   }

  private VisibilityCondition imageCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      return !((ImageItem) getItem()).isFixed();
    }
  };
}
