/*
 *
 * Copyright (c) 2022 Christian Holm Christensen
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
package VASSAL.build.module.map.boardPicker.board.mapgrid;

import java.awt.Point;

import org.apache.commons.lang3.ArrayUtils;

import VASSAL.i18n.Resources;

public class ObliqueHexGridNumbering extends HexGridNumbering {
  public boolean direction = true;
  public static final String DIRECTION = "direction"; //NON-NLS

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.add(super.getAttributeDescriptions(),
                          Resources.getString("Editor.ObliqueHexGridNumbering.direction"));
  }

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.add(super.getAttributeNames(), DIRECTION);
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.add(super.getAttributeTypes(), Boolean.class);
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (DIRECTION.equals(key)) {
      if (value instanceof String) 
        value = Boolean.valueOf((String) value);
      direction = (Boolean) value;
    }
    else 
      super.setAttribute(key, value);
  }

  @Override
  public String getAttributeValueString(String key) {
    if (DIRECTION.equals(key)) 
      return String.valueOf(direction);
    else 
      return super.getAttributeValueString(key);
  }

  @Override
  public int getRow(Point p) {
    return slantRow(super.getRow(p), super.getColumn(p));
  }
  public int slantRow(int row, int column) {
    return direction ? (int)Math.floor(column/2) + row : row - (int)Math.floor(column/2) + 1;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ObliqueHexGridNumbering.component_type"); //$NON-NLS-1$
  }
}
// EOF
