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
  /**
   * Return attribute descriptions.
   *
   * This replaces the "stagger" description with the "slanted"
   * description.
   */
  @Override
  public String[] getAttributeDescriptions() {
    final String[] parent = super.getAttributeDescriptions();
    return ArrayUtils.add(ArrayUtils.remove(parent, parent.length - 1),
                          Resources.getString("Editor.ObliqueHexGridNumbering.direction"));
  }

  @Override
  public int getRow(Point p) {
    return slantRow(super.getRow(p), super.getColumn(p));
  }
  public int slantRow(int row, int column) {
    return stagger ? column/2 + row : row - column/2 + 1;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.ObliqueHexGridNumbering.component_type"); //$NON-NLS-1$
  }
}
// EOF
