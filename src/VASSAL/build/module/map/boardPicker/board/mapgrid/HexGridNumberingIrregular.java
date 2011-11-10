/*
 * $Id$
 * 
 * Copyright (c) 2010-2011 by Pieter Geerkens
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

package VASSAL.build.module.map.boardPicker.board.mapgrid;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering;
import VASSAL.build.module.map.boardPicker.board.mapgrid.WorldHexIndices.*;
import VASSAL.configure.Attribute.AbstractAttribute;
import VASSAL.configure.VisibilityCondition;

/**
 * Implementation of arbitrary hex grid numbering by direct specification
 * of the affine transform matrix elements from grid-canonical indices to 
 * world indices.
 * 
 * This class is almost identical to HexGridNumbering, but just how to best
 * abstract the commonality remains unclear to date.
 * 
 * @since 3.2.1
 * @version V1.0
 * @author Pieter Geerkens
 * @see RegularGridNumbering
 * @see HexGridX
 * @see <a href="http://playtechs.blogspot.com/2007/04/hex-grids.html">Grids</a>
 *  - TODO archive this web-page as documentation
 */
public class HexGridNumberingIrregular extends AbstractUIHexGridNumberingIrregular {

  public HexGridNumberingIrregular() 
      throws NumberFormatException, IllegalArgumentException, SecurityException {
    super();
    setConfigureName(this.getClass().getSimpleName());
  }

  /** Identify self to AbstractConfigurable implementation    */
  @Override
  public void addTo(Buildable parent) {
    super.addTo(parent);
    whif = new WorldHexIndicesFactory(this);
    this.addPropertyChangeListener(pclNumbering);
    grid.addPropertyChangeListener(pclNumbering);
    if (GameModule.getGameModule() != null)
      GameModule.getGameModule().getGameState().addGameComponent(this);
  }
  //-------------------- EnumerableHexGrid implementation -------------------
  @Override
  public void setAffineTransform() {
    whif.setAffineTransform(m00, m10, m01, m11, m02, m12);
  }
   //-------------------------------------------------------------------------
  public static String getConfigureTypeName() { return "HexGridNumbering - Irregular"; }
  
  protected VisibilityCondition visibilityHDescend = AbstractAttribute.invisible;
  protected VisibilityCondition visibilityVDescend = AbstractAttribute.invisible;
}
