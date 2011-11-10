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

/**
 * Implementation of numbering for a hex grid supporting rectangular and 
 * oblique (in the old Avalon Hill fashion) numbering schemes. In addition 
 * to the controls provided by HexGridNumbering (Horizontal-Descending,
 * Vertical-Descending, Sideways and Stagger), ObliqueHexGridNumbering 
 * provides the option of specifying either a NW-SE or SW-NE direction
 * for the oblique parallels. Combined with the Horizontal-Descending switch
 * inherited from HexGridNumbering, this allows placement of the oblique 
 * origin at any corner of the map-board, in any orientation.
 * <p>
 * In oblique mode the STAGGER switch inherited from HexGridNumbering has 
 * no effect. Similarly in Rectangular mode the NW_SE switch has no effect.
 * <p>
 * In both sideways and vertical mode the numbering scheme first gives the
 * rectangular coordinate followed by the oblique coordinate. 
 * 
 * @since 3.2.1
 * @version V1.0
 * @author Pieter Geerkens
 * @see RegularGridNumbering
 * @see HexGridX
 * @see <a href="http://playtechs.blogspot.com/2007/04/hex-grids.html">Grids</a>
 *  - TODO archive this web-page as documentation
 */
public class HexGridNumberingX extends AbstractUIHexGridNumberingX 
/* implements EnumerableHexGrid, GameComponent */ {

  public HexGridNumberingX() 
      throws NumberFormatException, IllegalArgumentException, SecurityException{
    super();
    setConfigureName(this.getClass().getSimpleName());
  }

  /** Identify self to AbstractConfigurable implementation */
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
     whif.setAffineTransform(grid, oblique, nw_se, stagger,
         vDescend, hDescend);
   }
  //-------------------------------------------------------------------------
  public static String getConfigureTypeName() { return "HexGridNumbering - Oblique/Rectangular"; }
}
