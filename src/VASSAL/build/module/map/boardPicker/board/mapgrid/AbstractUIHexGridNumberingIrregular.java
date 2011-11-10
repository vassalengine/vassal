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

public abstract class AbstractUIHexGridNumberingIrregular extends AbstractHexGridNumbering{

  protected Double m00 = 1D;
  protected Double m10 = 0D;
  protected Double m01 = 0D;
  protected Double m11 = 1D;
  protected Double m02 = 0D;
  protected Double m12 = 0D;
  
  public static final String M00 = "M00";
  public static final String M10 = "M10";
  public static final String M01 = "M01";
  public static final String M11 = "M11";
  public static final String M02 = "M02";
  public static final String M12 = "M12";
  
  public AbstractUIHexGridNumberingIrregular() 
      throws NumberFormatException,  IllegalArgumentException, SecurityException  {
    super();
    addAttribute(M00, "Affine transformation element m00:  ");
    addAttribute(M10, "Affine transformation element m10:  ");
    addAttribute(M01, "Affine transformation element m01:  ");
    addAttribute(M11, "Affine transformation element m11:  ");
    addAttribute(M02, "Affine transformation element m02:  ");
    addAttribute(M12, "Affine transformation element m12:  ");
  }
}
