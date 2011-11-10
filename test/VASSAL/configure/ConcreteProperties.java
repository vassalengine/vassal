/*
 * $Id$
 *
 * Copyright (c) 2011 by Pieter Geerkens
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

package VASSAL.configure; 

import java.awt.Color;

import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering.AngleEnum;
import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering.CoordType;
import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering.FirstCoord;
import VASSAL.configure.AbstractAttributeListConfigurable;
import VASSAL.configure.Attribute.ColorAttribute;
import VASSAL.configure.Attribute.EnumCharAttribute;
import VASSAL.configure.Attribute.EnumIntAttribute;

public class ConcreteProperties extends AbstractAttributeListConfigurable {
    
  // Attributes of class
  static final public String BOOLEAN01 = "boolean01";
  static final public String BYTE02 = "byte02";
  static final public String SHORT03 = "short03";
  static final public String CHAR04 = "char04";
  static final public String INT05 = "int05";
  static final public String LONG06 = "long06";
  static final public String FLOAT07 = "float07";
  static final public String DOUBLE08 = "double08";
  static final public String STRING09 = "string09";
  static final public String FIRST10 = "first10";
  static final public String TEXTANGLE11 = "textAngle11";
  static final public String COORDTYPE12 = "coordType12";
  static final public String COLOR13 = "color13";
    
  // allocation of storage and type for attributes
  Boolean     boolean01  = true;
  byte       byte02    = -1;
  short     short03    = 32677;
  char       char04    = 'N';
  int       int05      = 0x7FFFFFFF;
  long       long06    = 0x80000000L;
  float     float07    = 2.71828F;
  double     double08    = 3.141592653589793D;
  String    string09    = "1st string value";
  FirstCoord  first10    = FirstCoord.HORIZONTAL_FIRST;
  AngleEnum  textAngle11  = AngleEnum.__90;
  CoordType  coordType12  = CoordType.NUMERIC;
  Color     color13    = Color.MAGENTA;
  

  // set capacity low to verify ordering on successive re-sizes.
  ConcreteProperties() { this(2); }
  @SuppressWarnings("deprecation")
  ConcreteProperties(int capacity)  { 
    super(capacity); 
      
    addAttribute(BOOLEAN01, "Boolean property (true)");
    addAttribute(BYTE02, "Byte property (-1)");
    addAttribute(SHORT03, "Short property (32677)");
    addAttribute(CHAR04, "Char property ('N')") ;      
    addAttribute(INT05, "Int property (2,147,483,647)") ;  
    addAttribute(LONG06, "Long property (2,147,483,648)") ;
    addAttribute(FLOAT07,"Float Attribute (2.71828)");      
    addAttribute( DOUBLE08,  "Double attribute (pi)") ;      
    addAttribute("string09","Color (Initially magenta)");
    
    addAttribute( new EnumCharAttribute<FirstCoord>
      (FirstCoord.class, FIRST10, "StringEnum FirstCoordinate (H)") {});    
    addAttribute(new EnumIntAttribute<AngleEnum>
      (AngleEnum.class, TEXTANGLE11, "StringEnum TextAngle (90)") {});      
    addAttribute(new EnumCharAttribute<CoordType>
      (CoordType.class,COORDTYPE12,"Enum CoordType (Numeric)") {});
    addAttribute(new ColorAttribute
        ("color13","Color (Initially magenta)"){});
  }
  @Override
  public void removeFrom(Buildable parent) {}
  @Override
  public HelpFile getHelpFile()  { return null; }
  @Override
  public Class<?>[] getAllowableConfigureComponents() { return null; }
  @Override
  public void addTo(Buildable parent) {}
}