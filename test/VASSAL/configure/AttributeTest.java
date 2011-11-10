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

import VASSAL.build.module.map.boardPicker.board.mapgrid.RegularGridNumbering.*;
import VASSAL.configure.Attribute;
import VASSAL.configure.ConcreteProperties;
import VASSAL.configure.Attribute.*;

public class AttributeTest {
  AttributeTest() {;}

  private static void echoInitial(Attribute<?> at, ConcreteProperties props){
    System.out.format("    %1$s (%2$s) set to '%3$8s' as initial (%4$s): \n", 
      at.name(), at.description(), 
      props.getAttributeValueString(at.name()), at.type().getName());
  }
  private static void echoWork(Attribute<?> at, String sWork, ConcreteProperties props) {
    System.out.format("    %1$s set to '%2$8s' as work (%3$s): \n", 
        at.name(), props.getAttributeValueString(at.name()), sWork);
  }

  @SuppressWarnings("cast")  // "unnecessary" casts test implicit conversions
  public static void main(String[] argsv) {
    AbstractAttribute<?> at;
    
    final ConcreteProperties propConc = new ConcreteProperties();
    final AttributeList propList = propConc;;
    
    // -----------------------------------------------------
    System.out.format("\nVerify Initialization:\n");
    for (AbstractAttribute<?> attribute: propList) {
      echoInitial(attribute,propConc);
    }
    
    // -----------------------------------------------------
    System.out.format("\nVerify Boolean01:\n");
    at = propList.attribute(ConcreteProperties.BOOLEAN01);
    propList.setAttribute(ConcreteProperties.BOOLEAN01,false);
    echoWork(at,"false",propConc);
    propList.setAttribute(ConcreteProperties.BOOLEAN01,"true");
    echoWork(at,"true",propConc);

    // -----------------------------------------------------
    System.out.format("\nVerify Long06:\n");
    at = propList.attribute(ConcreteProperties.LONG06);
    propList.setAttribute(ConcreteProperties.LONG06,((Long)(-4L)));
    echoWork(at,"(Long)(-4L))",propConc);
    propList.setAttribute(ConcreteProperties.LONG06,"65536");
    echoWork(at,"65536",propConc);
    propList.setAttribute(ConcreteProperties.LONG06,(long)-32768L);
    echoWork(at,"(long)-32768L",propConc);
    
    // -----------------------------------------------------
    System.out.format("\nVerify Double08:\n");
    at = propList.attribute(ConcreteProperties.DOUBLE08);
    propList.setAttribute(ConcreteProperties.DOUBLE08,(Double)2.718281828D);
    echoWork(at,"(Double)2.71828 1828D",propConc);
    propList.setAttribute(ConcreteProperties.DOUBLE08,"3.141592653589793");
    echoWork(at,"3.141592653589793",propConc);
    propList.setAttribute(ConcreteProperties.DOUBLE08,(double)2.718281828D);
    echoWork(at,"(double)2.71828 1828D",propConc);
    
    // -----------------------------------------------------
    System.out.format("\nVerify String09:\n");
    at = propList.attribute(ConcreteProperties.STRING09);
    propList.setAttribute(ConcreteProperties.STRING09,"2nd string value");
    echoWork(at,"2nd string value",propConc);

    // -----------------------------------------------------
    System.out.format("\nStringEnum FirstCoordinate Test: \n");
    EnumAttribute<?,?> eat;
    eat = (EnumAttribute<?, ?>) propList.attribute(ConcreteProperties.FIRST10);

    propList.setAttribute(ConcreteProperties.FIRST10, FirstCoord.VERTICAL_FIRST);
    echoWork(eat, "VERTICAL_FIRST",propConc);
    propList.setAttribute(ConcreteProperties.FIRST10, FirstCoord.HORIZONTAL_FIRST.toString());
    echoWork(eat, FirstCoord.HORIZONTAL_FIRST.toString(),propConc);
    propList.setAttribute(ConcreteProperties.FIRST10, "V");
    echoWork(eat, "V",propConc);
    propList.setAttribute(ConcreteProperties.FIRST10, "H");
    echoWork(eat, "H",propConc);
    propConc.first10 = FirstCoord.VERTICAL_FIRST;
    echoWork(eat, FirstCoord.VERTICAL_FIRST.toString(),propConc);
    propConc.first10 = FirstCoord.HORIZONTAL_FIRST;
    echoWork(eat, FirstCoord.HORIZONTAL_FIRST.toString(),propConc);
    
    System.out.format("\n - valid values for %1$s:\n",FirstCoord.class.getName());
    for (String s: eat.getValidValues(null)) {
      System.out.format("    %1$s\n",s);
    }
      
    // -----------------------------------------------------
    System.out.format("\nStringEnum TextAngle Test:\n");
    eat = (EnumAttribute<?, ?>) propList.attribute(ConcreteProperties.TEXTANGLE11);
    propList.setAttribute(ConcreteProperties.TEXTANGLE11, AngleEnum._180);
    echoWork(eat, "_180",propConc);
    propList.setAttribute(ConcreteProperties.TEXTANGLE11, AngleEnum._270.toString());
    echoWork(eat, AngleEnum._270.toString(),propConc);
    propList.setAttribute(ConcreteProperties.TEXTANGLE11, "330");
    echoWork(eat, "330",propConc);
    
    System.out.format("\n - valid values for %1$s:\n",AngleEnum.class.getName());
    long i = 0;
    for (String s: eat.getValidValues(null)) {
      System.out.format("    %1$5s",s);
      if ( (i++ % 8) == 7) System.out.format("\n");
    }
    System.out.format("\n");
    
    // -----------------------------------------------------
    System.out.format("\nStringEnum CoordType Test:\n");
    eat = (EnumAttribute<?, ?>) propList.attribute(ConcreteProperties.COORDTYPE12);
    propList.setAttribute(ConcreteProperties.COORDTYPE12, CoordType.NUMERIC);
    echoWork(eat, "NUMERIC",propConc);
    propList.setAttribute(ConcreteProperties.COORDTYPE12, CoordType.ALPHABETIC.toString());
    echoWork(eat, CoordType.ALPHABETIC.toString(),propConc);
    propList.setAttribute(ConcreteProperties.COORDTYPE12, "N");
    echoWork(eat, "N",propConc);
    propList.setAttribute(ConcreteProperties.COORDTYPE12, "A");
    echoWork(eat, "A",propConc);
    propConc.coordType12 = CoordType.NUMERIC;
    echoWork(eat, CoordType.NUMERIC.toString(),propConc);
    propConc.coordType12 = CoordType.ALPHABETIC;
    echoWork(eat, CoordType.ALPHABETIC.toString(),propConc);
    
    System.out.format("\n - valid values for %1$s:\n",CoordType.class.getName());
    for (String s: eat.getValidValues(null)) {
      System.out.format("    %1$s\n",s);
    }
    
    // -----------------------------------------------------
    System.out.format("\nColor Test:\n");
    at = propList.attribute(ConcreteProperties.COLOR13);
    propList.setAttribute(ConcreteProperties.COLOR13, "255,255,0");
    echoWork(at,"yellow",propConc);
    propList.setAttribute(ConcreteProperties.COLOR13, Color.CYAN);
    echoWork(at,"cyan",propConc);
    
    System.out.format("\n*** All done! ***\n");
  }
}
