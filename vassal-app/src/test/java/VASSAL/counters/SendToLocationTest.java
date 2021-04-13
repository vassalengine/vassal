/*
 * Copyright 2020 Vassal Development Team
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

package VASSAL.counters;

import VASSAL.tools.NamedKeyStroke;
import java.awt.Point;
import java.lang.reflect.InvocationTargetException;
import org.junit.jupiter.api.Test;

public class SendToLocationTest extends DecoratorTest {

  @Test
  public void serializeTests() throws InvocationTargetException, NoSuchMethodException, InstantiationException, IllegalAccessException {

    SendToLocation trait = new SendToLocation();

    // Default trait
    serializeTest("Default trait", trait); // NON-NLS

    // Complex trait
    trait = new SendToLocation();
    trait.commandName = "command";
    trait.key = NamedKeyStroke.of("xyzzy");
    trait.mapId.setFormat("abc");
    trait.boardName.setFormat("def");
    trait.x.setFormat("2");
    trait.y.setFormat("3");
    trait.backCommandName = "goback";
    trait.backKey = NamedKeyStroke.of("plover");
    trait.xIndex.setFormat("4");
    trait.yIndex.setFormat("5");
    trait.xOffset.setFormat("6");
    trait.yOffset.setFormat("7");
    trait.description = "desc";
    trait.destination = SendToLocation.DEST_LOCATION;
    trait.zone.setFormat("thezone");
    trait.region.setFormat("theregion");
    trait.propertyFilter.setExpression("abc=23");
    trait.gridLocation.setFormat("U21.23");

    serializeTest("Complex trait", trait); // NON-NLS

    // test Back options
    trait = new SendToLocation();
    trait.commandName = "command";
    trait.key = NamedKeyStroke.of("xyzzy");
    trait.mapId.setFormat("abc");
    trait.boardName.setFormat("def");
    trait.x.setFormat("2");
    trait.y.setFormat("3");
    trait.backCommandName = "goback";
    trait.backKey = NamedKeyStroke.of("plover");
    trait.xIndex.setFormat("4");
    trait.yIndex.setFormat("5");
    trait.xOffset.setFormat("6");
    trait.yOffset.setFormat("7");
    trait.description = "desc";
    trait.destination = SendToLocation.DEST_LOCATION;
    trait.zone.setFormat("thezone");
    trait.region.setFormat("theregion");
    trait.propertyFilter.setExpression("abc=23");
    trait.gridLocation.setFormat("U21.23");

    final Point backPoint = new Point(42, 134);
    trait.setInner(createBasicPiece());
    trait.setProperty(SendToLocation.BACK_POINT, backPoint);

    serializeTest("Back Test", trait); // NON-NLS
  }
}
