/*
 * Copyright 2026 Vassal Development Team
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
package VASSAL.build;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

import VASSAL.build.module.GameRefresher;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.DynamicProperty;
import VASSAL.counters.GamePiece;
import VASSAL.counters.SetGlobalProperty;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

/**
 * Game Refresher: carrying Dynamic Property values across to a modified
 * piece definition, matching traits by property name (issue #13085).
 */
public class GpIdCheckerTest {

  private MockedStatic<GameModule> staticGm;

  @BeforeEach
  public void setUp() {
    staticGm = Mockito.mockStatic(GameModule.class);
    staticGm.when(GameModule::getGameModule).thenReturn(mock(GameModule.class));
  }

  @AfterEach
  public void tearDown() {
    staticGm.close();
  }

  /** A piece in a saved game, with its Dynamic Properties at in-game values. */
  private static GamePiece oldPiece() {
    GamePiece p = new BasicPiece(BasicPiece.ID + ";;;Light Cavalry;"); // NON-NLS
    p = new DynamicProperty("PROP;MarkerLevel;true,0,4,false;;Marker level", p); // NON-NLS
    p = new DynamicProperty("PROP;Strength;true,0,6,false;;Strength", p); // NON-NLS
    p = new DynamicProperty("PROP;Nationality;false,0,100,false;;Nation", p); // NON-NLS
    setValue(p, "Nationality", "Portuguese"); // NON-NLS
    setValue(p, "Strength", "3"); // NON-NLS
    setValue(p, "MarkerLevel", "2"); // NON-NLS
    return p;
  }

  /**
   * The same piece after a module edit: constraints and descriptions changed,
   * traits reordered, a new Dynamic Property added, and a Set Global Property
   * trait (a Dynamic Property subclass) added.
   */
  private static GamePiece newPiece() {
    GamePiece p = new BasicPiece(BasicPiece.ID + ";;;Light Cavalry;"); // NON-NLS
    p = new DynamicProperty("PROP;Strength;true,0,8,false;;Strength (edited)", p); // NON-NLS
    p = new SetGlobalProperty("setprop;Strength;true,0,8,false;;Set GP;;;", p); // NON-NLS
    p = new DynamicProperty("PROP;MarkerLevel;true,0,5,false;;Marker level (edited)", p); // NON-NLS
    p = new DynamicProperty("PROP;Disordered;false,0,100,false;;New trait", p); // NON-NLS
    p = new DynamicProperty("PROP;Nationality;false,0,100,false;;Nation (edited)", p); // NON-NLS
    return p;
  }

  private static void setValue(GamePiece p, String key, String value) {
    for (GamePiece q = p; q instanceof Decorator; q = ((Decorator) q).getInner()) {
      if (q.getClass() == DynamicProperty.class && key.equals(((DynamicProperty) q).getKey())) {
        ((Decorator) q).mySetState(value);
        return;
      }
    }
  }

  @Test
  public void matchesModifiedDynamicPropertiesByName() {
    final GamePiece newP = newPiece();
    new GpIdChecker(Set.of(GameRefresher.USE_DYNAMICPROPERTY_NAME)).copyState(oldPiece(), newP);

    assertEquals("Portuguese", newP.getProperty("Nationality")); // NON-NLS
    assertEquals("3", newP.getProperty("Strength")); // NON-NLS
    assertEquals("2", newP.getProperty("MarkerLevel")); // NON-NLS
    assertEquals("", newP.getProperty("Disordered")); // NON-NLS
  }

  @Test
  public void modifiedDynamicPropertiesRevertWithoutOption() {
    final GamePiece newP = newPiece();
    new GpIdChecker(Set.of()).copyState(oldPiece(), newP);

    assertEquals("", newP.getProperty("Nationality")); // NON-NLS
    assertEquals("", newP.getProperty("Strength")); // NON-NLS
    assertEquals("", newP.getProperty("MarkerLevel")); // NON-NLS
  }
}
