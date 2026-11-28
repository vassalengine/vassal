package VASSAL.tools.imports.adc2;

import VASSAL.build.GameModule;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid.BadCoords;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.awt.Point;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ADC2ModuleTest {

  @Test
  public void writePlayersToArchive_noPlayers() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final PlayerRoster roster = new PlayerRoster();
      when(gm.getAllDescendantComponentsOf(PlayerRoster.class)).thenReturn(List.of(roster));

      new ADC2Module().writePlayersToArchive(gm);

      assertEquals(List.of(), roster.getSides());
    }
  }

  /**
   * A stack whose location the grid reads back as the same place records it.
   */
  @Test
  public void gridLocationOf_roundTripsToTheSameName() throws BadCoords {
    final MapGrid mg = mock(MapGrid.class);
    final Point p = new Point(10, 20);
    when(mg.getLocation("A1")).thenReturn(p);
    when(mg.locationName(p)).thenReturn("A1");

    assertEquals("A1", ADC2Module.gridLocationOf(mg, "A1"));
  }

  /**
   * #13711, #14206, #14546, #11930, #9375: a name the grid reads back as a
   * different place is not a grid location, and used to throw an
   * AssertionError which abandoned the entire import. The stack's absolute X
   * and Y are already set, so giving up the grid location costs nothing.
   */
  @Test
  public void gridLocationOf_isNullWhenTheNameDoesNotRoundTrip() throws BadCoords {
    final MapGrid mg = mock(MapGrid.class);
    final Point p = new Point(10, 20);
    when(mg.getLocation("A1")).thenReturn(p);
    when(mg.locationName(p)).thenReturn("B2");

    assertNull(ADC2Module.gridLocationOf(mg, "A1"));
  }

  /**
   * The grid may not place the name at all -- a deck's name or "offboard",
   * both of which Map.locationName() can return, parse nowhere.
   */
  @Test
  public void gridLocationOf_isNullWhenTheGridDoesNotPlaceTheName() throws BadCoords {
    final MapGrid mg = mock(MapGrid.class);
    when(mg.getLocation("Discard Pile")).thenReturn(null);

    assertNull(ADC2Module.gridLocationOf(mg, "Discard Pile"));
  }

  /**
   * An unparseable name was already non-fatal, logged and skipped. It still is
   * -- that arm is why throwing for a mismatch was the odd one out.
   */
  @Test
  public void gridLocationOf_isNullForBadCoords() throws BadCoords {
    final MapGrid mg = mock(MapGrid.class);
    when(mg.getLocation("offboard")).thenThrow(new BadCoords());

    assertNull(ADC2Module.gridLocationOf(mg, "offboard"));
  }
}
