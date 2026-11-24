package VASSAL.tools.imports.adc2;

import VASSAL.build.GameModule;
import VASSAL.build.module.PlayerRoster;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
}
