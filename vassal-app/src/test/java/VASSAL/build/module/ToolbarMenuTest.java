package VASSAL.build.module;

import VASSAL.build.GameModule;
import VASSAL.build.module.folder.MapSubFolder;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ToolbarMenuTest {

  @Test
  public void removeFromShouldWorkWithoutToolbar() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getGameState()).thenReturn(mock(GameState.class));

      // A folder with no ancestor yet, as when the Editor builds a pasted
      // folder before inserting it, so addTo() finds no toolbar.
      final MapSubFolder folder = new MapSubFolder();
      final ToolbarMenu menu = new ToolbarMenu();
      menu.addTo(folder);

      assertDoesNotThrow(() -> menu.removeFrom(folder));
    }
  }
}
