package VASSAL.build;

import VASSAL.tools.DataArchive;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@Disabled
public class MockModuleTest {
  private static boolean initialized = false;

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void init() throws Exception {
    if (initialized) {
      if (GameModule.getGameModule() == null) {
        int x = 5 / 0;
      }
      return;
    }

    final GameModule module = mock(GameModule.class);
    final DataArchive arch = mock(DataArchive.class);

    when(module.getDataArchive()).thenReturn(arch);

    GameModule.init(module);
    if (GameModule.getGameModule() == null) {
      int x = 5 / 0;
    }
    initialized = true;
  }
}
