package VASSAL.build;

import VASSAL.tools.DataArchive;

import org.junit.Before;
import org.junit.Ignore;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@Ignore
public class MockModuleTest {
  private static boolean initialized = false;

  @SuppressWarnings("unchecked")
  @Before
  public void init() throws Exception {
    if (initialized) {
      return;
    }

    final GameModule module = mock(GameModule.class);
    final DataArchive arch = mock(DataArchive.class);

    when(module.getDataArchive()).thenReturn(arch);

    GameModule.init(module);
    initialized = true;
  }
}
