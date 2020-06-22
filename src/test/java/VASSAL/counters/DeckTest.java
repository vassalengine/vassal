package VASSAL.counters;

import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.*;

import VASSAL.build.GameModule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;

@RunWith(MockitoJUnitRunner.class)
public class DeckTest {

  @Test
  public void defaultConstructorShouldConstruct() {
    // prepare
    final GameModule gameModule = mock(GameModule.class);

    // run
    Deck d = new Deck(gameModule);

    // assert
    assertNotNull(d);
  }

}
