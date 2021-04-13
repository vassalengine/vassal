package VASSAL.counters;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.Test;

public class GlobalCommandTargetTest extends DecoratorTest {

  @Test
  public void Tests() {
    GlobalCommandTarget target = new GlobalCommandTarget();

    target.setGKCtype(GlobalCommandTarget.GKCtype.COUNTER);
    target.setFastMatchLocation(true);
    target.setTargetType(GlobalCommandTarget.Target.MAP);
    target.setTargetMap("map");
    target.setTargetBoard("board");
    target.setTargetZone("zone");
    target.setTargetLocation("location");
    target.setTargetX(42);
    target.setTargetY(84);
    target.setTargetDeck("deck");
    target.setFastMatchProperty(true);
    target.setTargetProperty("prop");
    target.setTargetValue("value");

    String code = target.encode();

    GlobalCommandTarget target2 = new GlobalCommandTarget();
    target2.decode(code);

    assertThat("String values", target, is(equalTo(target2)));

    target = new GlobalCommandTarget();

    target.setGKCtype(GlobalCommandTarget.GKCtype.COUNTER);
    target.setFastMatchLocation(true);
    target.setTargetType(GlobalCommandTarget.Target.MAP);
    target.setTargetMap("{map+1}");
    target.setTargetBoard("{board+1}");
    target.setTargetZone("{zone+1}");
    target.setTargetLocation("{location+2}");
    target.setTargetX(42);
    target.setTargetY(84);
    target.setTargetDeck("{deck+1}");
    target.setFastMatchProperty(true);
    target.setTargetProperty("prop");
    target.setTargetValue("{value+2}");

    code = target.encode();

    target2 = new GlobalCommandTarget();
    target2.decode(code);

    assertThat("Expression values", target, is(equalTo(target2)));



  }
}