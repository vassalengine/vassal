package VASSAL.counters;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;


public class ImmobilizedSerializeTest extends SerializeTest<Immobilized> {

  @Test
  public void serialize011() throws Exception {
    Immobilized immob = new Immobilized();
    // radio
    immob.shiftToSelect = false;
    immob.altToSelect = false;
    immob.neverSelect = false;

    immob.ignoreGrid = true;

    // radio
    immob.neverMove = true;
    immob.moveIfSelected = false;
    super.serializeTest(Immobilized.class, immob);
  }

  @Test
  public void serialize111() throws Exception {
    Immobilized immob = new Immobilized();
    // radio
    immob.shiftToSelect = true;
    immob.altToSelect = false;
    immob.neverSelect = false;

    immob.ignoreGrid = true;

    // radio
    immob.neverMove = true;
    immob.moveIfSelected = false;
    super.serializeTest(Immobilized.class, immob);
  }

  @Test
  public void serialize211() throws Exception {
    Immobilized immob = new Immobilized();
    // radio
    immob.shiftToSelect = false;
    immob.altToSelect = true;
    immob.neverSelect = false;

    immob.ignoreGrid = true;

    // radio
    immob.neverMove = true;
    immob.moveIfSelected = false;
    super.serializeTest(Immobilized.class, immob);
  }

  @Test
  public void serialize311() throws Exception {
    Immobilized immob = new Immobilized();
    // radio
    immob.shiftToSelect = false;
    immob.altToSelect = false;
    immob.neverSelect = true;

    immob.ignoreGrid = true;

    // radio
    immob.neverMove = true;
    immob.moveIfSelected = false;
    super.serializeTest(Immobilized.class, immob);
  }

  @Test
  public void serialize312() throws Exception {
    Immobilized immob = new Immobilized();
    // radio
    immob.shiftToSelect = false;
    immob.altToSelect = false;
    immob.neverSelect = true;

    immob.ignoreGrid = true;

    // radio
    immob.neverMove = false;
    immob.moveIfSelected = true;
    super.serializeTest(Immobilized.class, immob);
  }

  @Test
  public void serialize310() throws Exception {
    Immobilized immob = new Immobilized();
    // radio
    immob.shiftToSelect = false;
    immob.altToSelect = false;
    immob.neverSelect = true;

    immob.ignoreGrid = true;

    // radio
    immob.neverMove = false;
    immob.moveIfSelected = false;
    super.serializeTest(Immobilized.class, immob);
  }

  @Test
  public void serialize300() throws Exception {
    Immobilized immob = new Immobilized();
    // radio
    immob.shiftToSelect = false;
    immob.altToSelect = false;
    immob.neverSelect = true;

    immob.ignoreGrid = false;

    // radio
    immob.neverMove = false;
    immob.moveIfSelected = false;
    super.serializeTest(Immobilized.class, immob);
  }

  @Override
  void assertSame(Immobilized immob1, Immobilized immob2) {
    assertEquals(immob1.shiftToSelect, immob2.shiftToSelect);
    assertEquals(immob1.altToSelect, immob2.altToSelect);
    assertEquals(immob1.neverSelect, immob2.neverSelect);
    assertEquals(immob1.ignoreGrid, immob2.ignoreGrid);
    assertEquals(immob1.neverMove, immob2.neverMove);
    assertEquals(immob1.moveIfSelected, immob2.moveIfSelected);
  }
}
