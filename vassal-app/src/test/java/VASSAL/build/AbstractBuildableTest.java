package VASSAL.build;

import VASSAL.build.module.PredefinedSetup;
import org.junit.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

/**
 * This test was created to test the bugfix for bug 554: AbstractBuildable.addComponents() fails to recurse properly
 * https://github.com/vassalengine/vassal/issues/554
 */
public class AbstractBuildableTest extends MockModuleTest {
  @Test
  public void Test1() {
    final PredefinedSetup pds1 = new PredefinedSetup();
    final PredefinedSetup pds2 = new PredefinedSetup();
    final PredefinedSetup pds3 = new PredefinedSetup();
    pds1.add(pds2);
    pds2.add(pds3);

    final List<PredefinedSetup> pdsList = pds1.getAllDescendantComponentsOf(PredefinedSetup.class);
    final int expected = 3;
    assertEquals(expected, pdsList.size());
  }
}
