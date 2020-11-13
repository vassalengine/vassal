package VASSAL.build.module;

import VASSAL.build.GameModule;
import VASSAL.build.MockModuleTest;
import VASSAL.tools.perl.Helper;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;


import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class PredefinedSetupTest {
    static final String MODNAME = "src/test/resources/test-predefined-setups.vmod";
    static GameModule mod = null;

    @BeforeClass
    public static void setup() {
        Helper.doInitGameModule(MODNAME);
        mod = GameModule.getGameModule();
    }

    @Test
    public void Test1() {
        assertNotNull(mod);
    }

    /**
     * This code is to test Bug 554 - AbstractBuildable.addComponents() fails to recurse properly
     * https://github.com/vassalengine/vassal/issues/554
     */
    @Test
    public void Test2() {
        final int numSetups = 4;
        final int numWindows = 1;
        assertNotNull(mod);

        List<PieceWindow> pwList = mod.getAllDescendantComponentsOf(PieceWindow.class);
        assertEquals(numWindows, pwList.size());
        List<PredefinedSetup> pdsList = mod.getAllDescendantComponentsOf(PredefinedSetup.class);
        assertEquals(numSetups, pdsList.size());

    }
}