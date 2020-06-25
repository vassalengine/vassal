package VASSAL.build;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;

@Ignore
public class MockModuleTest {
  private static boolean initialized = false;

  protected Mockery context = new Mockery() {
    {
      setImposteriser(ClassImposteriser.INSTANCE);
    }
  };

  @SuppressWarnings("unchecked")
  @Before
  public void init() throws Exception {
    if (initialized) {
      return;
    }
    final GameModule module = context.mock(GameModule.class);
    context.checking(new Expectations() {
      {
        allowing(module).setGpIdSupport(with(any(GameModule.class)));
        allowing(module).build();
        allowing(module).getDataArchive();
        allowing(module).getComponentsOf(with(any(Class.class)));
//        allowing(module).sliceLargeImages();
        allowing(module).getFrame();
      }
    });
    GameModule.init(module);
    initialized = true;
  }

  @After
  public void assertContextSatisfied() {
    context.assertIsSatisfied();
  }
}
