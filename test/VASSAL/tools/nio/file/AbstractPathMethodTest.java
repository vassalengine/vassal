package VASSAL.tools.nio.file;

import static org.junit.Assert.fail;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public abstract class AbstractPathMethodTest {
  protected FileSystem fs;
  protected final Object expected;
  protected final Class<? extends Throwable> tclass;
  protected final FSHandler fac;

  public AbstractPathMethodTest(FSHandler fac, Object expected) {
    this.fac =  fac;

    if (expected instanceof Throws) {
      this.expected = null;
      this.tclass = ((Throws) expected).tclass;
    }
    else {
      this.expected = expected;
      this.tclass = null;
    }
  }

  @Before
  public void setupFS() throws Throwable {
    fs = fac.setup();
  }

  @After
  public void teardownFS() throws Throwable {
    fac.teardown(fs);
  }
 
  protected abstract void doTest() throws Throwable;
 
  @Test
  public void test() throws Throwable {
    if (tclass == null) {
      // We are not expecting an exception.
      doTest();
    }
    else {
      // We are expecting an exception of type tclass.
      try {
        doTest();
      }
      catch (Throwable t) {
        // We still fail on exception of the wrong type.
        if (tclass.isInstance(t)) return;
        else throw t;
      }

      // We didn't see the expected exception.
      fail("Did not throw " + tclass.getSimpleName() + "!");
    }
  }

  protected static class Throws {
    public final Class<? extends Throwable> tclass;

    public Throws(Class<? extends Throwable> tclass) {
      this.tclass = tclass;
    }
  }

  public static Throws t(Class<? extends Throwable> tclass) {
    return new Throws(tclass);
  }
}
