package VASSAL.tools.nio.file;

import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * A base class for parametrized tests.
 *
 * @author Joel Uckelman
 * @since 3.2
 */
public abstract class AbstractMethodTest {
  protected final Object expected;
  protected final Class<? extends Throwable> tclass;

  public AbstractMethodTest(Object expected) {
    if (expected instanceof Throws) {
      this.expected = null;
      this.tclass = ((Throws) expected).tclass;
    }
    else {
      this.expected = expected;
      this.tclass = null;
    }
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
