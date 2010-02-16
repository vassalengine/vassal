package VASSAL.tools.nio.file;

import org.junit.After;
import org.junit.Before;

public abstract class AbstractPathMethodTest extends AbstractMethodTest {
  protected FileSystem fs;
  protected final FSHandler fac;

  public AbstractPathMethodTest(FSHandler fac, Object expected) {
    super(expected);
    this.fac =  fac;
  }

  @Before
  public void setupFS() throws Throwable {
    fs = fac.setup();
  }

  @After
  public void teardownFS() throws Throwable {
    fac.teardown(fs);
  }
}
