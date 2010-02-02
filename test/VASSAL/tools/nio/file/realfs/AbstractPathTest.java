package VASSAL.tools.nio.file.realfs;

import java.io.IOException;

import org.junit.Test;

public abstract class AbstractPathTest {

  @Test
  public abstract void testCompareTo();

  @Test
  public abstract void testCopyTo() throws IOException;

  @Test
  public abstract void testCreateLink() throws IOException;

  @Test
  public abstract void testCreateSymbolicLink() throws IOException;

  @Test
  public abstract void testReadSymbolicLink() throws IOException;

  @Test
  public abstract void testRegisterWatchServiceKindOfQArray()
                                                            throws IOException;

  @Test
  public abstract void testRegisterWatchServiceKindOfQArrayModifierArray()
                                                            throws IOException;

}
