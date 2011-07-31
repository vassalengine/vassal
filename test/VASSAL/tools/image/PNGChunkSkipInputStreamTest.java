package VASSAL.tools.image;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;

import VASSAL.tools.io.IOUtils;

import org.junit.*;
import static org.junit.Assert.*;

/**
 * @author Joel Uckelman
 * @since 3.2.0
 */
public class PNGChunkSkipInputStreamTest {
  private static String iTXt    = "test/VASSAL/tools/image/iTXt.png";
  private static String no_iTXt = "test/VASSAL/tools/image/no-iTXt.png";

  /**
   * Tests whether iTXt chunks are successfully skipped without losing
   * any other image data.
   */
  @Test
  public void testSkipiTXt() throws IOException {

    final InputStream ein = new FileInputStream(no_iTXt);
    final byte[] expected = IOUtils.toByteArray(ein);
    ein.close();

    final InputStream rin =
      new PNGChunkSkipInputStream(Collections.singleton(PNGDecoder.iTXt),
      new BufferedInputStream(new FileInputStream(iTXt))
    );
    final byte[] result = IOUtils.toByteArray(rin);
    rin.close();

    assertArrayEquals(expected, result);
  }
}
