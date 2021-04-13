package VASSAL.tools.image.svg;

import java.awt.Dimension;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.stream.Stream;

import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.lang.Pair;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static org.junit.jupiter.api.Assertions.*;

public class SVGImageUtilsTest {

  private static final String SVG = "<svg xmlns=\"http://www.w3.org/2000/svg\" %s %s %s></svg>";
  private static final String WH = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\"></svg>";

  @Test
  void garbage() {
    ByteArrayInputStream in = new ByteArrayInputStream("bogus".getBytes(StandardCharsets.UTF_8));
    assertThrows(ImageIOException.class, () -> SVGImageUtils.getImageSize("test.svg", in));
  }

  @ParameterizedTest
  @MethodSource("addFixture")
  void getSize(String w, String h, String vb, Object exp) throws IOException {
    String svg = String.format(SVG,
      w == null ? "" : "width=\"" + w + "\"",
      h == null ? "" : "height=\"" + h + "\"",
      vb == null ? "" : "viewBox=\"" + vb + "\"");

    ByteArrayInputStream in = new ByteArrayInputStream(svg.getBytes(StandardCharsets.UTF_8));

    if (exp instanceof Throwable) {
      Throwable t = assertThrows(Throwable.class, () -> SVGImageUtils.getImageSize("test.svg", in));
      assertTrue(exp.getClass().isAssignableFrom(t.getClass()));
    } else {
      assertEquals(exp, SVGImageUtils.getImageSize("test.svg", in));
    }
  }

  private static Stream<Arguments> addFixture() {
    return Stream.of(
        Arguments.of( null,  null,  "0 0 100 200",     new Dimension(100, 200) ),
        Arguments.of( null,  null,  "0 0 100.1 200.5", new Dimension(100, 201) ),
        Arguments.of( null,  null,  "3 8 100 200",     new Dimension(100, 200) ),
        Arguments.of( "20",  "30",  "0 0 0 0",         new Dimension(20, 30)   ),
        Arguments.of( "1",   "2",   null,              new Dimension(1, 2)     ),
        // missing width, height and empty view box gives empty imag
        Arguments.of( null,  null,  "0 0 0 0",         new Dimension(0, 0)     ),
        Arguments.of( null,  null,  "0 0 1 0",         new Dimension(0, 0)     ),
        Arguments.of( null,  null,  "0 0 0 1",         new Dimension(0, 0)     ),
        // missing dimension picked up from viewBox
        Arguments.of( "30",  null,  "0 0 50 60",       new Dimension(30, 60)   ),
        Arguments.of( null,  "30",  "0 0 50 60",       new Dimension(50, 30)   ),
        Arguments.of( "30",  null,  "0 0 0 0",         new Dimension(30, 0)    ),
        Arguments.of( null,  "30",  "0 0 0 0",         new Dimension(0, 30)    ),
        // given width, height always used
        Arguments.of( "0",   "0",   null,              new Dimension(0, 0)     ),
        Arguments.of( "1",   "0",   "0 0 50 60",       new Dimension(1, 0)     ),
        Arguments.of( "0",   "1",   "0 0 50 60",       new Dimension(0, 1)     ),
        // percentage width, height gives results from viewBox
        Arguments.of( "100%", "100%", "0 0 50 60",     new Dimension(50, 60)   ),
        Arguments.of( "20%",  "50%",  "0 0 50 60",     new Dimension(10, 30)   ),
        // missing one dimension results in a square
        Arguments.of( "30",  null,  null,              new Dimension(30, 30)   ),
        Arguments.of( null,  "30",  null,              new Dimension(30, 30)   ),
        // missing all dimensions gives empty image
        Arguments.of( null,  null,  null,              new Dimension(0, 0)     ),
        // any bogus value throws
        Arguments.of( "bob", "1",   null,              new IOException()       ),
        Arguments.of( "1",   "bob", null,              new IOException()       ),
        Arguments.of( "1",   "2",   "bob",             new IOException()       ),
        Arguments.of( null,  null,  "bob",             new IOException()       ),
        Arguments.of( null,  null,  "0 0 13 bob",      new IOException()       ),
        Arguments.of( null,  null,  "0 0 -1 0",        new IOException()       ));
  }

  @ParameterizedTest
  @MethodSource("data")
  void getSize(String aw, int ew, String ah, int eh) throws IOException {
    String svg = String.format(WH, aw, ah);
    ByteArrayInputStream in = new ByteArrayInputStream(svg.getBytes(StandardCharsets.UTF_8));

    assertEquals(new Dimension(ew, eh), SVGImageUtils.getImageSize("test.svg", in));
  }

  private static Stream<Arguments> data() {
    List<Pair<String, Integer>> lengths = List.of(
      Pair.of("87",     87),
      Pair.of("87px",   87),
      Pair.of("190",   190),
      Pair.of("190px", 190),
      Pair.of("1in",    96),
      Pair.of("72pt",   96),
      Pair.of("72pt",   96),
      Pair.of("6pc",    96),
      Pair.of("2.54cm", 96),
      Pair.of("25.4mm", 96),
      Pair.of("98.6",   99),
      Pair.of("98.4",   98)
    );

    // make the cartesian product as a stream
    return lengths.stream().flatMap(
      a -> lengths.stream().map(
        b -> Arguments.of(a.first, a.second, b.first, b.second)
      )
    );
  }

}
