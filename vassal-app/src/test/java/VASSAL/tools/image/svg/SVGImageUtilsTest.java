package VASSAL.tools.image.svg;

import java.awt.Dimension;
import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

import org.apache.commons.io.IOUtils;

import VASSAL.tools.image.ImageIOException;
import VASSAL.tools.lang.Pair;

import static org.junit.Assert.*;
import org.junit.Test;
import org.junit.experimental.runners.Enclosed;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

@RunWith(Enclosed.class)
public class SVGImageUtilsTest {

  public static class GetSizeBadInputTest {
    @Test(expected = ImageIOException.class)
    public void testGarbage() throws IOException {
      final ByteArrayInputStream in = new ByteArrayInputStream(
        "bogus".getBytes(StandardCharsets.UTF_8)
      );
      SVGImageUtils.getImageSize("test.svg", in);
    }
  }

  @RunWith(Parameterized.class)
  public static class GetSizeUnitsTest {
    private final String aw;
    private final String ah;
    private final int ew;
    private final int eh;

    public GetSizeUnitsTest(String aw, int ew, String ah, int eh) {
      this.aw = aw;
      this.ah = ah;
      this.ew = ew;
      this.eh = eh;
    }

    private static final String WH = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\"></svg>";

    @Test
    public void testGetSize() throws IOException {
      final String svg = String.format(WH, aw, ah);
      final ByteArrayInputStream in = new ByteArrayInputStream(
        svg.getBytes(StandardCharsets.UTF_8)
      );

      assertEquals(
        new Dimension(ew, eh),
        SVGImageUtils.getImageSize("test.svg", in)
      );
    }

    @Parameters
    public static Iterable<Object[]> data() {
      final List<Pair<String, Integer>> lengths = List.of(
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
      final Stream<Object[]> op = lengths.stream().flatMap(
        a -> lengths.stream().map(
          b -> new Object[]{a.first, a.second, b.first, b.second}
        )
      );

      return (Iterable<Object[]>) op::iterator;
    }
  }

  @RunWith(Parameterized.class)
  public static class GetSizeTest {
    private final String w;
    private final String h;
    private final String vb;
    private final Object exp;

    public GetSizeTest(String w, String h, String vb, Object exp) {
      this.w = w;
      this.h = h;
      this.vb = vb;
      this.exp = exp;
    }

    private static final String SVG = "<svg xmlns=\"http://www.w3.org/2000/svg\" %s %s %s></svg>";

    @Test
    public void testGetSize() throws IOException {
      final String svg = String.format(
        SVG,
        w == null ? "" : "width=\"" + w + "\"",
        h == null ? "" : "height=\"" + h + "\"",
        vb == null ? "" : "viewBox=\"" + vb + "\""
      );
      final ByteArrayInputStream in = new ByteArrayInputStream(
        svg.getBytes(StandardCharsets.UTF_8)
      );

      if (exp instanceof Throwable) {
        try {
          SVGImageUtils.getImageSize("test.svg", in);
          fail("Expected " + exp);
        }
        catch (Throwable t) {
          assertTrue(exp.getClass().isAssignableFrom(t.getClass()));
        }
      }
      else {
        assertEquals(
          exp,
          SVGImageUtils.getImageSize("test.svg", in)
        );
      }
    }

    @Parameters
    public static Iterable<Object[]> data() {
      return Arrays.asList(new Object[][]{
        { null,  null,  "0 0 100 200",     new Dimension(100, 200) },
        { null,  null,  "0 0 100.1 200.5", new Dimension(100, 201) },
        { null,  null,  "3 8 100 200",     new Dimension(100, 200) },
        { "20",  "30",  "0 0 0 0",         new Dimension(20, 30)   },
        { "1",   "2",   null,              new Dimension(1, 2)     },
        // missing width, height and empty view box gives empty image
        { null,  null,  "0 0 0 0",         new Dimension(0, 0)     },
        { null,  null,  "0 0 1 0",         new Dimension(0, 0)     },
        { null,  null,  "0 0 0 1",         new Dimension(0, 0)     },
        // missing dimension picked up from viewBox
        { "30",  null,  "0 0 50 60",       new Dimension(30, 60)   },
        { null,  "30",  "0 0 50 60",       new Dimension(50, 30)   },
        { "30",  null,  "0 0 0 0",         new Dimension(30, 0)    },
        { null,  "30",  "0 0 0 0",         new Dimension(0, 30)    },
        // given width, height always used
        { "0",   "0",   null,              new Dimension(0, 0)     },
        { "1",   "0",   "0 0 50 60",       new Dimension(1, 0)     },
        { "0",   "1",   "0 0 50 60",       new Dimension(0, 1)     },
        // percentage width, height gives results from viewBox
        { "100%", "100%", "0 0 50 60",     new Dimension(50, 60)   },
        { "20%",  "50%",  "0 0 50 60",     new Dimension(10, 30)   },
        // missing one dimension results in a square
        { "30",  null,  null,              new Dimension(30, 30)   },
        { null,  "30",  null,              new Dimension(30, 30)   },
        // missing all dimensions gives empty image
        { null,  null,  null,              new Dimension(0, 0)     },
        // any bogus value throws
        { "bob", "1",   null,              new IOException()       },
        { "1",   "bob", null,              new IOException()       },
        { "1",   "2",   "bob",             new IOException()       },
        { null,  null,  "bob",             new IOException()       },
        { null,  null,  "0 0 13 bob",      new IOException()       },
        { null,  null,  "0 0 -1 0",        new IOException()       },
      });
    }
  }
}
