package VASSAL.build.module.index;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.tools.DataArchive;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.Set;

public class PiecePropertyIndexTest {

  private static final String PROP = "propName";
  private int pieceId = 1;
  @Test
  public void indexTest() {

    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {

      // Mock DataArchive to return a list of image names
      final DataArchive da = mock(DataArchive.class);
      when(da.getImageNames()).thenReturn(new String[0]);

      // Mock some GpID Support
      final GpIdSupport gpid = mock(GpIdSupport.class);

      // Mock GameModule to return various resources
      final GameModule gm = mock(GameModule.class);
      when(gm.getDataArchive()).thenReturn(da);
      when(gm.getGpIdSupport()).thenReturn(gpid);

      staticGm.when(GameModule::getGameModule).thenReturn(gm);

      final PiecePropertyIndex index = new PiecePropertyIndex(PROP);
      final BasicPiece bp1 = new IndexPiece("1");
      final BasicPiece bp2 = new IndexPiece("1");
      final BasicPiece bp3 = new IndexPiece("2");

      index.addOrUpdatePiece(bp1);
      index.addOrUpdatePiece(bp2);
      index.addOrUpdatePiece(bp3);

      Set<GamePiece> pieces = index.getPieces("1");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(2));
      assertThat(pieces.contains(bp1), is(true));
      assertThat(pieces.contains(bp2), is(true));
      assertThat(pieces.contains(bp3), is(false));

      pieces = index.getPieces("2");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(1));
      assertThat(pieces.contains(bp1), is(false));
      assertThat(pieces.contains(bp2), is(false));
      assertThat(pieces.contains(bp3), is(true));

      pieces = index.getPieces("3");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(0));

      bp1.setProperty(PROP, "2");
      index.addOrUpdatePiece(bp1);

      pieces = index.getPieces("1");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(1));
      assertThat(pieces.contains(bp1), is(false));
      assertThat(pieces.contains(bp2), is(true));
      assertThat(pieces.contains(bp3), is(false));

      pieces = index.getPieces("2");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(2));
      assertThat(pieces.contains(bp1), is(true));
      assertThat(pieces.contains(bp2), is(false));
      assertThat(pieces.contains(bp3), is(true));

      pieces = index.getPieces("3");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(0));

      index.removePiece(bp2);

      pieces = index.getPieces("1");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(0));
      assertThat(pieces.contains(bp1), is(false));
      assertThat(pieces.contains(bp2), is(false));
      assertThat(pieces.contains(bp3), is(false));

      pieces = index.getPieces("2");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(2));
      assertThat(pieces.contains(bp1), is(true));
      assertThat(pieces.contains(bp2), is(false));
      assertThat(pieces.contains(bp3), is(true));

      index.removePiece(bp1);
      pieces = index.getPieces("1");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(0));
      assertThat(pieces.contains(bp1), is(false));
      assertThat(pieces.contains(bp2), is(false));
      assertThat(pieces.contains(bp3), is(false));

      pieces = index.getPieces("2");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(1));
      assertThat(pieces.contains(bp1), is(false));
      assertThat(pieces.contains(bp2), is(false));
      assertThat(pieces.contains(bp3), is(true));

      index.removePiece(bp3);
      pieces = index.getPieces("1");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(0));
      assertThat(pieces.contains(bp1), is(false));
      assertThat(pieces.contains(bp2), is(false));
      assertThat(pieces.contains(bp3), is(false));

      pieces = index.getPieces("2");
      assertThat(pieces, is(notNullValue()));
      assertThat(pieces.size(), is(0));
      assertThat(pieces.contains(bp1), is(false));
      assertThat(pieces.contains(bp2), is(false));
      assertThat(pieces.contains(bp3), is(false));
    }
  }

  private class IndexPiece extends BasicPiece {

    private String value;

    public IndexPiece(String propValue) {
      value = propValue;
      setProperty(Properties.PIECE_ID, String.valueOf(pieceId));
      setId(String.valueOf(pieceId));
      pieceId++;
    }

    @Override
    public Object getProperty(Object key) {
      if (PROP.equals(key)) {
        return value;
      }
      return super.getProperty(key);
    }

    @Override
    public void setProperty(Object key, Object val) {
      if (PROP.equals(key)) {
        value = (String) val;
      }
      else {
        super.setProperty(key, val);
      }
    }
  }

}