package VASSAL.build.module;

import VASSAL.counters.GamePiece;
import java.util.List;

public interface Refresher {
  List<GamePiece> getRefreshedPieces();
}
