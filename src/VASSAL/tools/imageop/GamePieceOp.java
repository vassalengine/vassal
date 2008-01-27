package VASSAL.tools.imageop;

import VASSAL.counters.GamePiece;

public interface GamePieceOp extends ImageOp {
  public GamePiece getPiece();

  public String getState();

  public boolean isChanged();
}
