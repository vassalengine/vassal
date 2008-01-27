package VASSAL.tools.imageop;

import java.awt.Image;
import java.awt.Rectangle;

import VASSAL.counters.GamePiece;

public class Op {
  public static SourceOp load(String name) {
    if (name.endsWith(".svg"))
      return new SourceOpSVGImpl(name);
    else
      return new SourceOpBitmapImpl(name);
  }

  public static SourceOp load(Image image) {
    return new ImageSourceOpBitmapImpl(image);
  }
  
  public static ScaleOp scale(ImageOp sop, double scale) {
    if (sop instanceof SVGOp)
      return new RotateScaleOpSVGImpl((SVGOp) sop, 0.0, scale);
    else 
      return new ScaleOpBitmapImpl(sop, scale);
  }

  public static RotateOp rotate(ImageOp sop, double angle) {
    if (sop instanceof SVGOp) 
      return new RotateScaleOpSVGImpl((SVGOp) sop, angle, 1.0);
    else if (angle % 90.0 == 0.0)
      return new OrthoRotateOpBitmapImpl(sop, (int) angle);
    else 
      return new RotateScaleOpBitmapImpl(sop, angle, 1.0);
  }

  public static RotateScaleOp rotateScale(ImageOp sop,
                                          double angle, double scale) {
    if (sop instanceof SVGOp)
      return new RotateScaleOpSVGImpl((SVGOp) sop, angle, scale);
    else 
      return new RotateScaleOpBitmapImpl(sop, angle, scale);
  }

  public static CropOp crop(ImageOp sop, int x0, int y0, int x1, int y1) {
    return new CropOpBitmapImpl(sop, x0, y0, x1, y1);
  }

  public static GamePieceOp piece(GamePiece gp) {
    return new GamePieceOpImpl(gp);
  } 

  public static void clearCache() {
    AbstractOpImpl.clearCache();
  }
}
