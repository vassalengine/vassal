package VASSAL.tools.imageop;

import java.awt.RenderingHints;

public interface RotateOp extends ImageOp {

  public double getAngle();

  public RenderingHints getHints();
}
