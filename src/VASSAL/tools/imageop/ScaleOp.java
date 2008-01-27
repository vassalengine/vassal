package VASSAL.tools.imageop;

import java.awt.RenderingHints;

public interface ScaleOp extends ImageOp {
  
  public double getScale();

  public RenderingHints getHints();
}
