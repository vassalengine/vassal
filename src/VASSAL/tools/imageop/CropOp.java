package VASSAL.tools.imageop;

import java.awt.Rectangle;

public interface CropOp extends ImageOp {

  public Rectangle getRect();

  public int getX0();

  public int getY0();

  public int getX1();

  public int getY1();
}
