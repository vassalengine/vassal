package VASSAL.build.module.map.boardPicker.board.mapgrid;

import VASSAL.configure.VisibilityCondition;

public abstract class AbstractUIHexGridNumberingX extends AbstractHexGridNumbering {
  protected Boolean oblique = false;
  protected Boolean stagger = true;
  protected Boolean nw_se = false;

  public static final String OBLIQUE = "oblique";
  public static final String STAGGER = "stagger";
  public static final String NW_SE = "nw_se";

  public AbstractUIHexGridNumberingX() 
      throws NumberFormatException, IllegalArgumentException, SecurityException  {
    super();  
    addAttribute(OBLIQUE, "Use oblique (set) or rectangular (clear) numbering?");
    addAttribute(STAGGER, "Odd-numbered rows numbered higher",
        new VisibilityCondition() {
          @Override public boolean shouldBeVisible() { return !oblique; }
      });
    addAttribute(NW_SE, "Oblique axis runs NW to SE (instead of SW to NE).",
        new VisibilityCondition() {
        @Override public boolean shouldBeVisible() { return oblique; }
      });
  }
}
