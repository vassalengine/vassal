package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.imageop.GamePieceOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.RotateScaleOp;

import static VASSAL.counters.BasicPiece.BASIC_NAME;
import static VASSAL.counters.BasicPiece.CURRENT_BOARD;
import static VASSAL.counters.BasicPiece.CURRENT_MAP;
import static VASSAL.counters.BasicPiece.CURRENT_ZONE;
import static VASSAL.counters.BasicPiece.LOCATION_NAME;
import static VASSAL.counters.BasicPiece.PIECE_NAME;

/**
 * Designates the piece as "Cargo", which can be placed on a "Mat" to move along with it
 */
public class MatCargo extends Decorator implements TranslatablePiece {
  public static final String ID = "matPiece;"; // NON-NLS
  public static final String NO_MAT = "noMat"; //NON-NLS

  public static final String CURRENT_MAT    = "CurrentMat"; //NON-NLS     // Exposed property giving our current mat or "null"
  public static final String CURRENT_MAT_ID = "CurrentMatID"; //NON-NLS   // Exposed property giving our current mat + uniqueID or "null"
  public static final String CURRENT_MAT_X  = "CurrentMatX"; //NON-NLS    // Exposed property giving X position of our current mat
  public static final String CURRENT_MAT_Y  = "CurrentMatY"; //NON-NLS    // Exposed property giving Y position of our current mat
  public static final String CURRENT_MAT_OFFSET_X  = "CurrentMatOffsetX"; //NON-NLS    // Exposed property giving X offset to our current mat
  public static final String CURRENT_MAT_OFFSET_Y  = "CurrentMatOffsetY"; //NON-NLS    // Exposed property giving Y offset to our current mat
  public static final String CURRENT_MAT_PIECE_NAME = "CurrentMatPieceName"; // NON-NLS // Exposed property giving PieceName of our current mat
  public static final String CURRENT_MAT_BASIC_NAME = "CurrentMatBasicName"; // NON-NLS // Exposed property giving BasicName of our current mat
  public static final String CURRENT_MAT_LOCATION_NAME = "CurrentMatLocationName"; //NON-NLS
  public static final String CURRENT_MAT_ZONE = "CurrentMatZone"; //NON-NLS
  public static final String CURRENT_MAT_BOARD = "CurrentMatBoard"; //NON-NLS
  public static final String CURRENT_MAT_MAP = "CurrentMatMap"; //NON-NLS
  public static final String CURRENT_MAT_PROP0 = "CurrentMatProp0"; //NON-NLS // These expose identically named properties of the current mat (e.g. I would receive CurrentMatProp0 **of this piece's current map**
  public static final String CURRENT_MAT_PROP1 = "CurrentMatProp1"; //NON-NLS
  public static final String CURRENT_MAT_PROP2 = "CurrentMatProp2"; //NON-NLS
  public static final String CURRENT_MAT_PROP3 = "CurrentMatProp3"; //NON-NLS
  public static final String CURRENT_MAT_PROP4 = "CurrentMatProp4"; //NON-NLS
  public static final String CURRENT_MAT_PROP5 = "CurrentMatProp5"; //NON-NLS
  public static final String CURRENT_MAT_PROP6 = "CurrentMatProp6"; //NON-NLS
  public static final String CURRENT_MAT_PROP7 = "CurrentMatProp7"; //NON-NLS
  public static final String CURRENT_MAT_PROP8 = "CurrentMatProp8"; //NON-NLS
  public static final String CURRENT_MAT_PROP9 = "CurrentMatProp9"; //NON-NLS
  public static final String IS_CARGO       = "IsCargo"; //NON-NLS        // Exposed property returns "true"

  protected String desc;

  protected boolean maintainRelativeFacing;
  protected int detectionDistanceX;
  protected int detectionDistanceY;
  protected GamePieceOp gpOp;
  protected java.util.Map<Double, Rectangle> bounds = new HashMap<>();
  protected java.util.Map<Double, RotateScaleOp> rotOp = new HashMap<>();

  protected static final double PI_180 = Math.PI / 180.0;

  protected GamePiece mat; // Mat piece we are assigned to, or null

  public MatCargo() {
    this(ID + ";true", null); //NON-NLS
  }

  public MatCargo(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    desc = st.nextToken();
    maintainRelativeFacing = st.nextBoolean(true);
    detectionDistanceX = st.nextInt(0);
    detectionDistanceY = st.nextInt(0);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(desc)
      .append(maintainRelativeFacing)
      .append(detectionDistanceX)
      .append(detectionDistanceY);
    return ID + se.getValue();
  }

  /**
   * Clear our relationship with any Mat we're assigned to. Does NOT clear the Mat's reference to us -- must be done separately.
   */
  public void clearMat() {
    mat = null;
  }

  /**
   * Clear our relationship with any Mat we're assigned to, and return a command to do that.
   * @return return a command to clear our Mat relationship
   */
  public Command makeClearMatCommand() {
    if (mat == null) {
      return new NullCommand();
    }

    ChangeTracker ctMat = null;
    final Mat actualMat = (Mat)Decorator.getDecorator(Decorator.getOutermost(mat), Mat.class);
    if (actualMat != null) {
      ctMat = new ChangeTracker(actualMat);
      actualMat.removeCargo(Decorator.getOutermost(this));
    }

    final ChangeTracker ct = new ChangeTracker(this);

    clearMat();

    Command c = ct.getChangeCommand();
    if (ctMat != null) {
      c = c.append(ctMat.getChangeCommand());
    }

    return c;
  }

  /**
   * Mark us as being on a specific Mat piece
   * @param mat the mat we are joining
   */
  public void setMat(GamePiece mat) {
    this.mat = mat;
    if (mat != null) {
      final GamePiece actualMat = Decorator.getDecorator(Decorator.getOutermost(mat), Mat.class);
      if (actualMat != null) {
        if (!((Mat)actualMat).hasCargo(Decorator.getOutermost(this))) {
          ((Mat) actualMat).addCargo(Decorator.getOutermost(this));
        }
      }
    }
  }

  /**
   * Places us "on" the designated Mat and returns a Command to duplicate the changes on other clients
   * @param newMat GamePiece with a Mat trait, that we are to be placed "on"
   * @return Command to duplicate the changes on other clients
   */
  public Command makeSetMatCommand(GamePiece newMat) {
    if (mat == newMat) {
      return new NullCommand();
    }

    if (newMat == null) {
      return makeClearMatCommand();
    }

    final Mat actualMat = (Mat)Decorator.getDecorator(Decorator.getOutermost(newMat), Mat.class);
    if (actualMat == null) {
      return makeClearMatCommand();
    }
    else {
      return actualMat.makeAddCargoCommand(this);
    }
  }

  /**
   * Finds us a Mat to join at the specified point on a specified map. Or if no Mat, marks our removal from any we're on.
   * @param map map to check
   * @param pt point to check
   * @return A command that will duplicate any changes on another client.
   */
  public Command findNewMat(Map map, Point pt) {
    Command comm = new NullCommand();
    if (map != null) {
      GamePiece newMat = map.findAnyPiece(pt, PieceFinder.MAT_ONLY);

      if ((newMat == null) && (detectionDistanceX != 0) || (detectionDistanceY != 0)) {
        final Point pt2 = new Point();

        pt2.x = pt.x + detectionDistanceX;
        pt2.y = pt.y + detectionDistanceY;
        newMat = map.findAnyPiece(pt2, PieceFinder.MAT_ONLY);

        if (newMat == null) {
          pt2.x = pt.x - detectionDistanceX;
          pt2.y = pt.y + detectionDistanceY;
          newMat = map.findAnyPiece(pt2, PieceFinder.MAT_ONLY);

          if (newMat == null) {
            pt2.x = pt.x + detectionDistanceX;
            pt2.y = pt.y - detectionDistanceY;
            newMat = map.findAnyPiece(pt2, PieceFinder.MAT_ONLY);

            if (newMat == null) {
              pt2.x = pt.x - detectionDistanceX;
              pt2.y = pt.y - detectionDistanceY;
              newMat = map.findAnyPiece(pt2, PieceFinder.MAT_ONLY);
            }
          }
        }
      }

      if (newMat != null) {
        final Mat mat = (Mat) Decorator.getDecorator(newMat, Mat.class);
        if (mat != null) {
          comm = comm.append(mat.makeAddCargoCommand(getOutermost(this)));
        }
      }
      else {
        // We're NOT on a mat, so if we WERE on one, mark ourselves as no longer on it
        comm = comm.append(makeClearMatCommand());
      }
    }
    return comm;
  }

  /**
   * Finds us a Mat to join at our current location. Or if no Mat, marks our removal from any we were on.
   * @return A Command that will duplicate any changes on another client.
   */
  public Command findNewMat() {
    return findNewMat(getMap(), getPosition());
  }

  /**
   * @return current Mat we are on top of (or null for none)
   */
  public GamePiece getMat() {
    return mat;
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(mat == null ? NO_MAT : mat.getId());
    return se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void mySetState(String newState) {
    final GameModule gm = GameModule.getGameModule();

    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState, ';');
    final String token = st.nextToken();

    mat = NO_MAT.equals(token) ? null : gm.getGameState().getPieceForId(token);
    gm.setMatSupport(true);
  }

  @Override
  public Rectangle boundingBox() {
    final Rectangle b = piece.boundingBox();
    final double angle = getMatAngle();

    if (angle == 0.0) {
      return b;
    }

    Rectangle r;
    if ((getGpOp() != null && getGpOp().isChanged()) ||
        (r = bounds.get(angle)) == null) {
      r = AffineTransform.getRotateInstance(-PI_180 * angle,
                                            centerX(),
                                            centerY())
                         .createTransformedShape(b).getBounds();
      bounds.put(angle, r);
    }

    return new Rectangle(r);
  }

  protected GamePieceOp getGpOp() {
    if (gpOp == null) {
      if (getInner() != null) {
        gpOp = Op.piece(getInner());
      }
    }
    return gpOp;
  }

  protected double getMatAngle() {
    if (mat == null || !maintainRelativeFacing) {
      return 0.0;
    }

    final FreeRotator mrot = (FreeRotator) Decorator.getDecorator(getOutermost(mat), FreeRotator.class);
    return mrot == null ? 0.0 : mrot.getAngle();
  }

  /**
   * If we're maintaining facing to our Mat, rotate our graphics as appropriate to account for that when drawing
   */
  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    final double angle = getMatAngle();
    if (angle == 0.0) {
      piece.draw(g, x, y, obs, zoom);
      return;
    }

    RotateScaleOp op;
    if (getGpOp() != null && getGpOp().isChanged()) {
      gpOp = Op.piece(piece);
      bounds.clear();
      rotOp.clear();
      op = Op.rotateScale(gpOp, angle, zoom);
      rotOp.put(angle, op);
    }
    else {
      op = rotOp.get(angle);
      if (op == null || op.getScale() != zoom) {
        op = Op.rotateScale(gpOp, angle, zoom);
        rotOp.put(angle, op);
      }
    }

    final Rectangle r = boundingBox();

    final Image img = op.getImage();
    if (img != null) {
      g.drawImage(img, x + (int) (zoom * r.x), y + (int) (zoom * r.y), obs);
    }
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  private double centerX() {
    // The center is not on a vertex for pieces with odd widths.
    return (piece.boundingBox().width % 2) / 2.0;
  }

  private double centerY() {
    // The center is not on a vertex for pieces with odd heights.
    return (piece.boundingBox().height % 2) / 2.0;
  }

  /**
   * If we're maintaining facing to our Mat, rotate our piece's shape to account for that.
   * @return Properly rotated shape
   */
  @Override
  public Shape getShape() {
    final double angle = getMatAngle();
    final Shape s = piece.getShape();

    if (angle == 0.0) {
      return s;
    }

    return AffineTransform.getRotateInstance(
      -PI_180 * angle, centerX(), centerY()
    ).createTransformedShape(s);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public Object getProperty(Object key) {
    if (mat != null) {
      if (CURRENT_MAT.equals(key)) {
        return mat.getProperty(Mat.MAT_NAME);
      }
      else if (CURRENT_MAT_ID.equals(key)) {
        return mat.getProperty(Mat.MAT_ID);
      }
      else if (CURRENT_MAT_X.equals(key)) {
        return Decorator.getOutermost(mat).getPosition().x;
      }
      else if (CURRENT_MAT_Y.equals(key)) {
        return Decorator.getOutermost(mat).getPosition().y;
      }
      else if (CURRENT_MAT_OFFSET_X.equals(key)) {
        return Decorator.getOutermost(mat).getPosition().x - Decorator.getOutermost(this).getPosition().x;
      }
      else if (CURRENT_MAT_OFFSET_Y.equals(key)) {
        return Decorator.getOutermost(mat).getPosition().y - Decorator.getOutermost(this).getPosition().y;
      }
      else if (CURRENT_MAT_BASIC_NAME.equals(key)) {
        return Decorator.getOutermost(mat).getProperty(BASIC_NAME);
      }
      else if (CURRENT_MAT_PIECE_NAME.equals(key)) {
        return Decorator.getOutermost(mat).getProperty(PIECE_NAME);
      }
      else if (CURRENT_MAT_LOCATION_NAME.equals(key)) {
        return Decorator.getOutermost(mat).getProperty(LOCATION_NAME);
      }
      else if (CURRENT_MAT_ZONE.equals(key)) {
        return Decorator.getOutermost(mat).getProperty(CURRENT_ZONE);
      }
      else if (CURRENT_MAT_BOARD.equals(key)) {
        return Decorator.getOutermost(mat).getProperty(CURRENT_BOARD);
      }
      else if (CURRENT_MAT_MAP.equals(key)) {
        return Decorator.getOutermost(mat).getProperty(CURRENT_MAP);
      }
      else if (List.of(CURRENT_MAT_PROP0, CURRENT_MAT_PROP1, CURRENT_MAT_PROP2, CURRENT_MAT_PROP3, CURRENT_MAT_PROP4, CURRENT_MAT_PROP5, CURRENT_MAT_PROP6, CURRENT_MAT_PROP7, CURRENT_MAT_PROP8, CURRENT_MAT_PROP9
      ).contains(key)) {
        return Decorator.getOutermost(mat).getProperty(key);
      }
    }

    if (IS_CARGO.equals(key)) {
      return Boolean.TRUE;
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (mat != null) {
      if (CURRENT_MAT.equals(key)) {
        return mat.getLocalizedProperty(Mat.MAT_NAME);
      }
      else if (CURRENT_MAT_BASIC_NAME.equals(key)) {
        return Decorator.getOutermost(mat).getLocalizedProperty(BASIC_NAME);
      }
      else if (CURRENT_MAT_PIECE_NAME.equals(key)) {
        return Decorator.getOutermost(mat).getLocalizedProperty(PIECE_NAME);
      }
      else if (CURRENT_MAT_LOCATION_NAME.equals(key)) {
        return Decorator.getOutermost(mat).getLocalizedProperty(LOCATION_NAME);
      }
      else if (CURRENT_MAT_ZONE.equals(key)) {
        return Decorator.getOutermost(mat).getLocalizedProperty(CURRENT_ZONE);
      }
      else if (CURRENT_MAT_BOARD.equals(key)) {
        return Decorator.getOutermost(mat).getLocalizedProperty(CURRENT_BOARD);
      }
      else if (CURRENT_MAT_MAP.equals(key)) {
        return Decorator.getOutermost(mat).getLocalizedProperty(CURRENT_MAP);
      }
      else if (List.of(CURRENT_MAT_PROP0, CURRENT_MAT_PROP1, CURRENT_MAT_PROP2, CURRENT_MAT_PROP3, CURRENT_MAT_PROP4, CURRENT_MAT_PROP5, CURRENT_MAT_PROP6, CURRENT_MAT_PROP7, CURRENT_MAT_PROP8, CURRENT_MAT_PROP9
      ).contains(key)) {
        return Decorator.getOutermost(mat).getLocalizedProperty(key);
      }
    }

    if (List.of(
      CURRENT_MAT_ID,
      CURRENT_MAT_X,
      CURRENT_MAT_Y,
      CURRENT_MAT_OFFSET_X,
      CURRENT_MAT_OFFSET_Y,
      IS_CARGO
    ).contains(key)) {
      return getProperty(key);
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.MatCargo.trait_description", desc);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof MatCargo)) return false;
    final MatCargo c = (MatCargo) o;
    return Objects.equals(desc, c.desc) &&
           maintainRelativeFacing == c.maintainRelativeFacing;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("MatCargo.html"); // NON-NLS
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    return Arrays.asList(CURRENT_MAT, CURRENT_MAT_ID, IS_CARGO, CURRENT_MAT_X, CURRENT_MAT_Y, CURRENT_MAT_OFFSET_X, CURRENT_MAT_OFFSET_Y, CURRENT_MAT_BASIC_NAME, CURRENT_MAT_PIECE_NAME, CURRENT_MAT_LOCATION_NAME, CURRENT_MAT_ZONE, CURRENT_MAT_BOARD, CURRENT_MAT_MAP, CURRENT_MAT_PROP0, CURRENT_MAT_PROP1, CURRENT_MAT_PROP2, CURRENT_MAT_PROP3, CURRENT_MAT_PROP4, CURRENT_MAT_PROP5, CURRENT_MAT_PROP6, CURRENT_MAT_PROP7, CURRENT_MAT_PROP8, CURRENT_MAT_PROP9);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer descInput;
    private final BooleanConfigurer rotInput;
    private final TraitConfigPanel controls;
    private final IntConfigurer xInput;
    private final IntConfigurer yInput;

    public Ed(MatCargo p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.desc);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);

      rotInput = new BooleanConfigurer(p.maintainRelativeFacing);
      controls.add("Editor.MatCargo.maintain_relative_facing", rotInput);

      xInput = new IntConfigurer(p.detectionDistanceX);
      controls.add("Editor.MatCargo.detection_distance_x", xInput);

      yInput = new IntConfigurer(p.detectionDistanceY);
      controls.add("Editor.MatCargo.detection_distance_y", yInput);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(descInput.getValueString())
        .append(rotInput.getValueBoolean())
        .append(xInput.getIntValue(0))
        .append(yInput.getIntValue(0));

      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
