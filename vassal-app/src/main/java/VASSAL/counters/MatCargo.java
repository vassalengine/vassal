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
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.imageop.GamePieceOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.RotateScaleOp;

/**
 * Designates the piece as a "Mat" on which other pieces can be placed.
 */
public class MatCargo extends Decorator implements TranslatablePiece {
  public static final String ID = "matPiece;"; // NON-NLS
  public static final String NO_MAT = "noMat"; //NON-NLS

  public static final String CURRENT_MAT = "CurrentMat"; //NON-NLS     // Exposed property giving our current mat or "null"
  public static final String IS_CARGO    = "IsCargo"; //NON-NLS        // Exposed property returns "true"

  protected String desc;

  protected boolean rotateWithMat;
  protected GamePieceOp gpOp;
  protected java.util.Map<Double, Rectangle> bounds = new HashMap<>();
  protected java.util.Map<Double, RotateScaleOp> rotOp = new HashMap<>();

  protected static final double PI_180 = Math.PI / 180.0;

  protected GamePiece mat; // Mat piece we are assigned to, or null

  public MatCargo() {
    this(ID + ";", null);
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
    rotateWithMat = st.nextBoolean(false);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(desc)
      .append(rotateWithMat);
    return ID + se.getValue();
  }

  /**
   * Clear our relationship with any Mat we're assigned to
   */
  public void clearMat() {
    if (mat != null) {
      final GamePiece actualMat = Decorator.getDecorator(mat, Mat.class);
      mat = null;
      if (actualMat != null) {
        ((Mat)actualMat).removeCargo(Decorator.getOutermost(this));
      }
    }
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
      final GamePiece newMat = map.findAnyPiece(pt, PieceFinder.MAT_ONLY);
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
    if (mat == null) {
      return 0.0;
    }

    final FreeRotator mrot = (FreeRotator) Decorator.getDecorator(mat, FreeRotator.class);
    return mrot == null ? 0.0 : mrot.getAngle();
  }

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
    if (CURRENT_MAT.equals(key)) {
      if (mat != null) {
        return mat.getProperty(Mat.MAT_NAME);
      }
    }
    else if (IS_CARGO.equals(key)) {
      return "true"; //NON-NLS
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (CURRENT_MAT.equals(key)) {
      if (mat != null) {
        return mat.getLocalizedProperty(Mat.MAT_NAME);
      }
    }
    else if (IS_CARGO.equals(key)) {
      return "true"; //NON-NLS
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
    return Objects.equals(desc, c.desc) && rotateWithMat == c.rotateWithMat;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Mat.html"); // NON-NLS
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    return Arrays.asList(CURRENT_MAT);
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Arrays.asList(CURRENT_MAT);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer descInput;
    private final BooleanConfigurer rotInput;
    private final TraitConfigPanel controls;

    public Ed(MatCargo p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.desc);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);

      rotInput = new BooleanConfigurer(p.rotateWithMat);
      controls.add("Editor.MatCargo.rotate_with_mat", rotInput);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(descInput.getValueString())
        .append(rotInput.getValueBoolean());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
