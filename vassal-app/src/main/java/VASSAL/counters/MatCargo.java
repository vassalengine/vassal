package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;

import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Designates the piece as a "Mat" on which other pieces can be placed.
 */
public class MatCargo extends Decorator implements TranslatablePiece {
  public static final String ID = "matPiece;"; // NON-NLS
  public static final String NO_MAT = "noMat"; //NON-NLS

  public static final String CURRENT_MAT = "CurrentMat"; //NON-NLS     // Exposed property giving our current mat or "null"
  public static final String IS_CARGO    = "IsCargo"; //NON-NLS        // Exposed property returns "true"
  protected String desc;
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
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(desc);
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

    if (mat == null) {
      se.append(NO_MAT);
    }
    else {
      se.append(mat.getId());
    }

    return se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void mySetState(String newState) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState, ';');
    final String token = st.nextToken();

    if (NO_MAT.equals(token)) {
      mat = null;
    }
    else {
      mat = GameModule.getGameModule().getGameState().getPieceForId(token);
    }

    GameModule.getGameModule().setMatSupport(true);
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
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
    return Objects.equals(desc, c.desc);
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
    private final TraitConfigPanel controls;

    public Ed(MatCargo p) {
      controls = new TraitConfigPanel();

      descInput = new StringConfigurer(p.desc);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);
    }


    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(descInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
