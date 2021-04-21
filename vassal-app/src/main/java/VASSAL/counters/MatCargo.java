package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;

import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
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
   * Mark us as being on a specific Mat piece
   * @param mat the mat we are joining
   */
  public void setMat(GamePiece mat) {
    this.mat = mat;
    if (mat != null) {
      final GamePiece actualMat = Decorator.getDecorator(mat, Mat.class);
      if (actualMat != null) {
        if (!((Mat)actualMat).hasCargo(Decorator.getOutermost(this))) {
          ((Mat) actualMat).addCargo(Decorator.getOutermost(this));
        }
      }
    }
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
