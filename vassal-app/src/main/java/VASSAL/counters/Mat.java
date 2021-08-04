package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;

import javax.swing.KeyStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP0;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP1;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP2;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP3;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP4;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP5;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP6;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP7;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP8;
import static VASSAL.counters.MatCargo.CURRENT_MAT_PROP9;
import static VASSAL.counters.Properties.PIECE_ID;

/**
 * Designates the piece as a "Mat" on which other pieces ("Cargo") can be placed.
 */
public class Mat extends Decorator implements TranslatablePiece {
  public static final String ID = "mat;"; // NON-NLS
  public static final String MAT_NAME = "MatName"; //NON-NLS
  public static final String MAT_ID = "MatID"; //NON-NLS
  public static final String MAT_CONTENTS = "MatContents"; //NON-NLS
  public static final String MAT_NUM_CARGO = "MatNumCargo"; //NON-NLS
  protected String matName;
  protected String desc;
  protected List<GamePiece> contents = new ArrayList<>();

  public Mat() {
    this(ID + "Mat;;", null); //NON-NLS
  }

  public Mat(String name) {
    this (ID + name + ";;", null);
  }

  public Mat(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public void mySetType(String type) {
    type = type.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');

    matName = st.nextToken();
    desc = st.nextToken();
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(matName).append(desc);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');

    se.append(contents.size());
    for (final GamePiece p : contents) {
      se.append(p.getId());
    }

    return se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public List<GamePiece> getContents() {
    return new ArrayList<>(contents);
  }

  public List<Point> getOffsets(int x, int y) {
    final List<Point> offsets = new ArrayList<>();
    for (final GamePiece piece : getContents()) {
      final Point pt = piece.getPosition();
      pt.x -= x;
      pt.y -= y;
      offsets.add(pt);
    }
    return offsets;
  }


  @Override
  public void mySetState(String newState) {
    contents.clear();
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(newState, ';');
    final int num = st.nextInt(0);
    final GameState gs = GameModule.getGameModule().getGameState();
    for (int i = 0; i < num; i++) {
      contents.add(gs.getPieceForId(st.nextToken()));
    }

    GameModule.getGameModule().setMatSupport(true);
  }

  /**
   * @param p a particular gamepiece, presumably with a MatCargo trait
   * @return true if the given piece is on our list of contained cargo
   */
  public boolean hasCargo(GamePiece p) {
    return contents.contains(p);
  }

  /**
   * Adds a piece of cargo to this mat
   * @param p game piece to add
   */
  public void addCargo(GamePiece p) {
    if (!(p instanceof Decorator) || hasCargo(p)) {
      return;
    }

    contents.add(p);

    final GamePiece cargo = Decorator.getDecorator(Decorator.getOutermost(p), MatCargo.class);
    if (cargo != null) {
      ((MatCargo)cargo).setMat(Decorator.getOutermost(this));
    }
  }


  /**
   * Adds a piece of cargo and returns a command to duplicate the operation on another client
   * @param p piece of cargo to add
   * @return Command that adds the cargo to this mat (and removes it from any other mat it was on)
   */
  public Command makeAddCargoCommand(GamePiece p) {
    final ChangeTracker ct  = new ChangeTracker(this);
    final ChangeTracker ct2 = new ChangeTracker(p);
    ChangeTracker ct3 = null;

    if ((p instanceof Decorator) && !hasCargo(p)) {
      final GamePiece cargo = Decorator.getDecorator(Decorator.getOutermost(p), MatCargo.class);
      if (cargo != null) {
        final GamePiece mt = ((MatCargo)cargo).getMat();
        if ((mt != null) && (mt != Decorator.getOutermost(this))) {
          ct3 = new ChangeTracker(mt);
        }
      }
    }

    addCargo(p);

    Command c = ct.getChangeCommand().append(ct2.getChangeCommand());
    if (ct3 != null) {
      c = c.append(ct3.getChangeCommand());
    }

    return c;
  }

  /**
   * Removes a MatCargo piece from our list of cargo
   * @param p Cargo to remove
   */
  public void removeCargo(GamePiece p) {
    if ((p instanceof Decorator) && hasCargo(p)) {
      contents.remove(p);
      final GamePiece mp = Decorator.getDecorator(p, MatCargo.class);
      if (mp != null) {
        ((MatCargo)mp).clearMat();
      }
    }
  }

  /**
   * Removes a MatCargo piece from our list of cargo, and returns a Command to duplicate the changes on another client
   * @param p GamePiece with a MatCargo trait, to be removed
   * @return Command to remove the piece
   */
  public Command makeRemoveCargoCommand(GamePiece p) {
    final ChangeTracker ct  = new ChangeTracker(this);
    final ChangeTracker ct2 = new ChangeTracker(p);

    removeCargo(p);

    return ct.getChangeCommand().append(ct2.getChangeCommand());
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
  public String getDescription() {
    return buildDescription("Editor.Mat.trait_description", matName, desc);
  }

  @Override
  public Object getProperty(Object key) {
    if (MAT_NAME.equals(key)) {
      return matName;
    }
    if (MAT_ID.equals(key)) {
      return matName + "_" + getProperty(BasicPiece.PIECE_UID);
    }
    else if (MAT_CONTENTS.equals(key)) {
      return new ArrayList<>(contents);
    }
    else if (MAT_NUM_CARGO.equals(key)) {
      return String.valueOf(contents.size());
    }
    else if (Properties.NO_STACK.equals(key)) { // Mats can't stack
      return Boolean.TRUE;
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (MAT_NAME.equals(key)) {
      return matName;
    }
    if (MAT_ID.equals(key)) {
      return matName + "_" + getProperty(BasicPiece.PIECE_UID);
    }
    else if (MAT_NUM_CARGO.equals(key)) {
      return String.valueOf(contents.size());
    }
    else if (Properties.NO_STACK.equals(key)) { //  Mats can't stack
      return Boolean.TRUE;
    }
    return super.getLocalizedProperty(key);
  }

  @Override
  public void setProperty(Object key, Object value) {
    if (MAT_NAME.equals(key)) {
      matName = (String) value;
      return;
    }
    super.setProperty(key, value);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Mat)) return false;
    final Mat c = (Mat) o;
    if (!Objects.equals(matName, c.matName)) {
      return false;
    }
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
    return Arrays.asList(MAT_NAME, MAT_ID, MAT_NUM_CARGO, CURRENT_MAT_PROP0, CURRENT_MAT_PROP1, CURRENT_MAT_PROP2, CURRENT_MAT_PROP3, CURRENT_MAT_PROP4, CURRENT_MAT_PROP5, CURRENT_MAT_PROP6, CURRENT_MAT_PROP7, CURRENT_MAT_PROP8, CURRENT_MAT_PROP9);
  }

  public static class Ed implements PieceEditor {
    private final StringConfigurer matNameInput;
    private final StringConfigurer descInput;
    private final TraitConfigPanel controls;

    public Ed(Mat p) {
      controls = new TraitConfigPanel();

      matNameInput = new StringConfigurer(p.matName);
      matNameInput.setHintKey("Editor.Mat.name_hint");
      controls.add("Editor.Mat.name_label", matNameInput);

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
      se.append(matNameInput.getValueString());
      se.append(descInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
