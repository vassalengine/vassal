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
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * Designates the piece as a "Mat" on which other pieces can be placed.
 */
public class Mat extends Decorator implements TranslatablePiece {
  public static final String ID = "mat;"; // NON-NLS
  public static final String MAT_NAME = "MatName"; //NON-NLS
  protected String matName;
  protected String desc;
  protected KeyCommand separatorCommand;
  protected List<GamePiece> contents = new ArrayList<>();

  public Mat() {
    this(ID + ";Mat;", null); //NON-NLS
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
    return contents;
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

  public boolean hasMatPiece(GamePiece p) {
    return contents.contains(p);
  }

  public void addMatPiece(GamePiece p) {
    if (!(p instanceof Decorator) || hasMatPiece(p)) {
      return;
    }

    contents.add(p);

    final GamePiece mp = Decorator.getDecorator(Decorator.getOutermost(p), MatPiece.class);
    if (mp != null) {
      ((MatPiece)mp).setMat(Decorator.getOutermost(this));
    }
  }


  public Command addMatPieceCommand(GamePiece p) {
    final ChangeTracker ct  = new ChangeTracker(this);
    final ChangeTracker ct2 = new ChangeTracker(p);
    ChangeTracker ct3 = null;

    if ((p instanceof Decorator) && !hasMatPiece(p)) {
      final GamePiece mp = Decorator.getDecorator(Decorator.getOutermost(p), MatPiece.class);
      if (mp != null) {
        final GamePiece mt = ((MatPiece)mp).getMat();
        if ((mt != null) && (mt != Decorator.getOutermost(this))) {
          ct3 = new ChangeTracker(mt);
        }
      }
    }

    addMatPiece(p);

    Command c = ct.getChangeCommand().append(ct2.getChangeCommand());
    if (ct3 != null) {
      c = c.append(ct3.getChangeCommand());
    }

    return c;
  }

  public void removeMatPiece(GamePiece p) {
    if (!(p instanceof Decorator) || !hasMatPiece(p)) {
      contents.remove(p);
      final GamePiece mp = Decorator.getDecorator(p, MatPiece.class);
      if (mp != null) {
        ((MatPiece)mp).clearMat();
      }
    }
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
    return buildDescription("Editor.Mat.trait_description", desc);
  }

  @Override
  public Object getProperty(Object key) {
    if (MAT_NAME.equals(key)) {
      return matName;
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (MAT_NAME.equals(key)) {
      return matName;
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
    return Arrays.asList(MAT_NAME);
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Arrays.asList(MAT_NAME);
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
