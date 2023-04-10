/*
 *
 * Copyright (c) 2005 by Scott Giese, Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.counters;

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MapShader;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.module.map.boardPicker.board.GeometricGrid;
import VASSAL.build.module.map.boardPicker.board.MapGrid;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ChooseComponentDialog;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;
import net.miginfocom.swing.MigLayout;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * @author Scott Giese sgiese@sprintmail.com
 *
 * Displays a transparency surrounding the GamePiece which represents the Area of Effect of the GamePiece
 */
public class AreaOfEffect extends Decorator implements TranslatablePiece, MapShader.ShadedPiece {
  public static final String ID = "AreaOfEffect;"; // NON-NLS
  public static final String ACTIVE = "_Active"; // NON-NLS
  protected static final Color defaultTransparencyColor = Color.GRAY;
  protected static final float defaultTransparencyLevel = 0.3F;
  protected static final int defaultRadius = 1;

  protected Color transparencyColor;
  protected float transparencyLevel;
  protected int radius;
  protected boolean alwaysActive;
  /** Track the Globally active visibility of the trait. Reported in State to other players. */
  protected boolean active;
  /** Track the Locally active visibility of the trait. Not reported in state to other players. */
  protected boolean locallyActive;
  protected String activateCommand = "";
  protected NamedKeyStroke activateKey;
  protected KeyCommand[] commands;
  protected String mapShaderName;
  protected MapShader shader;
  protected KeyCommand keyCommand;
  protected boolean fixedRadius = true;
  protected String radiusMarker = "";
  protected String description = "";

  protected String onMenuText = "";
  protected NamedKeyStroke onKey;
  protected KeyCommand onKeyCommand;
  protected String offMenuText = "";
  protected NamedKeyStroke offKey;
  protected KeyCommand offKeyCommand;
  protected String name = "";
  protected boolean globallyVisible = true;

  public AreaOfEffect() {
    this(ID + ColorConfigurer.colorToString(defaultTransparencyColor), null);
  }

  public AreaOfEffect(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.AreaOfEffect.trait_description", description);
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.AreaOfEffect.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return description;
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(transparencyColor);
    se.append((int) (transparencyLevel * 100));
    se.append(radius);
    se.append(alwaysActive);
    se.append(activateCommand);
    se.append(activateKey);
    se.append(mapShaderName == null ? "" : mapShaderName);
    se.append(fixedRadius);
    se.append(radiusMarker);
    se.append(description);
    se.append(name);
    se.append(onMenuText);
    se.append(onKey);
    se.append(offMenuText);
    se.append(offKey);
    se.append(globallyVisible);

    return ID + se.getValue();
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();    // Discard ID
    transparencyColor = st.nextColor(defaultTransparencyColor);
    transparencyLevel = st.nextInt((int) (defaultTransparencyLevel * 100)) / 100.0F;
    radius = st.nextInt(defaultRadius);
    alwaysActive = st.nextBoolean(true);
    active = alwaysActive;  // Set initial value of Active flag
    locallyActive = alwaysActive;
    activateCommand = st.nextToken(Resources.getString("Editor.AreaOfEffect.show_area"));
    activateKey = st.nextNamedKeyStroke(null);
    keyCommand = new KeyCommand(activateCommand, activateKey, Decorator.getOutermost(this), this);
    mapShaderName = st.nextToken("");
    if (mapShaderName.length() == 0) {
      mapShaderName = null;
    }
    fixedRadius = st.nextBoolean(true);
    radiusMarker = st.nextToken("");
    description = st.nextToken("");
    name = st.nextToken("");
    onMenuText = st.nextToken("");
    onKey = st.nextNamedKeyStroke(null);
    onKeyCommand = new KeyCommand(onMenuText, onKey, Decorator.getOutermost(this), this);
    offMenuText = st.nextToken("");
    offKey = st.nextNamedKeyStroke(null);
    offKeyCommand = new KeyCommand(offMenuText, offKey, Decorator.getOutermost(this), this);
    globallyVisible = st.nextBoolean(true);
    shader = null;
    commands = null;
  }

  // State is locked to the Globally Visible state. Global visibility (and thus state) will never change
  // if visibility is local.
  @Override
  public String myGetState() {
    return (alwaysActive || !globallyVisible) ? "false" : String.valueOf(active); // NON-NLS
  }

  @Override
  public void mySetState(String newState) {
    if (!alwaysActive) {
      active = "true".equals(newState); // NON-NLS
    }
  }

  @Override
  public Rectangle boundingBox() {
    final Rectangle r = piece.boundingBox();

    // include the AoE in the bounding box if it's visible
    if ((alwaysActive || active) && mapShaderName == null) {
      final Area a = getArea();
      if (a != null) {
        final Rectangle aoeBounds = a.getBounds();
        final Point mapPosition = getPosition();
        aoeBounds.translate(-mapPosition.x, -mapPosition.y);
        r.add(aoeBounds);
      }
    }

    return r;
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  public boolean isLocallyActive() {
    return locallyActive;
  }

  public boolean isGloballyActive() {
    return active;
  }

  public boolean isActive() {
    return isLocallyActive() || isGloballyActive();
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(activateKey, onKey, offKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(activateCommand, onMenuText, offMenuText);
  }


  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if ((alwaysActive || isActive()) && mapShaderName == null) {
      // The transparency is only drawn on a Map.View component. Only the
      // GamePiece is drawn within other windows (Counter Palette, etc.).
      if (obs instanceof Map.View && getMap() != null) {
        Area a = getArea();
        if (a != null) {
          final Graphics2D g2d = (Graphics2D) g;

          final Color oldColor = g2d.getColor();
          g2d.setColor(transparencyColor);

          final Composite oldComposite = g2d.getComposite();
          g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, transparencyLevel));

          if (zoom != 1.0) {
            a = new Area(AffineTransform.getScaleInstance(zoom, zoom).createTransformedShape(a));
          }
          g2d.fill(a);


          g2d.setColor(oldColor);
          g2d.setComposite(oldComposite);
        }
      }
    }

    // Draw the GamePiece
    piece.draw(g, x, y, obs, zoom);
  }

  protected Area getArea() {
    final Map map = getMap();
    if (map == null) {
      return null;
    }
    // Always draw the area centered on the piece's current position
    // (For instance, don't draw it at an offset if it's in an expanded stack)
    final Point mapPosition = getPosition();
    final int myRadius = getRadius();

    final Board board = map.findBoard(mapPosition);
    final MapGrid grid = board == null ? null : board.getGrid();

    Area a;
    if (grid instanceof GeometricGrid) {
      final GeometricGrid gGrid = (GeometricGrid) grid;

      final Rectangle boardBounds = board.bounds();
      final Point boardPosition = new Point(
        mapPosition.x - boardBounds.x, mapPosition.y - boardBounds.y);

      a = gGrid.getGridShape(boardPosition, myRadius); // In board co-ords
      final AffineTransform t = AffineTransform.getTranslateInstance(
        boardBounds.x, boardBounds.y); // Translate back to map co-ords

      final double mag = board.getMagnification();
      if (mag != 1.0) {
        t.translate(boardPosition.x, boardPosition.y);
        t.scale(mag, mag);
        t.translate(-boardPosition.x, -boardPosition.y);
      }
      a = a.createTransformedArea(t);
    }
    else {
      a = new Area(
        new Ellipse2D.Double(mapPosition.x - myRadius,
                             mapPosition.y - myRadius,
                             myRadius * 2, myRadius * 2));
    }
    return a;
  }

  protected int getRadius() {
    if (fixedRadius) {
      return radius;
    }
    else {
      final String r =
        (String) Decorator.getOutermost(this).getProperty(radiusMarker);
      try {
        return Integer.parseInt(r);
      }
      catch (NumberFormatException e) {
        reportDataError(this, Resources.getString("Error.non_number_error"),
          "radius[" + radiusMarker + "]=" + r, e); // NON-NLS
        return 0;
      }
    }
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      if (!alwaysActive) {
        final List<KeyCommand> l = new ArrayList<>();
        if (!activateCommand.isBlank()) {
          l.add(keyCommand);
        }
        if (!onMenuText.isBlank()) {
          l.add(onKeyCommand);
        }
        if (!offMenuText.isEmpty()) {
          l.add(offKeyCommand);
        }
        commands = l.toArray(new KeyCommand[0]);
      }
    }
    return commands;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (!alwaysActive && keyCommand.matches(stroke)) {
      if (globallyVisible) {
        final ChangeTracker t = new ChangeTracker(this);
        active = !active;
        c = t.getChangeCommand();
      }
      locallyActive = !locallyActive;
    }
    else if (!alwaysActive && !isActive() && onKeyCommand.matches(stroke)) {
      if (globallyVisible) {
        final ChangeTracker t = new ChangeTracker(this);
        active = true;
        c = t.getChangeCommand();
      }
      locallyActive = true;
    }
    else if (!alwaysActive && isActive() && offKeyCommand.matches(stroke)) {
      if (globallyVisible) {
        final ChangeTracker t = new ChangeTracker(this);
        active = false;
        c = t.getChangeCommand();
      }
      locallyActive = false;
    }

    return c;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("AreaOfEffect.html"); // NON-NLS
  }

  @Override
  public PieceEditor getEditor() {
    return new TraitEditor(this);
  }

  @Override
  public Area getArea(MapShader shader) {
    Area a = null;
    final MapShader.ShadedPiece shaded = (MapShader.ShadedPiece) Decorator.getDecorator(piece, MapShader.ShadedPiece.class);
    if (shaded != null) {
      a = shaded.getArea(shader);
    }
    if (alwaysActive || isActive()) {
      if (shader.getConfigureName().equals(mapShaderName)) {
        final Area myArea = getArea();
        if (a == null) {
          a = myArea;
        }
        else {
          a.add(myArea);
        }
      }
    }
    return a;
  }

  /**
   * Return the active state of the trait
   * @param key Property name
   * @return state
   */
  @Override
  public Object getProperty(Object key) {
    if ((name + ACTIVE).equals(key)) {
      return String.valueOf(isActive());
    }
    else if (key.equals(Properties.VISIBLE_STATE)) {
      return String.valueOf(super.getProperty(key)) + String.valueOf(isActive());
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (key.equals(name + ACTIVE) || key.equals(Properties.VISIBLE_STATE)) {
      return getProperty(key);
    }
    return super.getLocalizedProperty(key);
  }
  @Override
  public List<String> getPropertyNames() {
    return List.of(name + ACTIVE);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof AreaOfEffect)) return false;
    final AreaOfEffect c = (AreaOfEffect) o;
    if (! Objects.equals(transparencyColor, c.transparencyColor)) return false;
    if (! Objects.equals(transparencyLevel, c.transparencyLevel)) return false;
    if (! Objects.equals(radius, c.radius)) return false;
    if (! Objects.equals(alwaysActive, c.alwaysActive)) return false;
    if (! Objects.equals(activateCommand, c.activateCommand)) return false;
    if (! Objects.equals(activateKey, c.activateKey)) return false;
    if (! Objects.equals(mapShaderName, c.mapShaderName)) return false;
    if (! Objects.equals(fixedRadius, c.fixedRadius)) return false;

    if (alwaysActive) {
      if (! Objects.equals(active, c.active)) return false;
      if (! Objects.equals(locallyActive, c.locallyActive)) return false;
    }

    if (! Objects.equals(name, c.name)) return false;
    if (! Objects.equals(onMenuText, c.onMenuText)) return false;
    if (! Objects.equals(onKey, c.onKey)) return false;
    if (! Objects.equals(offMenuText, c.offMenuText)) return false;
    if (! Objects.equals(offKey, c.offKey)) return false;

    return Objects.equals(description, c.description);

  }

  protected static class TraitEditor implements PieceEditor {
    protected TraitConfigPanel panel;
    protected final JLabel transparencyColorLabel;
    protected ColorConfigurer transparencyColorValue;
    protected final JLabel transparencyLabel;
    protected IntConfigurer transparencyValue;
    protected IntConfigurer radiusValue;
    protected JLabel radiusValueLabel;
    protected BooleanConfigurer alwaysActive;
    protected StringConfigurer activateCommand;
    protected JLabel activateCommandLabel;
    protected NamedHotKeyConfigurer activateKey;
    protected JLabel activateKeyLabel;
    protected BooleanConfigurer useMapShader;
    protected BooleanConfigurer fixedRadius;
    protected StringConfigurer radiusMarker;
    protected JLabel radiusMarkerLabel;
    protected StringConfigurer descConfig;
    protected JLabel selectShaderLabel;
    protected JPanel selectShader;
    protected String mapShaderId;

    protected StringConfigurer nameConfig;
    protected JLabel nameLabel;
    protected StringConfigurer onMenuTextConfig;
    protected JLabel onMenuTextLabel;
    protected NamedHotKeyConfigurer onKeyConfig;
    protected JLabel onKeyLabel;
    protected StringConfigurer offMenuTextConfig;
    protected JLabel offMenuTextLabel;
    protected NamedHotKeyConfigurer offKeyConfig;
    protected JLabel offKeyLabel;
    protected BooleanConfigurer globallyVisibleConfig;

    protected TraitEditor(AreaOfEffect trait) {
      panel = new TraitConfigPanel();

      descConfig = new StringConfigurer(trait.description);
      descConfig.setHintKey("Editor.description_hint");
      panel.add("Editor.description_label", descConfig);

      nameConfig = new StringConfigurer(trait.name);
      nameConfig.setHintKey("Editor.AreaOfEffect.name_hint");
      panel.add("Editor.name_label", nameConfig);

      useMapShader = new BooleanConfigurer(trait.mapShaderName != null);
      panel.add("Editor.AreaOfEffect.use_map_shading", useMapShader);

      mapShaderId = trait.mapShaderName;
      selectShader = new JPanel(new MigLayout("ins 0", "[fill, grow]0[]")); // NON-NLS

      final JTextField tf = new JTextField();
      tf.setEditable(false);
      selectShader.add(tf, "growx"); // NON-NLS
      tf.setText(trait.mapShaderName);
      final JButton b = new JButton(Resources.getString("Editor.AreaOfEffect.select"));
      selectShader.add(b);
      b.addActionListener(e -> {
        final ChooseComponentDialog d = new ChooseComponentDialog((Frame) SwingUtilities.getAncestorOfClass(Frame.class, panel), MapShader.class);
        d.setVisible(true);
        if (d.getTarget() != null) {
          mapShaderId = d.getTarget().getConfigureName();
          tf.setText(mapShaderId);
        }
        else {
          mapShaderId = null;
          tf.setText("");
        }
      });
      selectShaderLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.map_shading"));
      panel.add(selectShaderLabel, selectShader);

      transparencyColorLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.fill_color"));
      transparencyColorValue = new ColorConfigurer(trait.transparencyColor);
      panel.add(transparencyColorLabel, transparencyColorValue);

      transparencyLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.opacity"));
      transparencyValue = new IntConfigurer((int) (trait.transparencyLevel * 100));
      transparencyValue.setHint("0-100");
      panel.add(transparencyLabel, transparencyValue);

      fixedRadius = new BooleanConfigurer(Boolean.valueOf(trait.fixedRadius));
      fixedRadius.addPropertyChangeListener(evt -> updateRangeVisibility());
      panel.add("Editor.AreaOfEffect.fixed_radius", fixedRadius);

      radiusValueLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.radius"));
      radiusValue = new IntConfigurer(trait.radius);
      panel.add(radiusValueLabel, radiusValue);

      radiusMarkerLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.radius_marker"));
      radiusMarker = new StringConfigurer(trait.radiusMarker);
      panel.add(radiusMarkerLabel, radiusMarker);

      globallyVisibleConfig = new BooleanConfigurer(trait.globallyVisible);
      panel.add("Editor.AreaOfEffect.visible_to_all_players", globallyVisibleConfig);

      alwaysActive = new BooleanConfigurer(trait.alwaysActive ? Boolean.TRUE : Boolean.FALSE);
      activateCommand = new StringConfigurer(trait.activateCommand);
      activateCommandLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.toggle_visible_command"));
      activateKey = new NamedHotKeyConfigurer(trait.activateKey);
      activateKeyLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.toggle_visible_keyboard_shortcut"));

      onMenuTextConfig = new StringConfigurer(trait.onMenuText);
      onMenuTextLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.on_command"));

      onKeyConfig = new NamedHotKeyConfigurer(trait.onKey);
      onKeyLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.on_key"));

      offMenuTextConfig = new StringConfigurer(trait.offMenuText);
      offMenuTextLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.off_command"));

      offKeyConfig = new NamedHotKeyConfigurer(trait.offKey);
      offKeyLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.off_key"));

      updateRangeVisibility();

      alwaysActive.addPropertyChangeListener(evt -> updateCommandVisibility());
      updateCommandVisibility();

      useMapShader.addPropertyChangeListener(evt -> updateFillVisibility());
      updateFillVisibility();

      panel.add("Editor.AreaOfEffect.always_visible", alwaysActive);
      panel.add(activateCommandLabel, activateCommand);
      panel.add(activateKeyLabel, activateKey);
      panel.add(onMenuTextLabel, onMenuTextConfig);
      panel.add(onKeyLabel, onKeyConfig);
      panel.add(offMenuTextLabel, offMenuTextConfig);
      panel.add(offKeyLabel, offKeyConfig);


    }

    protected void updateFillVisibility() {
      final boolean useShader = Boolean.TRUE.equals(useMapShader.getValue());
      transparencyColorLabel.setVisible(!useShader);
      transparencyColorValue.getControls().setVisible(!useShader);
      transparencyLabel.setVisible(!useShader);
      transparencyValue.getControls().setVisible(!useShader);
      selectShader.setVisible(useShader);
      selectShaderLabel.setVisible(useShader);
      repack();
    }

    protected void updateRangeVisibility() {
      final boolean fixedRange = fixedRadius.booleanValue();
      radiusValue.getControls().setVisible(fixedRange);
      radiusMarker.getControls().setVisible(!fixedRange);
      radiusValueLabel.setVisible(fixedRange);
      radiusMarkerLabel.setVisible(!fixedRange);
      repack();
    }

    protected void updateCommandVisibility() {
      final boolean alwaysActiveSelected = Boolean.TRUE.equals(alwaysActive.getValue());
      activateCommand.getControls().setVisible(!alwaysActiveSelected);
      activateKey.getControls().setVisible(!alwaysActiveSelected);
      activateCommandLabel.setVisible(!alwaysActiveSelected);
      activateKeyLabel.setVisible(!alwaysActiveSelected);

      onMenuTextConfig.getControls().setVisible(!alwaysActiveSelected);
      onMenuTextLabel.setVisible(!alwaysActiveSelected);
      onKeyConfig.getControls().setVisible(!alwaysActiveSelected);
      onKeyLabel.setVisible(!alwaysActiveSelected);

      offMenuTextConfig.getControls().setVisible(!alwaysActiveSelected);
      offMenuTextLabel.setVisible(!alwaysActiveSelected);
      offKeyConfig.getControls().setVisible(!alwaysActiveSelected);
      offKeyLabel.setVisible(!alwaysActiveSelected);

      repack();
    }

    protected void repack() {
      Decorator.repack(panel);
    }

    @Override
    public Component getControls() {
      return panel;
    }

    @Override
    public String getState() {
      return "false"; // NON-NLS
    }

    @Override
    public String getType() {
      final boolean alwaysActiveSelected = Boolean.TRUE.equals(alwaysActive.getValue());
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(transparencyColorValue.getValueString());
      se.append(transparencyValue.getValueString());
      se.append(radiusValue.getValueString());
      se.append(alwaysActiveSelected);
      se.append(activateCommand.getValueString());
      se.append(activateKey.getValueString());
      if (Boolean.TRUE.equals(useMapShader.getValue()) && mapShaderId != null) {
        se.append(mapShaderId);
      }
      else {
        se.append("");
      }
      se.append(fixedRadius.getValueString());
      se.append(radiusMarker.getValueString());
      se.append(descConfig.getValueString());

      se.append(nameConfig.getValueString());
      se.append(onMenuTextConfig.getValueString());
      se.append(onKeyConfig.getValueString());
      se.append(offMenuTextConfig.getValueString());
      se.append(offKeyConfig.getValueString());
      se.append(globallyVisibleConfig.getValueString());

      return AreaOfEffect.ID + se.getValue();
    }
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {activateCommand, onMenuText, offMenuText},
      new String[] {
        getCommandDescription(description, Resources.getString("Editor.AreaOfEffect.toggle_visible_command_name")),
        getCommandDescription(description, Resources.getString("Editor.AreaOfEffect.on_command")),
        getCommandDescription(description, Resources.getString("Editor.AreaOfEffect.off_command"))
      }
    );
  }
}
