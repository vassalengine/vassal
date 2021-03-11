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
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

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

/**
 * @author Scott Giese sgiese@sprintmail.com
 *
 * Displays a transparency surrounding the GamePiece which represents the Area of Effect of the GamePiece
 */
public class AreaOfEffect extends Decorator implements TranslatablePiece, MapShader.ShadedPiece {
  public static final String ID = "AreaOfEffect;"; // NON-NLS
  protected static final Color defaultTransparencyColor = Color.GRAY;
  protected static final float defaultTransparencyLevel = 0.3F;
  protected static final int defaultRadius = 1;

  protected Color transparencyColor;
  protected float transparencyLevel;
  protected int radius;
  protected boolean alwaysActive;
  protected boolean active;
  protected String activateCommand = "";
  protected NamedKeyStroke activateKey;
  protected KeyCommand[] commands;
  protected String mapShaderName;
  protected MapShader shader;
  protected KeyCommand keyCommand;
  protected boolean fixedRadius = true;
  protected String radiusMarker = "";
  protected String description = "";

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
    shader = null;
    commands = null;
  }

  // State does not change during the game
  @Override
  public String myGetState() {
    return alwaysActive ? "false" : String.valueOf(active); // NON-NLS
  }

  // State does not change during the game
  @Override
  public void mySetState(String newState) {
    if (!alwaysActive) {
      active = "true".equals(newState); // NON-NLS
    }
  }

  @Override
  public Rectangle boundingBox() {
    // TODO: Need the context of the parent Component, because the transparency is only drawn
    // on a Map.View object.  Because this context is not known, the bounding box returned by
    // this method does not encompass the bounds of the transparency.  The result of this is
    // that portions of the transparency will not be drawn after scrolling the Map window.
    return piece.boundingBox();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getName() {
    return piece.getName();
  }


  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(activateKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(activateCommand);
  }


  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if ((alwaysActive || active) && mapShaderName == null) {
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

  // No hot-keys
  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      if (alwaysActive || activateCommand.length() == 0) {
        commands = KeyCommand.NONE;
      }
      else {
        commands = new KeyCommand[]{keyCommand};
      }
    }
    return commands;
  }

  // No hot-keys
  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    myGetKeyCommands();
    if (!alwaysActive
        && keyCommand.matches(stroke)) {
      final ChangeTracker t = new ChangeTracker(this);
      active = !active;
      c = t.getChangeCommand();
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
    if (alwaysActive || active) {
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
    if (! Objects.equals(radiusMarker, c.radiusMarker)) return false;

    if (alwaysActive) {
      if (!Objects.equals(active, c.active)) return false;
    }

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

    protected TraitEditor(AreaOfEffect trait) {
      panel = new TraitConfigPanel();

      descConfig = new StringConfigurer(trait.description);
      descConfig.setHintKey("Editor.description_hint");
      panel.add("Editor.description_label", descConfig);

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

      alwaysActive = new BooleanConfigurer(trait.alwaysActive ? Boolean.TRUE : Boolean.FALSE);
      activateCommand = new StringConfigurer(trait.activateCommand);
      activateCommandLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.toggle_visible_command"));
      activateKey = new NamedHotKeyConfigurer(trait.activateKey);
      activateKeyLabel = new JLabel(Resources.getString("Editor.AreaOfEffect.toggle_visible_keyboard_shortcut"));

      updateRangeVisibility();

      alwaysActive.addPropertyChangeListener(evt -> updateCommandVisibility());
      updateCommandVisibility();

      useMapShader.addPropertyChangeListener(evt -> updateFillVisibility());
      updateFillVisibility();

      panel.add("Editor.AreaOfEffect.always_visible", alwaysActive);
      panel.add(activateCommandLabel, activateCommand);
      panel.add(activateKeyLabel, activateKey);
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

      return AreaOfEffect.ID + se.getValue();
    }
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(activateCommand, getCommandDescription(description, Resources.getString("Editor.AreaOfEffect.toggle_visible_command_name")));
  }
}
