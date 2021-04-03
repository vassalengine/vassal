/*
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.build.module.map;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.configure.TranslatableStringEnum;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.ProblemDialog;
import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Stroke;
import java.awt.TexturePaint;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang3.builder.HashCodeBuilder;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.command.Command;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.AbstractTileOpImpl;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;

/**
 * Draw shaded regions on a map.
 *
 * @author Brent Easton
 */
public class MapShader extends AbstractToolbarItem implements GameComponent, Drawable, UniqueIdManager.Identifyable {

  public static final String NAME = "name"; //NON-NLS
  public static final String HOT_KEY = "hotkey"; //NON-NLS
  public static final String BUTTON_TEXT = "buttonText"; //NON-NLS
  public static final String ICON = "icon"; //NON-NLS
  public static final String TOOLTIP = "tooltip"; //NON-NLS

  public static final String ALWAYS_ON = "alwaysOn"; //NON-NLS
  public static final String STARTS_ON = "startsOn"; //NON-NLS
  public static final String BOARDS = "boards"; //NON-NLS
  public static final String BOARD_LIST = "boardList"; //NON-NLS

  public static final String ALL_BOARDS = "Yes"; //NON-NLS (really)
  public static final String EXC_BOARDS = "No, exclude Boards in list"; //NON-NLS (really)
  public static final String INC_BOARDS = "No, only shade Boards in List"; //NON-NLS (really)

  protected static final UniqueIdManager idMgr = new UniqueIdManager("MapShader"); //NON-NLS

  /** @deprecated use launch from the superclass */
  @Deprecated(since = "2021-04-03", forRemoval = true)
  protected LaunchButton launch;

  protected boolean alwaysOn = false;
  protected boolean startsOn = false;
  protected String boardSelection = ALL_BOARDS;
  protected String[] boardList = new String[0];
  protected boolean shadingVisible;
  protected boolean scaleImage;
  protected Map map;
  protected String id;

  protected Area boardClip = null;

  public static final String TYPE = "type"; //NON-NLS
  public static final String DRAW_OVER = "drawOver"; //NON-NLS
  public static final String PATTERN = "pattern"; //NON-NLS
  public static final String COLOR = "color"; //NON-NLS
  public static final String IMAGE = "image"; //NON-NLS
  public static final String SCALE_IMAGE = "scaleImage"; //NON-NLS
  public static final String OPACITY = "opacity"; //NON-NLS
  public static final String BORDER = "border"; //NON-NLS
  public static final String BORDER_COLOR = "borderColor"; //NON-NLS
  public static final String BORDER_WIDTH = "borderWidth"; //NON-NLS
  public static final String BORDER_OPACITY = "borderOpacity"; //NON-NLS

  public static final String BG_TYPE = "Background"; //NON-NLS (really)
  public static final String FG_TYPE = "Foreground"; //NON-NLS (really)

  public static final String TYPE_25_PERCENT = "25%"; //NON-NLS (really)
  public static final String TYPE_50_PERCENT = "50%"; //NON-NLS (really)
  public static final String TYPE_75_PERCENT = "75%"; //NON-NLS (really)
  public static final String TYPE_SOLID = "100% (Solid)"; //NON-NLS (really)
  public static final String TYPE_IMAGE = "Custom Image"; //NON-NLS (really)

  protected String imageName;
  protected Color color = Color.BLACK;
  protected String type = FG_TYPE;
  protected boolean drawOver = false;
  protected String pattern = TYPE_25_PERCENT;
  protected int opacity = 100;
  protected boolean border = false;
  protected Color borderColor = Color.BLACK;
  protected int borderWidth = 1;
  protected int borderOpacity = 100;

  protected Area shape;
  protected Rectangle patternRect = new Rectangle();

  protected ImageOp srcOp;

  protected TexturePaint texture = null;
  protected java.util.Map<Double, TexturePaint> textures = new HashMap<>();
  protected AlphaComposite composite = null;
  protected AlphaComposite borderComposite = null;
  protected BasicStroke stroke = null;

  public MapShader() {
    setButtonTextKey(BUTTON_TEXT);

    setLaunchButton(makeLaunchButton(
      "",
      Resources.getString("Editor.MapShader.shade"),
      "",
      e -> toggleShading()
    ));
    launch = getLaunchButton(); // for compatibility

    getLaunchButton().setEnabled(false);
    setLaunchButtonVisibility();
    setConfigureName(Resources.getString("Editor.MapShader.configure_name"));
    reset();
  }

  @Override
  public void draw(Graphics g, Map map) {
    if (!shadingVisible) {
      return;
    }

    Area area = getShadeShape(map);
    if (area.isEmpty()) {
      return;
    }

    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final double zoom = map.getZoom() * os_scale;

    if (zoom != 1.0) {
      area = new Area(AffineTransform.getScaleInstance(zoom, zoom)
                                     .createTransformedShape(area));
    }

    final Composite oldComposite = g2d.getComposite();
    final Color oldColor = g2d.getColor();
    final Paint oldPaint = g2d.getPaint();

    g2d.setComposite(getComposite());
    g2d.setColor(getColor());
    g2d.setPaint(getTexture(zoom));

    g2d.fill(area);

    if (border) {
      final Stroke oldStroke = g2d.getStroke();

      g2d.setComposite(getBorderComposite());
      g2d.setStroke(getStroke(zoom));
      g2d.setColor(getBorderColor());
      g2d.draw(area);

      g2d.setStroke(oldStroke);
    }

    g2d.setComposite(oldComposite);
    g2d.setColor(oldColor);
    g2d.setPaint(oldPaint);
  }

  /**
   * Get/Build the AlphaComposite used to draw the semi-transparent shade/
   */
  protected AlphaComposite getComposite() {
    if (composite == null) {
      buildComposite();
    }
    return composite;
  }

  protected void buildComposite() {
    composite =
      AlphaComposite.getInstance(AlphaComposite.SRC_OVER, opacity / 100.0f);
  }

  protected AlphaComposite getBorderComposite() {
    if (borderComposite == null) {
      borderComposite = buildBorderComposite();
    }
    return borderComposite;
  }

  protected AlphaComposite buildBorderComposite() {
    return AlphaComposite.getInstance(
      AlphaComposite.SRC_OVER, borderOpacity / 100.0f);
  }

  /**
   * Get/Build the shape of the shade.
   */
  protected Area getShadeShape(Map map) {
    final Area myShape = type.equals(FG_TYPE) ?
      new Area() : new Area(getBoardClip());
    Arrays.stream(map.getPieces()).forEach(p -> checkPiece(myShape, p));
    return myShape;
  }

  protected void checkPiece(Area area, GamePiece piece) {
    if (piece instanceof Stack) {
      final Stack s = (Stack) piece;
      s.asList().forEach(gamePiece -> checkPiece(area, gamePiece));
    }
    else {
      final ShadedPiece shaded = (ShadedPiece) Decorator.getDecorator(piece, ShadedPiece.class);
      if (shaded != null) {
        final Area shape = shaded.getArea(this);
        if (shape != null) {
          if (type.equals(FG_TYPE)) {
            area.add(shape);
          }
          else {
            area.subtract(shape);
          }
        }
      }
    }
  }

  /**
   * Get/Build the repeating rectangle used to generate the shade texture
   * pattern.
   * @deprecated Use {@link #getShadePattern(double)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected BufferedImage getShadePattern() {
    ProblemDialog.showDeprecated("2020-08-06");
    return getShadePattern(1.0);
  }

  protected BufferedImage getShadePattern(double zoom) {
    if (srcOp == null) {
      buildShadePattern();
    }

    final BufferedImage src = (zoom == 1.0 ? srcOp : Op.scale(srcOp, zoom)).getImage();
    // Linux xrender apparently requires translucency and its own copy
    return ImageUtils.toType(src, ImageUtils.getCompatibleTranslucentImageType());
  }

  /**
   * @deprecated Use {@link #getPatternRect(double)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected Rectangle getPatternRect() {
    ProblemDialog.showDeprecated("2020-08-06");
    return patternRect;
  }

  protected Rectangle getPatternRect(double zoom) {
    return zoom == 1.0 ? patternRect :
      new Rectangle(
        (int)Math.round(zoom * patternRect.width),
        (int)Math.round(zoom * patternRect.height)
      );
  }

  protected void buildShadePattern() {
    srcOp = pattern.equals(TYPE_IMAGE) && imageName != null
          ? Op.load(imageName) : new PatternOp(color, pattern);
    patternRect = new Rectangle(srcOp.getSize());
  }

  private static class PatternOp extends AbstractTileOpImpl {
    private final Color color;
    private final String pattern;
    private final int hash;

    public PatternOp(Color color, String pattern) {
      if (color == null || pattern == null)
        throw new IllegalArgumentException();
      this.color = color;
      this.pattern = pattern;

      hash = new HashCodeBuilder().append(color).append(pattern).toHashCode();
    }

    @Override
    public BufferedImage eval() throws Exception {
      final BufferedImage im =
        ImageUtils.createCompatibleTranslucentImage(2, 2);
      final Graphics2D g = im.createGraphics();
      g.setColor(color);
      if (TYPE_25_PERCENT.equals(pattern)) {
        g.drawLine(0, 0, 0, 0);
      }
      else if (TYPE_50_PERCENT.equals(pattern)) {
        g.drawLine(0, 0, 0, 0);
        g.drawLine(1, 1, 1, 1);
      }
      else if (TYPE_75_PERCENT.equals(pattern)) {
        g.drawLine(0, 0, 1, 0);
        g.drawLine(1, 1, 1, 1);
      }
      else {
        g.drawLine(0, 0, 1, 0);
        g.drawLine(0, 1, 1, 1);
      }
      g.dispose();
      return im;
    }

    @Override
    protected void fixSize() { }

    @Override
    public Dimension getSize() {
      return new Dimension(2, 2);
    }

    @Override
    public int getWidth() {
      return 2;
    }

    @Override
    public int getHeight() {
      return 2;
    }

    @Override
    public List<VASSAL.tools.opcache.Op<?>> getSources() {
      return Collections.emptyList();
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof PatternOp)) return false;
      return color.equals(((PatternOp) o).color) &&
             pattern.equals(((PatternOp) o).pattern);
    }

    @Override
    public int hashCode() {
      return hash;
    }
  }

  protected BasicStroke getStroke(double zoom) {
    if (stroke == null) {
      buildStroke(zoom);
    }
    return stroke;
  }

  protected void buildStroke(double zoom) {
    stroke = new BasicStroke((float) Math.min(borderWidth * zoom, 1.0),
                             BasicStroke.CAP_ROUND,
                             BasicStroke.JOIN_ROUND);
  }

  public Color getBorderColor() {
    return borderColor;
  }

  /**
   * Get/Build the textured paint used to fill in the Shade
   * @deprecated Use {@link #getTexture(double)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected TexturePaint getTexture() {
    ProblemDialog.showDeprecated("2020-08-06");
    return getTexture(1.0);
  }

  protected TexturePaint getTexture(double zoom) {
    final boolean usingScaledImage =
      scaleImage && imageName != null && pattern.equals(TYPE_IMAGE);

    if (!usingScaledImage) {
      zoom = 1.0;
    }

    texture = textures.computeIfAbsent(zoom, this::makeTexture);
    return texture;
  }

  /**
   * @deprecated Use {@link #buildTexture(double)} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  protected void buildTexture() {
    ProblemDialog.showDeprecated("2020-08-06");
    buildTexture(1.0);
  }

  protected void buildTexture(double zoom) {
    texture = makeTexture(zoom);
  }

  protected TexturePaint makeTexture(double zoom) {
    final BufferedImage pat = getShadePattern(zoom);
    return pat != null ? new TexturePaint(pat, getPatternRect(zoom)) : null;
  }

  public Color getColor() {
    return color;
  }

  /**
   * Is this Shade drawn over or under counters?
   */
  @Override
  public boolean drawAboveCounters() {
    return drawOver;
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      ALWAYS_ON,
      STARTS_ON,
      BUTTON_TEXT,
      TOOLTIP,
      ICON,
      HOT_KEY,
      BOARDS,
      BOARD_LIST,
      TYPE,
      DRAW_OVER,
      PATTERN,
      COLOR,
      IMAGE,
      SCALE_IMAGE,
      OPACITY,
      BORDER,
      BORDER_COLOR,
      BORDER_WIDTH,
      BORDER_OPACITY
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      Boolean.class,
      Boolean.class,
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class,
      BoardPrompt.class,
      String[].class,
      TypePrompt.class,
      Boolean.class,
      PatternPrompt.class,
      Color.class,
      Image.class,
      Boolean.class,
      Integer.class,
      Boolean.class,
      Color.class,
      Integer.class,
      Integer.class
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString("Editor.MapShader.shading_on"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.shading_start"), //$NON-NLS-1$
      Resources.getString(Resources.BUTTON_TEXT),
      Resources.getString(Resources.TOOLTIP_TEXT),
      Resources.getString(Resources.BUTTON_ICON),
      Resources.getString(Resources.HOTKEY_LABEL),
      Resources.getString("Editor.MapShader.shade_boards"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.board_list"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.type"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.shade_top"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.pattern"), //$NON-NLS-1$
      Resources.getString(Resources.COLOR_LABEL),
      Resources.getString("Editor.image_label"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.scale"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.opacity"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.border"), //$NON-NLS-1$
      Resources.getString("Editor.border_color"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.border_width"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.border_opacity"), //$NON-NLS-1$
    };
  }

  public static class TypePrompt extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{FG_TYPE, BG_TYPE};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.MapShader.foreground",
        "Editor.MapShader.background"
      };
    }
  }

  public static class PatternPrompt extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{TYPE_25_PERCENT, TYPE_50_PERCENT, TYPE_75_PERCENT, TYPE_SOLID, TYPE_IMAGE};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.MapShader.p25",
        "Editor.MapShader.p50",
        "Editor.MapShader.p75",
        "Editor.MapShader.solid",
        "Editor.MapShader.image"
      };
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public static class BoardPrompt extends TranslatableStringEnum {
    @Override
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALL_BOARDS, EXC_BOARDS, INC_BOARDS};
    }

    @Override
    public String[] getI18nKeys(AutoConfigurable target) {
      return new String[] {
        "Editor.MapShader.all_boards",
        "Editor.MapShader.exc_boards",
        "Editor.MapShader.inc_boards"
      };
    }
  }

  public void reset() {
    shadingVisible = isAlwaysOn() || isStartsOn();
  }

  protected void toggleShading() {
    setShadingVisibility(!shadingVisible);
  }

  public void setShadingVisibility(boolean b) {
    shadingVisible = b;
    map.repaint();
  }

  protected boolean isAlwaysOn() {
    return alwaysOn;
  }

  protected boolean isStartsOn() {
    return startsOn;
  }

  protected Map getMap() {
    return map;
  }

  public Area getBoardClip() {
    buildBoardClip();
    return boardClip;
  }

  /**
   * Build a clipping region excluding boards that do not needed to be Shaded.
   */
  protected void buildBoardClip() {
    if (boardClip == null) {
      boardClip = new Area();
      for (final Board b : map.getBoards()) {
        final String boardName = b.getName();
        boolean doShade = false;
        switch (boardSelection) {
        case ALL_BOARDS:
          doShade = true;
          break;
        case EXC_BOARDS:
          doShade = true;
          for (int i = 0; i < boardList.length && doShade; i++) {
            doShade = !boardList[i].equals(boardName);
          }
          break;
        case INC_BOARDS:
          for (int i = 0; i < boardList.length && !doShade; i++) {
            doShade = boardList[i].equals(boardName);
          }
          break;
        }
        if (doShade) {
          boardClip.add(new Area(b.bounds()));
        }
      }
    }
  }

  public void setLaunchButtonVisibility() {
    getLaunchButton().setVisible(!isAlwaysOn());
  }

  /*
   * -----------------------------------------------------------------------
   * GameComponent Implementation
   * -----------------------------------------------------------------------
   */
  @Override
  public void setup(boolean gameStarting) {
    getLaunchButton().setEnabled(gameStarting);
    if (!gameStarting) {
      boardClip = null;
    }
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  @Deprecated(since = "2020-10-01", forRemoval = true)
  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(
        key, name,
        ((MapShader) c).getLaunchButton().getAttributeValueString(ICON)
      );
    }
  }

  protected void buildPatternAndTexture() {
    textures.clear();
    buildShadePattern();
    buildTexture(1.0);
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      if (super.getAttributeValueString(TOOLTIP) == null) {
        super.setAttribute(TOOLTIP, value);
      }
    }
    else if (ALWAYS_ON.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      alwaysOn = (Boolean) value;
      setLaunchButtonVisibility();
      reset();
    }
    else if (STARTS_ON.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      startsOn = (Boolean) value;
      setLaunchButtonVisibility();
      reset();
    }
    else if (BOARDS.equals(key)) {
      boardSelection = (String) value;
    }
    else if (BOARD_LIST.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      boardList = (String[]) value;
    }
    else if (TYPE.equals(key)) {
      type = (String) value;
    }
    else if (DRAW_OVER.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      drawOver = (Boolean) value;
    }
    else if (PATTERN.equals(key)) {
      pattern = (String) value;
      buildPatternAndTexture();
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      // Bug 9969. Color configurer returns null if cancelled, so ignore a null.
      // and leave pattern and texture unchanged
      if (value != null) {
        color = (Color) value;
        buildPatternAndTexture();
      }
    }
    else if (IMAGE.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      imageName = (String) value;
      buildPatternAndTexture();
    }
    else if (SCALE_IMAGE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String)value);
      }
      scaleImage = (Boolean) value;
    }
    else if (BORDER.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      border = (Boolean) value;
    }
    else if (BORDER_COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      borderColor = (Color) value;
    }
    else if (BORDER_WIDTH.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      borderWidth = (Integer) value;
      if (borderWidth < 0) {
        borderWidth = 0;
      }
      stroke = null;
    }
    else if (OPACITY.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      opacity = (Integer) value;
      if (opacity < 0 || opacity > 100) {
        opacity = 100;
      }
      buildComposite();
    }
    else if (BORDER_OPACITY.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      borderOpacity = (Integer) value;
      if (borderOpacity < 0 || borderOpacity > 100) {
        borderOpacity = 100;
      }
      buildBorderComposite();
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (ALWAYS_ON.equals(key)) {
      return String.valueOf(isAlwaysOn());
    }
    else if (STARTS_ON.equals(key)) {
      return String.valueOf(isStartsOn());
    }
    else if (BOARDS.equals(key)) {
      return boardSelection;
    }
    else if (BOARD_LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(boardList);
    }
    else if (TYPE.equals(key)) {
      return type;
    }
    else if (DRAW_OVER.equals(key)) {
      return String.valueOf(drawOver);
    }
    else if (PATTERN.equals(key)) {
      return pattern;
    }
    else if (COLOR.equals(key)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (IMAGE.equals(key)) {
      return imageName;
    }
    else if (SCALE_IMAGE.equals(key)) {
      return String.valueOf(scaleImage);
    }
    else if (BORDER.equals(key)) {
      return String.valueOf(border);
    }
    else if (BORDER_COLOR.equals(key)) {
      return ColorConfigurer.colorToString(borderColor);
    }
    else if (BORDER_WIDTH.equals(key)) {
      return Integer.toString(borderWidth);
    }
    else if (OPACITY.equals(key)) {
      return Integer.toString(opacity);
    }
    else if (BORDER_OPACITY.equals(key)) {
      return Integer.toString(borderOpacity);
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (List.of(ICON, HOT_KEY, BUTTON_TEXT, STARTS_ON, TOOLTIP).contains(name)) {
      return () -> !isAlwaysOn();
    }
    else if (BOARD_LIST.equals(name)) {
      return () -> !boardSelection.equals(ALL_BOARDS);
    }
    else if (COLOR.equals(name)) {
      return () -> !pattern.equals(TYPE_IMAGE);
    }
    else if (IMAGE.equals(name)) {
      return () -> pattern.equals(TYPE_IMAGE);
    }
    else if (SCALE_IMAGE.equals(name)) {
      return () -> pattern.equals(TYPE_IMAGE);
    }
    else if (List.of(BORDER_COLOR, BORDER_WIDTH, BORDER_OPACITY).contains(name)) {
      return () -> border;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (imageName != null) s.add(imageName);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.MapShader.component_type"); //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getToolBar().remove(getLaunchButton());
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    map.removeDrawComponent(this);
    idMgr.remove(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "MapShading"); //NON-NLS
  }

  @Override
  public void addTo(Buildable parent) {
    final LaunchButton lb = getLaunchButton();
    GameModule.getGameModule().getToolBar().add(lb);
    lb.setAlignmentY(0.0F);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    map = (Map) parent;
    map.addDrawComponent(this);
    idMgr.add(this);
    validator = idMgr;
    setAttributeTranslatable(NAME, false);
  }

  @Override
  public String getId() {
    return id;
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  /**
   * Pieces that contribute to shading must implement this interface
   */
  @FunctionalInterface
  public interface ShadedPiece {
    /**
     * Returns the Area to add to (or subtract from) the area drawn by the MapShader's.
     * Area is assumed to be at zoom factor 1.0
     * @param shader Map Shader
     * @return the Area contributed by the piece
     */
    Area getArea(MapShader shader);
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Arrays.asList(boardList);
  }
}
