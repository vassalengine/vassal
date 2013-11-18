/*
 * $Id$
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
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang.builder.HashCodeBuilder;

import VASSAL.build.AbstractConfigurable;
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
import VASSAL.configure.StringEnum;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;
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
public class MapShader extends AbstractConfigurable implements GameComponent, Drawable, UniqueIdManager.Identifyable {

  public static final String NAME = "name";
  public static final String ALWAYS_ON = "alwaysOn";
  public static final String STARTS_ON = "startsOn";
  public static final String HOT_KEY = "hotkey";
  public static final String ICON = "icon";
  public static final String BUTTON_TEXT = "buttonText";
  public static final String TOOLTIP = "tooltip";
  public static final String BOARDS = "boards";
  public static final String BOARD_LIST = "boardList";

  public static final String ALL_BOARDS = "Yes";
  public static final String EXC_BOARDS = "No, exclude Boards in list";
  public static final String INC_BOARDS = "No, only shade Boards in List";

  protected static UniqueIdManager idMgr = new UniqueIdManager("MapShader");

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


  public static final String TYPE = "type";
  public static final String DRAW_OVER = "drawOver";
  public static final String PATTERN = "pattern";
  public static final String COLOR = "color";
  public static final String IMAGE = "image";
  public static final String SCALE_IMAGE = "scaleImage";
  public static final String OPACITY = "opacity";
  public static final String BORDER = "border";
  public static final String BORDER_COLOR = "borderColor";
  public static final String BORDER_WIDTH = "borderWidth";
  public static final String BORDER_OPACITY = "borderOpacity";

  public static final String BG_TYPE = "Background";
  public static final String FG_TYPE = "Foreground";

  public static final String TYPE_25_PERCENT = "25%";
  public static final String TYPE_50_PERCENT = "50%";
  public static final String TYPE_75_PERCENT = "75%";
  public static final String TYPE_SOLID = "100% (Solid)";
  public static final String TYPE_IMAGE = "Custom Image";

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
  @Deprecated protected BufferedImage shadePattern = null;
  protected Rectangle patternRect = new Rectangle();

  protected ImageOp srcOp;

  protected TexturePaint texture = null;
  protected java.util.Map<Double,TexturePaint> textures =
    new HashMap<Double,TexturePaint>();
  protected AlphaComposite composite = null;
  protected AlphaComposite borderComposite = null;
  protected BasicStroke stroke = null;

  public void draw(Graphics g, Map map) {
    if (shadingVisible) {

      double zoom = map.getZoom();
      buildStroke(zoom);

      final Graphics2D g2 = (Graphics2D) g;

      final Composite oldComposite = g2.getComposite();
      final Color oldColor = g2.getColor();
      final Paint oldPaint = g2.getPaint();
      final Stroke oldStroke = g2.getStroke();

      g2.setComposite(getComposite());
      g2.setColor(getColor());
      g2.setPaint(
        scaleImage && pattern.equals(TYPE_IMAGE) && imageName != null ?
        getTexture(zoom) : getTexture());
      Area area = getShadeShape(map);
      if (zoom != 1.0) {
        area = new Area(AffineTransform.getScaleInstance(zoom,zoom)
                                       .createTransformedShape(area));
      }
      g2.fill(area);
      if (border) {
        g2.setComposite(getBorderComposite());
        g2.setStroke(getStroke(map.getZoom()));
        g2.setColor(getBorderColor());
        g2.draw(area);
      }

      g2.setComposite(oldComposite);
      g2.setColor(oldColor);
      g2.setPaint(oldPaint);
      g2.setStroke(oldStroke);
    }
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

    for (GamePiece p : map.getPieces()) {
      checkPiece(myShape, p);
    }

    return myShape;
  }

  protected void checkPiece(Area area, GamePiece piece) {
    if (piece instanceof Stack) {
      Stack s = (Stack) piece;
      for (int i = 0; i < s.getPieceCount(); i++) {
        checkPiece(area, s.getPieceAt(i));
      }
    }
    else {
      ShadedPiece shaded = (ShadedPiece) Decorator.getDecorator(piece,ShadedPiece.class);
      if (shaded != null) {
        Area shape = shaded.getArea(this);
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
   */
  protected BufferedImage getShadePattern() {
    if (srcOp == null) buildShadePattern();
    return srcOp.getImage();
  }

  protected BufferedImage getShadePattern(double zoom) {
    if (srcOp == null) buildShadePattern();
    return Op.scale(srcOp,zoom).getImage();
  }

  protected Rectangle getPatternRect() {
    return patternRect;
  }

  protected Rectangle getPatternRect(double zoom) {
    return new Rectangle((int)Math.round(zoom*patternRect.width),(int)Math.round(zoom*patternRect.height));
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

    protected void fixSize() { }

    @Override
    public Dimension getSize() {
      return new Dimension(2,2);
    }

    @Override
    public int getWidth() {
      return 2;
    }

    @Override
    public int getHeight() {
      return 2;
    }

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
   */
  protected TexturePaint getTexture() {
    if (texture == null) {
      buildTexture();
    }
    return texture;
  }

  protected TexturePaint getTexture(double zoom) {
    if (zoom == 1.0) {
      return getTexture();
    }
    TexturePaint texture = textures.get(zoom);
    if (texture == null) {
      texture = new TexturePaint(getShadePattern(zoom),getPatternRect(zoom));
      textures.put(zoom,texture);
    }
    return texture;
  }

  protected void buildTexture() {
    if (getShadePattern() != null) {
      texture = new TexturePaint(getShadePattern(), getPatternRect());
    }
  }

  public Color getColor() {
    return color;
  }

  /**
   * Is this Shade drawn over or under counters?
   */
  public boolean drawAboveCounters() {
    return drawOver;
  }

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
      Resources.getString("Editor.MapShader.image"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.scale"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.opacity"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.border"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.border_color"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.border_width"), //$NON-NLS-1$
      Resources.getString("Editor.MapShader.border_opacity"), //$NON-NLS-1$
    };
  }

  public static class TypePrompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{FG_TYPE, BG_TYPE};
    }
  }

  public static class PatternPrompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{TYPE_25_PERCENT, TYPE_50_PERCENT, TYPE_75_PERCENT, TYPE_SOLID, TYPE_IMAGE};
    }
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public static class BoardPrompt extends StringEnum {
    public String[] getValidValues(AutoConfigurable target) {
      return new String[]{ALL_BOARDS, EXC_BOARDS, INC_BOARDS};
    }
  }

  public MapShader() {

    ActionListener al = new ActionListener() {
      public void actionPerformed(java.awt.event.ActionEvent e) {
        toggleShading();
      }
    };
    launch = new LaunchButton("Shade", TOOLTIP, BUTTON_TEXT, HOT_KEY, ICON, al);
    launch.setEnabled(false);
    setLaunchButtonVisibility();
    setConfigureName("Shading");
    reset();
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
      for (Board b : map.getBoards()) {
        String boardName = b.getName();
        boolean doShade = false;
        if (boardSelection.equals(ALL_BOARDS)) {
          doShade = true;
        }
        else if (boardSelection.equals(EXC_BOARDS)) {
          doShade = true;
          for (int i = 0; i < boardList.length && doShade; i++) {
            doShade = !boardList[i].equals(boardName);
          }
        }
        else if (boardSelection.equals(INC_BOARDS)) {
          for (int i = 0; i < boardList.length && !doShade; i++) {
            doShade = boardList[i].equals(boardName);
          }
        }
        if (doShade) {
          boardClip.add(new Area(b.bounds()));
        }
      }
    }
  }

  public void setLaunchButtonVisibility() {
    launch.setVisible(!isAlwaysOn());
  }

  /*
   * -----------------------------------------------------------------------
   * GameComponent Implementation
   * -----------------------------------------------------------------------
   */
  public void setup(boolean gameStarting) {
    launch.setEnabled(gameStarting);
    if (!gameStarting) {
      boardClip = null;
    }
  }

  public Command getRestoreCommand() {
    return null;
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((MapShader) c).launch.getAttributeValueString(ICON));
    }
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      if (launch.getAttributeValueString(TOOLTIP) == null) {
        launch.setAttribute(TOOLTIP, value);
      }
    }
    else if (ALWAYS_ON.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      alwaysOn = ((Boolean) value).booleanValue();
      setLaunchButtonVisibility();
      reset();
    }
    else if (STARTS_ON.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      startsOn = ((Boolean) value).booleanValue();
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
      drawOver = ((Boolean) value).booleanValue();
    }
    else if (PATTERN.equals(key)) {
      pattern = (String) value;
      buildShadePattern();
      buildTexture();
    }
    else if (COLOR.equals(key)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      // Bug 9969. Color configurer returns null if cancelled, so ignore a null.
      // and leave pattern and texture unchanged
      if (value != null) {
        color = (Color) value;
        buildShadePattern();
        buildTexture();
      }
    }
    else if (IMAGE.equals(key)) {
      if (value instanceof File) {
        value = ((File) value).getName();
      }
      imageName = (String) value;
      buildShadePattern();
      textures.clear();
      buildTexture();
    }
    else if (SCALE_IMAGE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String)value);
      }
      scaleImage = ((Boolean)value).booleanValue();
    }
    else if (BORDER.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      border = ((Boolean) value).booleanValue();
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
      borderWidth = ((Integer) value).intValue();
      if (borderWidth < 0) {
        borderWidth = 0;
      }
      stroke = null;
    }
    else if (OPACITY.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      opacity = ((Integer) value).intValue();
      if (opacity < 0 || opacity > 100) {
        opacity = 100;
      }
      buildComposite();
    }
    else if (BORDER_OPACITY.equals(key)) {
      if (value instanceof String) {
        value = Integer.valueOf((String) value);
      }
      borderOpacity = ((Integer) value).intValue();
      if (borderOpacity < 0 || borderOpacity > 100) {
        borderOpacity = 100;
      }
      buildBorderComposite();
    }
    else {
      launch.setAttribute(key, value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName() + "";
    }
    else if (ALWAYS_ON.equals(key)) {
      return String.valueOf(isAlwaysOn());
    }
    else if (STARTS_ON.equals(key)) {
      return String.valueOf(isStartsOn());
    }
    else if (BOARDS.equals(key)) {
      return boardSelection + "";
    }
    else if (BOARD_LIST.equals(key)) {
      return StringArrayConfigurer.arrayToString(boardList);
    }
    else if (TYPE.equals(key)) {
      return type + "";
    }
    else if (DRAW_OVER.equals(key)) {
      return String.valueOf(drawOver);
    }
    else if (PATTERN.equals(key)) {
      return pattern + "";
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
      return borderWidth + "";
    }
    else if (OPACITY.equals(key)) {
      return opacity + "";
    }
    else if (BORDER_OPACITY.equals(key)) {
      return borderOpacity + "";
    }
    else {
      return launch.getAttributeValueString(key);
    }

  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (ICON.equals(name) || HOT_KEY.equals(name) || BUTTON_TEXT.equals(name) || STARTS_ON.equals(name) || TOOLTIP.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !isAlwaysOn();
        }
      };
    }
    else if (BOARD_LIST.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !boardSelection.equals(ALL_BOARDS);
        }
      };
    }
    else if (COLOR.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !pattern.equals(TYPE_IMAGE);
        }
      };
    }
    else if (IMAGE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return pattern.equals(TYPE_IMAGE);
        }
      };
    }
    else if (SCALE_IMAGE.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return pattern.equals(TYPE_IMAGE);
        }};
    }
    else if (BORDER_COLOR.equals(name) || BORDER_WIDTH.equals(name) || BORDER_OPACITY.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return border;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.MapShader.component_type"); //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getToolBar().remove(launch);
    GameModule.getGameModule().getGameState().removeGameComponent(this);
    map.removeDrawComponent(this);
    idMgr.remove(this);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "MapShading");
  }

  public void addTo(Buildable parent) {
    GameModule.getGameModule().getToolBar().add(launch);
    launch.setAlignmentY(0.0F);
    GameModule.getGameModule().getGameState().addGameComponent(this);
    map = (Map) parent;
    map.addDrawComponent(this);
    idMgr.add(this);
    validator = idMgr;
    setAttributeTranslatable(NAME, false);
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  /**
   * Pieces that contribute to shading must implement this interface
   */
  public static interface ShadedPiece {
    /**
     * Returns the Area to add to (or subtract from) the area drawn by the MapShader's.
     * Area is assumed to be at zoom factor 1.0
     * @param shader
     * @return the Area contributed by the piece
     */
    public Area getArea(MapShader shader);
  }
}
