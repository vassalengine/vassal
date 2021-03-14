/*
 * Copyright (c) 2000-2012 by Rodney Kinney, Joel Uckelman, Brent Easton
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

import VASSAL.build.GameModule;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.function.DoubleConsumer;

import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.plaf.basic.BasicHTML;

import net.miginfocom.swing.MigLayout;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.image.LabelUtils;
import VASSAL.tools.swing.SwingUtils;
import VASSAL.tools.imageop.AbstractTileOpImpl;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.ScaledImagePainter;

/**
 * Displays a text label, with content specified by the user at runtime.
 */
public class Labeler extends Decorator implements TranslatablePiece, Loopable {
  public static final String ID = "label;"; // NON-NLS
  protected Color textBg = Color.black;
  protected Color textFg = Color.white;

  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static final int CENTER = 0;
  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static final int RIGHT = 1;
  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static final int LEFT = 2;
  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static final int TOP = 3;
  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static final int BOTTOM = 4;

  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static int HORIZONTAL_ALIGNMENT = CENTER;
  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static int VERTICAL_ALIGNMENT = TOP;

  private String label = "";
  private String lastCachedLabel;
  private NamedKeyStroke labelKey;
  private String menuCommand = Resources.getString("Editor.TextLabel.change_label");
  private Font font = new Font(Font.DIALOG, Font.PLAIN, 10);
  private KeyCommand[] commands;
  private final FormattedString nameFormat = new FormattedString("$" + PIECE_NAME + "$ ($" + LABEL + "$)");
  private final FormattedString labelFormat = new FormattedString("");
  private static final String PIECE_NAME = "pieceName"; // NON-NLS
  private static final String BAD_PIECE_NAME = "PieceName"; // NON-NLS
  private static final String LABEL = "label"; // NON-NLS

  private double lastZoom = -1.0;
  private ImageOp lastCachedOp;
  private ImageOp baseOp;

  @Deprecated
  protected ScaledImagePainter imagePainter = new ScaledImagePainter();

  private char verticalJust = 'b';
  private char horizontalJust = 'c';
  private char verticalPos = 't';
  private char horizontalPos = 'c';
  private int verticalOffset = 0;
  private int horizontalOffset = 0;
  protected int rotateDegrees;
  protected String propertyName;
  protected KeyCommand menuKeyCommand;
  protected String description = "";

  private Point position = null; // Label position cache

  public Labeler() {
    this(ID, null);
  }

  public Labeler(String s, GamePiece d) {
    mySetType(s);
    setInner(d);
  }

  @Override
  public void mySetType(String type) {
    commands = null;
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    labelKey = st.nextNamedKeyStroke(null);
    menuCommand = st.nextToken(Resources.getString("Editor.TextLabel.change_label"));
    final int fontSize = st.nextInt(10);
    textBg = st.nextColor(null);
    textFg = st.nextColor(Color.black);
    verticalPos = st.nextChar('t');
    verticalOffset = st.nextInt(0);
    horizontalPos = st.nextChar('c');
    horizontalOffset = st.nextInt(0);
    verticalJust = st.nextChar('b');
    horizontalJust = st.nextChar('c');
    nameFormat.setFormat(clean(st.nextToken("$" + PIECE_NAME + "$ ($" + LABEL + "$)")));
    final String fontFamily = st.nextToken(Font.DIALOG);
    final int fontStyle = st.nextInt(Font.PLAIN);
    font = new Font(fontFamily, fontStyle, fontSize);
    rotateDegrees = st.nextInt(0);
    propertyName = st.nextToken("TextLabel"); // NON-NLS
    description = st.nextToken("");
  }

  /*
   * Clean up any property names that will cause an infinite loop when used in a label name
   */
  protected String clean(String s) {
    // Cannot use $PieceName$ in a label format, must use $pieceName$
    return s.replace("$" + BAD_PIECE_NAME + "$", "$" + PIECE_NAME + "$");
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (key.equals(propertyName)) {
      return getLocalizedLabel();
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return getLocalizedLabel() + piece.getProperty(key);
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }

  @Override
  public Object getProperty(Object key) {
    if (key.equals(propertyName)) {
      return getLabel();
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return getLabel() + piece.getProperty(key);
    }
    else {
      return super.getProperty(key);
    }
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(labelKey)
      .append(menuCommand)
      .append(font.getSize())
      .append(textBg)
      .append(textFg)
      .append(verticalPos)
      .append(verticalOffset)
      .append(horizontalPos)
      .append(horizontalOffset)
      .append(verticalJust)
      .append(horizontalJust)
      .append(nameFormat.getFormat())
      .append(font.getFamily())
      .append(font.getStyle())
      .append(rotateDegrees)
      .append(propertyName)
      .append(description);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    return label;
  }

  @Override
  public void mySetState(String s) {
    setLabel(s.trim());
  }

  @Override
  public String getName() {
    String result = "";
    if (label.length() == 0) {
      result =  piece.getName();
    }
    else {
      nameFormat.setProperty(PIECE_NAME, piece.getName());
      //
      // Bug 9483
      // Don't evaluate the label while reporting an infinite loop
      // Can cause further looping so that the infinite loop report
      // never finishes before a StackOverflow occurs
      //
      if (!RecursionLimiter.isReportingInfiniteLoop()) {
        nameFormat.setProperty(LABEL, getLabel());
      }
      try {
        RecursionLimiter.startExecution(this);
        result = nameFormat.getText(Decorator.getOutermost(this));
      }
      catch (RecursionLimitException e) {
        e.printStackTrace();
      }
      finally {
        RecursionLimiter.endExecution();
      }
    }
    return result;
  }

  @Override
  public String getLocalizedName() {
    if (label.length() == 0) {
      return piece.getLocalizedName();
    }
    else {
      final FormattedString f =
        new FormattedString(getTranslation(nameFormat.getFormat()));
      f.setProperty(PIECE_NAME, piece.getLocalizedName());
      f.setProperty(LABEL, getLocalizedLabel());
      return f.getLocalizedText(Decorator.getOutermost(this));
    }
  }

  public String getActualDescription() {
    return description;
  }
  private void updateCachedOpForZoomWindows(double zoom) {
    if (zoom == lastZoom && lastCachedOp != null) {
      return;
    }

    final float fsize = (float)(font.getSize() * zoom);

    // Windows renders some characters (e.g. "4") very poorly at 8pt. To
    // mitigate that, we upscale, render, then downscale when the font
    // would be 8pt.

    final boolean isHTML = BasicHTML.isHTMLString(lastCachedLabel);

    if (!isHTML && Math.round(fsize) == 8.0f) {
      final Font zfont = font.deriveFont(((float)(3 * font.getSize() * zoom)));
      lastCachedOp = Op.scale(new LabelOp(lastCachedLabel, zfont, textFg, textBg), 1.0 / 3.0);
    }
    else if (zoom == 1.0) {
      lastCachedOp = baseOp;
    }
    else if (isHTML) {
      lastCachedOp = Op.scale(baseOp, zoom);
    }
    else {
      final Font zfont = font.deriveFont(fsize);
      lastCachedOp = new LabelOp(lastCachedLabel, zfont, textFg, textBg);
    }

    lastZoom = zoom;
  }

  private void updateCachedOpForZoomNotWindows(double zoom) {
    if (zoom == lastZoom && lastCachedOp != null) {
      return;
    }

    if (zoom == 1.0) {
      lastCachedOp = baseOp;
    }
    else if (BasicHTML.isHTMLString(lastCachedLabel)) {
      lastCachedOp = Op.scale(baseOp, zoom);
    }
    else {
      final float fsize = (float)(font.getSize() * zoom);
      final Font zfont = font.deriveFont(fsize);
      lastCachedOp = new LabelOp(lastCachedLabel, zfont, textFg, textBg);
    }

    lastZoom = zoom;
  }

  private final DoubleConsumer updateCachedOpForZoom =
    SystemUtils.IS_OS_WINDOWS ? this::updateCachedOpForZoomWindows
                              : this::updateCachedOpForZoomNotWindows;

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);

    updateCachedImage();
    if (lastCachedLabel == null) {
      return;
    }

    updateCachedOpForZoom.accept(zoom);

    final Point p = getLabelPosition();
    final int labelX = x + (int) (zoom * p.x);
    final int labelY = y + (int) (zoom * p.y);

    AffineTransform saveXForm = null;
    final Graphics2D g2d = (Graphics2D) g;

    if (rotateDegrees != 0) {
      saveXForm = g2d.getTransform();
      final AffineTransform newXForm = AffineTransform.getRotateInstance(
        Math.toRadians(rotateDegrees), x, y);
      g2d.transform(newXForm);
    }

    g.drawImage(lastCachedOp.getImage(), labelX, labelY, obs);

    if (rotateDegrees != 0) {
      g2d.setTransform(saveXForm);
    }
  }

  protected void updateCachedImage() {
    final String ll = getLocalizedLabel();
    if (ll == null || ll.isEmpty()) {
      if (lastCachedLabel != null) {
        // label has changed to be empty
        position = null;
        lastCachedLabel = null;
        baseOp = lastCachedOp = null;
      }
    }
    else if (!ll.equals(lastCachedLabel)) {
      // label has changed, is non-empty
      position = null;
      lastCachedLabel = ll;

      if (BasicHTML.isHTMLString(lastCachedLabel)) {
        baseOp = new HTMLLabelOp(lastCachedLabel, font, textFg, textBg);
      }
      else {
        baseOp = new LabelOp(lastCachedLabel, font, textFg, textBg);
      }
      lastCachedOp = null;
    }
  }

  /**
   * Return the relative position of the upper-left corner of the label,
   * for a piece at position (0,0). Cache the position of the label once the label
   * image has been generated.
   */
  private Point getLabelPosition() {
    if (position != null) {
      return position;
    }
    int x = horizontalOffset;
    int y = verticalOffset;

    updateCachedImage();
    final Dimension lblSize = baseOp != null ? baseOp.getSize() : new Dimension();
    final Rectangle selBnds = piece.getShape().getBounds();

    switch (verticalPos) {
    case 't':
      y += selBnds.y;
      break;
    case 'b':
      y += selBnds.y + selBnds.height;
    }

    switch (horizontalPos) {
    case 'l':
      x += selBnds.x;
      break;
    case 'r':
      x += selBnds.x + selBnds.width;
    }

    switch (verticalJust) {
    case 'b':
      y -= lblSize.height;
      break;
    case 'c':
      y -= lblSize.height / 2;
    }

    switch (horizontalJust) {
    case 'c':
      x -= lblSize.width / 2;
      break;
    case 'r':
      x -= lblSize.width;
    }

    final Point result = new Point(x, y);

    // Cache the position once the label image has been generated
    if (lblSize.height > 0 && lblSize.width > 0) {
      position = result;
    }

    return result;
  }

  public void setLabel(String s) {
    if (s == null) s = "";

    int index = s.indexOf("$" + propertyName + "$");
    while (index >= 0) {
      s = s.substring(0, index) +
          s.substring(index + propertyName.length() + 2);
      index = s.indexOf("$" + propertyName + "$");
    }
    label = s;
    // prevent recursive references from this label
    // to piece name (which may contain this label)
    labelFormat.setProperty(BasicPiece.PIECE_NAME, piece.getName());
    labelFormat.setFormat(label);

    // clear cached values
    position = null;
    lastCachedLabel = null;
    baseOp = lastCachedOp = null;
  }

  public void setBackground(Color textBg) {
    this.textBg = textBg;
  }

  public void setForeground(Color textFg) {
    this.textFg = textFg;
  }

  protected static class LabelOp extends AbstractTileOpImpl {
    protected final String txt;
    protected final Font font;
    protected final Color fg;
    protected final Color bg;
    protected final int hash;

    public LabelOp(String txt, Font font, Color fg, Color bg) {
      this.txt = txt;
      this.font = font;
      this.fg = fg;
      this.bg = bg;
      hash = new HashCodeBuilder().append(txt)
                                  .append(font)
                                  .append(fg)
                                  .append(bg)
                                  .toHashCode();
    }

    @Override
    public List<VASSAL.tools.opcache.Op<?>> getSources() {
      return Collections.emptyList();
    }

    @Override
    public BufferedImage eval() throws Exception {
      // fix our size
      if (size == null) fixSize();

      // draw nothing if our size is zero
      if (size.width <= 0 || size.height <= 0) return ImageUtils.NULL_IMAGE;

      // prepare the target image
      final BufferedImage im = ImageUtils.createCompatibleImage(
        size.width,
        size.height,
        bg == null || bg.getTransparency() != Color.OPAQUE
      );

      final Graphics2D g = im.createGraphics();
      g.addRenderingHints(SwingUtils.FONT_HINTS);
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);

      // paint the background
      if (bg != null) {
        g.setColor(bg);
        g.fillRect(0, 0, size.width, size.height);
      }

      // paint the foreground
      if (fg != null) {
        g.setColor(fg);
        g.setFont(font);

        final FontMetrics fm = g.getFontMetrics(font);
        g.drawString(txt, 0, size.height - fm.getDescent());
      }

      g.dispose();
      return im;
    }

    protected Dimension buildDimensions() {
      final Graphics2D g = ImageUtils.NULL_IMAGE.createGraphics();
      final FontMetrics fm = g.getFontMetrics(font);
      final Dimension s = new Dimension(fm.stringWidth(txt), fm.getHeight());
      g.dispose();
      return s;
    }

    @Override
    protected void fixSize() {
      if ((size = getSizeFromCache()) == null) {
        final Dimension s = buildDimensions();
        // ensure that our area is nonempty
        if (s.width <= 0 || s.height <= 0) {
          s.width = s.height = 1;
        }
        size = s;
      }
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof LabelOp)) return false;

      final LabelOp lop = (LabelOp) o;
      return Objects.equals(txt, lop.txt) &&
             Objects.equals(font, lop.font) &&
             Objects.equals(fg, lop.fg) &&
             Objects.equals(bg, lop.bg);
    }

    @Override
    public int hashCode() {
      return hash;
    }
  }

  protected static class HTMLLabelOp extends LabelOp {
    public HTMLLabelOp(String txt, Font font, Color fg, Color bg) {
      super(txt, font, fg, bg);
    }

    @Override
    public BufferedImage eval() throws Exception {
      // fix our size
      if (size == null) fixSize();

      // draw nothing if our size is zero
      if (size.width <= 0 || size.height <= 0) return ImageUtils.NULL_IMAGE;

      // prepare the target image
      final BufferedImage im = ImageUtils.createCompatibleImage(
        size.width,
        size.height,
        bg == null || bg.getTransparency() != Color.OPAQUE
      );

      final Graphics2D g = im.createGraphics();
      g.addRenderingHints(SwingUtils.FONT_HINTS);
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);

      // paint the background
      if (bg != null) {
        g.setColor(bg);
        g.fillRect(0, 0, size.width, size.height);
      }

      // paint the foreground
      if (fg != null) {
        final JLabel l = makeLabel();
        l.paint(g);
      }

      g.dispose();
      return im;
    }

    protected JLabel makeLabel() {
      // Build a JLabel to render HTML
      final JLabel l = new JLabel(txt);
      l.setForeground(fg);
      l.setFont(font);
      l.setSize(l.getPreferredSize());
      return l;
    }

    @Override
    protected Dimension buildDimensions() {
      return makeLabel().getSize();
    }
  }

  public String getLabel() {
    return labelFormat.getText(Decorator.getOutermost(this));
  }

  public String getLocalizedLabel() {
    final FormattedString f =
      new FormattedString(getTranslation(labelFormat.getFormat()));
    return f.getLocalizedText(Decorator.getOutermost(this));
  }

  @Override
  public Rectangle boundingBox() {
    final Rectangle r = piece.boundingBox();
    r.add(new Rectangle(
      getLabelPosition(),
      baseOp != null ? baseOp.getSize() : new Dimension()
    ));
    return r;
  }

  @Deprecated(since = "2021-03-14", forRemoval = true)
  protected Rectangle lastRect = null;
  @Deprecated(since = "2021-03-14", forRemoval = true)
  protected Area lastShape = null;

  /**
   * Return the Shape of the counter by adding the shape of this label to the shape of all inner traits.
   * Minimize generation of new Area objects.
   */
  @Override
  public Shape getShape() {
    final Shape innerShape = piece.getShape();

    // If the label has a Control key, then the image of the label is NOT
    // included in the selectable area of the counter
    if (!labelKey.isNull()) {
      return innerShape;
    }

    final Rectangle r = new Rectangle(
      getLabelPosition(),
      baseOp != null ? baseOp.getSize() : new Dimension()
    );

    // If the label is completely enclosed in the current counter shape,
    // then we can just return the current shape
    if (innerShape.contains(r.x, r.y, r.width, r.height)) {
      return innerShape;
    }

    final Area a = new Area(innerShape);
    a.add(AreaCache.get(r));
    return a;
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      menuKeyCommand = new KeyCommand(menuCommand, labelKey, Decorator.getOutermost(this), this);
      if (labelKey == null
        || labelKey.isNull()
        || menuCommand == null
        || menuCommand.length() == 0) {
        commands = KeyCommand.NONE;
      }
      else {
        commands = new KeyCommand[]{menuKeyCommand};
      }
    }
    return commands;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command c = null;
    if (menuKeyCommand.matches(stroke)) {
      ChangeTracker tracker = new ChangeTracker(this);
      final String s = (String) JOptionPane.showInputDialog(
        getMap() == null ? GameModule.getGameModule().getPlayerWindow() : getMap().getView().getTopLevelAncestor(),
        menuKeyCommand.getName(),
        null,
        JOptionPane.QUESTION_MESSAGE,
        null,
        null,
        label
      );
      if (s == null) {
        tracker = null;
      }
      else {
        setLabel(s);
        c = tracker.getChangeCommand();
      }
    }
    return c;
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.TextLabel.component_type", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Label.html"); // NON-NLS
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public boolean testEquals(Object o) {

    // Check Class
    if (! (o instanceof Labeler)) return false;
    final Labeler l = (Labeler) o;

    // Check Type
    if (! Objects.equals(labelKey, l.labelKey)) return false;
    if (! Objects.equals(menuCommand, l.menuCommand)) return false;
    if (! Objects.equals(font, l.font)) return false;
    if (! Objects.equals(textBg, l.textBg)) return false;
    if (! Objects.equals(textFg, l.textFg)) return false;
    if (! Objects.equals(verticalPos, l.verticalPos)) return false;
    if (! Objects.equals(verticalOffset, l.verticalOffset)) return false;
    if (! Objects.equals(horizontalPos, l.horizontalPos)) return false;
    if (! Objects.equals(horizontalOffset, l.horizontalOffset)) return false;
    if (! Objects.equals(verticalJust, l.verticalJust)) return false;
    if (! Objects.equals(horizontalJust, l.horizontalJust)) return false;
    if (! Objects.equals(nameFormat, l.nameFormat)) return false;
    if (! Objects.equals(rotateDegrees, l.rotateDegrees)) return false;
    if (! Objects.equals(propertyName, l.propertyName)) return false;
    if (! Objects.equals(description, l.description)) return false;

    // Check State
    return Objects.equals(label, l.label);
  }
  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    if (propertyName.length() > 0) {
      l.add(propertyName);
    }
    return l;
  }

  private static class Ed implements PieceEditor {
    private final NamedHotKeyConfigurer labelKeyInput;
    private final TraitConfigPanel controls;
    private final StringConfigurer command;
    private final StringConfigurer initialValue;
    private final ColorConfigurer fg, bg;
    private final TranslatingStringEnumConfigurer vPos;
    private final TranslatingStringEnumConfigurer hPos;
    private final TranslatingStringEnumConfigurer vJust;
    private final TranslatingStringEnumConfigurer hJust;
    private final IntConfigurer hOff, vOff, fontSize;
    private final FormattedStringConfigurer format;
    private final TranslatingStringEnumConfigurer fontFamily;
    private final IntConfigurer rotate;
    private final BooleanConfigurer bold, italic;
    private final StringConfigurer propertyNameConfig;
    private final StringConfigurer descConfig;

    public Ed(Labeler l) {
      controls = new TraitConfigPanel();

      descConfig = new StringConfigurer(l.description);
      descConfig.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descConfig);

      initialValue = new StringConfigurer(l.label);
      initialValue.setHintKey("Editor.TextLabel.label_text_hint");
      controls.add("Editor.TextLabel.label_text", initialValue);

      format = new FormattedStringConfigurer(new String[]{PIECE_NAME, LABEL});
      format.setValue(l.nameFormat.getFormat()); // NON-NLS
      format.setHintKey("Editor.TextLabel.name_format_hint");
      controls.add("Editor.TextLabel.name_format", format); //NON-NLS

      command = new StringConfigurer(l.menuCommand);
      command.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.menu_command", command);

      labelKeyInput = new NamedHotKeyConfigurer(l.labelKey);
      controls.add("Editor.keyboard_command", labelKeyInput);

      fontFamily = new TranslatingStringEnumConfigurer(
        new String[]{Font.SERIF, Font.SANS_SERIF, Font.MONOSPACED, Font.DIALOG, Font.DIALOG_INPUT},
        new String[] {
          "Editor.Font.serif",
          "Editor.Font.sans_serif",
          "Editor.Font.monospaced",
          "Editor.Font.dialog",
          "Editor.Font.dialog_input"},
          l.font.getFamily());

      JPanel p = new JPanel(new MigLayout("ins 0", "[]unrel[]rel[]unrel[]rel[]unrel[]rel[]")); // NON-NLS
      p.add(fontFamily.getControls());
      p.add(new JLabel(Resources.getString("Editor.size_label")));
      fontSize = new IntConfigurer(l.font.getSize());
      p.add(fontSize.getControls());
      p.add(new JLabel(Resources.getString("Editor.TextLabel.bold")));
      final int fontStyle = l.font.getStyle();
      bold = new BooleanConfigurer(Boolean.valueOf(fontStyle != Font.PLAIN && fontStyle != Font.ITALIC));
      p.add(bold.getControls());
      p.add(new JLabel(Resources.getString("Editor.TextLabel.italic")));
      italic = new BooleanConfigurer(Boolean.valueOf(fontStyle != Font.PLAIN && fontStyle != Font.BOLD));
      p.add(italic.getControls());

      controls.add("Editor.TextLabel.label_font", p);

      fg = new ColorConfigurer(l.textFg);
      controls.add("Editor.TextLabel.text_color", fg);

      bg = new ColorConfigurer(l.textBg);
      controls.add("Editor.TextLabel.background_color", bg);

      vPos = new TranslatingStringEnumConfigurer(
        new String[] {"c", "t", "b"}, // NON-NLS
        new String[] {
          "Editor.center",
          "Editor.top",
          "Editor.bottom"
        },
        l.verticalPos
      );

      p = new JPanel(new MigLayout("ins 0", "[100]unrel[]rel[]")); // NON-NLS
      p.add(vPos.getControls());
      p.add(new JLabel(Resources.getString("Editor.TextLabel.offset")));
      vOff = new IntConfigurer(l.verticalOffset);
      p.add(vOff.getControls());
      controls.add("Editor.TextLabel.vertical_position", p);

      hPos = new TranslatingStringEnumConfigurer(
        new String[] {"c", "l", "r"}, // NON-NLS
        new String[] {
          "Editor.center",
          "Editor.left",
          "Editor.right"
        },
        l.horizontalPos
      );

      p = new JPanel(new MigLayout("ins 0", "[100]unrel[]rel[]")); // NON-NLS
      p.add(hPos.getControls());
      p.add(new JLabel(Resources.getString("Editor.TextLabel.offset")));
      hOff = new IntConfigurer(l.horizontalOffset);
      p.add(hOff.getControls());
      controls.add("Editor.TextLabel.horizontal_position", p);

      vJust = new TranslatingStringEnumConfigurer(
        new String[] {"c", "t", "b"}, // NON-NLS
        new String[] {
          "Editor.center",
          "Editor.top",
          "Editor.bottom"
        },
        l.verticalJust
      );
      controls.add("Editor.TextLabel.vertical_text_justification", vJust, "grow 0"); // NON-NLS

      hJust = new TranslatingStringEnumConfigurer(
        new String[] {"c", "l", "r"}, // NON-NLS
        new String[] {
          "Editor.center",
          "Editor.left",
          "Editor.right"
        },
        l.horizontalJust
      );
      controls.add("Editor.TextLabel.horizontal_text_justification", hJust, "grow 0"); // NON-NLS

      rotate = new IntConfigurer(l.rotateDegrees);
      controls.add("Editor.TextLabel.rotate_text_degrees", rotate);

      propertyNameConfig = new StringConfigurer(l.propertyName);
      propertyNameConfig.setHintKey("Editor.TextLabel.property_name_hint");
      controls.add("Editor.property_name", propertyNameConfig);
    }

    @Override
    public String getState() {
      return initialValue.getValueString();
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(labelKeyInput.getValueString())
        .append(command.getValueString());

      Integer i = (Integer) fontSize.getValue();
      if (i == null || i <= 0) {
        i = 10;
      }
      se.append(i.toString())
        .append(bg.getValueString())
        .append(fg.getValueString())
        .append(vPos.getValueString());
      i = (Integer) vOff.getValue();
      if (i == null) i = 0;

      se.append(i.toString())
        .append(hPos.getValueString());
      i = (Integer) hOff.getValue();
      if (i == null) i = 0;

      se.append(i.toString())
        .append(vJust.getValueString())
        .append(hJust.getValueString())
        .append(format.getValueString())
        .append(fontFamily.getValueString());
      final int style = Font.PLAIN +
        (bold.booleanValue() ? Font.BOLD : 0) +
        (italic.booleanValue() ? Font.ITALIC : 0);
      se.append(style);
      i = (Integer) rotate.getValue();
      if (i == null) i = 0;

      se.append(i.toString())
        .append(propertyNameConfig.getValueString())
        .append(descConfig.getValueString());

      return ID + se.getValue();
    }

    @Override
    public Component getControls() {
      return controls;
    }
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(
        new String[] {labelFormat.getFormat(), nameFormat.getFormat(), menuCommand},
        new String[] {
          Resources.getString("Editor.TextLabel.label_text"),
          Resources.getString("Editor.TextLabel.name_format"),
          Resources.getString("Editor.TextLabel.change_label_command")
        });
  }

  @Override
  public String getComponentTypeName() {
    // Use inner name to prevent recursive looping when reporting errors.
    return piece.getName();
  }

  @Override
  public String getComponentName() {
    return getDescription();
  }

  /** @deprecated Use {@link VASSAL.tools.image.LabelUtils#drawLabel(Graphics, String, int, int, int, int, Color, Color)} instead. **/
  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static void drawLabel(Graphics g, String text, int x, int y, int hAlign, int vAlign, Color fgColor, Color bgColor) {
    ProblemDialog.showDeprecated("2020-08-27");
    LabelUtils.drawLabel(g, text, x, y, hAlign, vAlign, fgColor, bgColor);
  }

  /** @deprecated Use {@link VASSAL.tools.image.LabelUtils#drawLabel(Graphics, String, int, int, Font, int, int, Color, Color, Color)} instead. **/
  @Deprecated(since = "2020-08-27", forRemoval = true)
  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor) {
    ProblemDialog.showDeprecated("2020-08-27");
    LabelUtils.drawLabel(g, text, x, y, f, hAlign, vAlign, fgColor, bgColor, borderColor);
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return List.of(propertyName);
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(labelKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(menuCommand);
  }

  /**
   * @return a list of any Message Format strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(label, nameFormat.getFormat(), labelFormat.getFormat());
  }

  /**
   * In case our labels refer to any image files
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    final HTMLImageFinder h = new HTMLImageFinder(label);
    h.addImageNames(s);
  }
}
