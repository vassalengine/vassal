/*
 *
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
import java.util.Collections;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.ListCellRenderer;
import javax.swing.plaf.basic.BasicHTML;

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
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.AbstractTileOpImpl;
import VASSAL.tools.imageop.ScaledImagePainter;

/**
 * Displays a text label, with content specified by the user at runtime.
 */
public class Labeler extends Decorator implements TranslatablePiece, Loopable {
  public static final String ID = "label;";
  protected Color textBg = Color.black;
  protected Color textFg = Color.white;

  public static final int CENTER = 0;
  public static final int RIGHT = 1;
  public static final int LEFT = 2;
  public static final int TOP = 3;
  public static final int BOTTOM = 4;

  public static int HORIZONTAL_ALIGNMENT = CENTER;
  public static int VERTICAL_ALIGNMENT = TOP;

  private String label = "";
  private String lastCachedLabel;
  private NamedKeyStroke labelKey;
  private String menuCommand = "Change Label";
  private Font font = new Font("Dialog", 0, 10);
  private KeyCommand[] commands;
  private FormattedString nameFormat = new FormattedString("$" + PIECE_NAME + "$ ($" + LABEL + "$)");
  private FormattedString labelFormat = new FormattedString("");
  private static final String PIECE_NAME = "pieceName";
  private static final String BAD_PIECE_NAME = "PieceName";
  private static final String LABEL = "label";

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
  protected String description;

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
    menuCommand = st.nextToken("Change Label");
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
    final String fontFamily = st.nextToken("Dialog");
    final int fontStyle = st.nextInt(Font.PLAIN);
    font = new Font(fontFamily, fontStyle, fontSize);
    rotateDegrees = st.nextInt(0);
    propertyName = st.nextToken("TextLabel");
    description = st.nextToken("");
  }

  /*
   * Clean up any property names that will cause an infinite loop when used in a label name
   */
  protected String clean(String s) {
    // Cannot use $PieceName$ in a label format, must use $pieceName$
    return s.replace("$"+BAD_PIECE_NAME+"$", "$"+PIECE_NAME+"$");
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
      return getLabel()+piece.getProperty(key);
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
      .append(font.getSize());
    String s = ColorConfigurer.colorToString(textBg);
    se.append(s == null ? "" : s);
    s = ColorConfigurer.colorToString(textFg);
    se.append(s == null ? "" : s)
      .append(String.valueOf(verticalPos))
      .append(String.valueOf(verticalOffset))
      .append(String.valueOf(horizontalPos))
      .append(String.valueOf(horizontalOffset))
      .append(String.valueOf(verticalJust))
      .append(String.valueOf(horizontalJust))
      .append(nameFormat.getFormat())
      .append(font.getFamily())
      .append(font.getStyle())
      .append(String.valueOf(rotateDegrees))
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
      if (! RecursionLimiter.isReportingInfiniteLoop()) {
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


// FIXME: This doesn't belong here. Should be in ImageUtils instead?
  public static void drawLabel(Graphics g, String text, int x, int y, int hAlign, int vAlign, Color fgColor, Color bgColor) {
    drawLabel(g, text, x, y, new Font("Dialog", Font.PLAIN, 10), hAlign, vAlign, fgColor, bgColor, null);
  }

// FIXME: This doesn't belong here. Should be in ImageUtils instead?
  public static void drawLabel(Graphics g, String text, int x, int y, Font f, int hAlign, int vAlign, Color fgColor, Color bgColor, Color borderColor) {
    g.setFont(f);
    final int width = g.getFontMetrics().stringWidth(text + "  ");
    final int height = g.getFontMetrics().getHeight();
    int x0 = x;
    int y0 = y;
    switch (hAlign) {
    case CENTER:
      x0 = x - width / 2;
      break;
    case LEFT:
      x0 = x - width;
      break;
    }
    switch (vAlign) {
    case CENTER:
      y0 = y - height / 2;
      break;
    case BOTTOM:
      y0 = y - height;
      break;
    }
    if (bgColor != null) {
      g.setColor(bgColor);
      g.fillRect(x0, y0, width, height);
    }
    if (borderColor != null) {
      g.setColor(borderColor);
      g.drawRect(x0, y0, width, height);
    }
    g.setColor(fgColor);
    ((Graphics2D) g).setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                      RenderingHints.VALUE_ANTIALIAS_ON);
    g.drawString(" " + text + " ", x0,
      y0 + g.getFontMetrics().getHeight() - g.getFontMetrics().getDescent());
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    updateCachedImage();
    piece.draw(g, x, y, obs, zoom);

// FIXME: We should be drawing the text at the right size, not scaling it!
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

    imagePainter.draw(g, labelX, labelY, zoom, obs);

    if (rotateDegrees != 0) {
      g2d.setTransform(saveXForm);
    }
  }

  protected void updateCachedImage() {
    final String label = getLocalizedLabel();
    if (label != null && !label.equals(lastCachedLabel)) {
      imagePainter.setSource(null);
      lastCachedLabel = null;
      position = null;
    }

    if (imagePainter.getSource() == null &&
        label != null && label.length() > 0) {
      lastCachedLabel = label;
      imagePainter.setSource(
        new LabelOp(lastCachedLabel, font, textFg, textBg));
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

    updateCachedImage();  // ensure that the LabelOp is set
    final Dimension lblSize = imagePainter.getImageSize();
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
    if (lblSize.height > 0 && lblSize.width > 0 ) {
      position = result;
    }

    return result;
  }

  public void setLabel(String s) {
    if (s == null) s = "";

    position = null;  // clear position cache

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

    if (getMap() != null && label != null && label.length() > 0) {
      imagePainter.setSource(
        new LabelOp(getLocalizedLabel(), font, textFg, textBg));
    }
    else {
      imagePainter.setSource(null);
    }
  }

  public void setBackground(Color textBg) {
    this.textBg = textBg;
  }

  public void setForeground(Color textFg) {
    this.textFg = textFg;
  }

  protected static class LabelOp extends AbstractTileOpImpl {
    private final String txt;
    private final Font font;
    private final Color fg;
    private final Color bg;
    private final int hash;

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
      // determine whether we are HTML and fix our size
      final JLabel label = buildDimensions();

      // draw nothing if our size is zero
      if (size.width <= 0 || size.height <= 0) return ImageUtils.NULL_IMAGE;

      // prepare the target image
      final BufferedImage im = ImageUtils.createCompatibleImage(
        size.width,
        size.height,
        bg == null || bg.getTransparency() != Color.OPAQUE
      );

      final Graphics2D g = im.createGraphics();
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);

      // paint the background
      if (bg != null) {
        g.setColor(bg);
        g.fillRect(0, 0, size.width, size.height);
      }

      // paint the foreground
      if (fg != null) {
        if (label != null) {
          label.paint(g);
        }
        else {
          g.setColor(fg);
          g.setFont(font);

          final FontMetrics fm = g.getFontMetrics(font);
          g.drawString(txt, 0, size.height - fm.getDescent());
        }
      }

      g.dispose();
      return im;
    }

    private JLabel buildDimensions() {
      // Build and return a JLabel if we need to render HTML,
      // then determine the dimensions of the label.
      final JLabel label;

      if (BasicHTML.isHTMLString(txt)) {
        label = new JLabel(txt);
        label.setForeground(fg);
        label.setFont(font);
        size = label.getPreferredSize();
        label.setSize(size);
      }
      else {
        label = null;

        final Graphics2D g = ImageUtils.NULL_IMAGE.createGraphics();
        final FontMetrics fm = g.getFontMetrics(font);
        size = new Dimension(fm.stringWidth(txt), fm.getHeight());
        g.dispose();
      }

      return label;
    }

    @Override
    protected void fixSize() {
      if ((size = getSizeFromCache()) == null) {
        buildDimensions();

        // ensure that our area is nonempty
        if (size.width <= 0 || size.height <= 0) {
          size.width = size.height = 1;
        }
      }
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof LabelOp)) return false;

      final LabelOp lop = (LabelOp) o;
      return (txt == null ? lop.txt == null : txt.equals(lop.txt)) &&
             (font == null ? lop.font == null : font.equals(lop.font)) &&
             (fg == null ? lop.fg == null : fg.equals(lop.fg)) &&
             (bg == null ? lop.bg == null : bg.equals(lop.bg));
    }

    @Override
    public int hashCode() {
      return hash;
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
    r.add(new Rectangle(getLabelPosition(), imagePainter.getImageSize()));
    return r;
  }

  protected Rectangle lastRect = null;
  protected Area lastShape = null;

  /**
   * Return the Shape of the counter by adding the shape of this label to the shape of all inner traits.
   * Minimize generation of new Area objects.
   */
  @Override
  public Shape getShape() {
    Shape innerShape = piece.getShape();

    // If the label has a Control key, then the image of the label is NOT included in the selectable area of the
    // counter
    if (! labelKey.isNull()) {
      return innerShape;
    }
    else {
      final Rectangle r = new Rectangle(getLabelPosition(), imagePainter.getImageSize());

      // If the label is completely enclosed in the current counter shape, then we can just return
      // the current shape
      if (innerShape.contains(r.x, r.y, r.width, r.height)) {
        return innerShape;
      }
      else {
        final Area a = new Area(innerShape);

        // Cache the Area object generated. Only recreate if the label position or size has changed
        if (!r.equals(lastRect)) {
          lastShape = new Area(r);
          lastRect = new Rectangle(r);
        }
        a.add(lastShape);
        return a;
      }
    }
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      menuKeyCommand = new KeyCommand(menuCommand, labelKey, Decorator.getOutermost(this), this);
      if (labelKey == null
        || labelKey.isNull()
        || menuCommand == null
        || menuCommand.length() == 0) {
        commands = new KeyCommand[0];
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
      final String s = (String) JOptionPane.showInputDialog
          (getMap() == null ? null : getMap().getView().getTopLevelAncestor(),
           menuKeyCommand.getName(),
           null,
           JOptionPane.QUESTION_MESSAGE,
           null,
           null,
           label);
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
    return "Text Label" + (description.length() > 0 ? (" - "+description) : "");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Label.htm");
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<>();
    if (propertyName.length() > 0) {
      l.add(propertyName);
    }
    return l;
  }

  private static class Ed implements PieceEditor {
    private NamedHotKeyConfigurer labelKeyInput;
    private JPanel controls = new JPanel();
    private StringConfigurer command;
    private StringConfigurer initialValue;
    private ColorConfigurer fg,bg;
    private JComboBox<Character> hPos, vPos, hJust, vJust;
    private IntConfigurer hOff,vOff,fontSize;
    private ListCellRenderer renderer;
    private FormattedStringConfigurer format;
    private JComboBox<String> fontFamily;
    private IntConfigurer rotate;
    private BooleanConfigurer bold, italic;
    private StringConfigurer propertyNameConfig;
    private StringConfigurer descConfig;

    public Ed(Labeler l) {
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      descConfig = new StringConfigurer(null, "Description:  ", l.description);
      controls.add(descConfig.getControls());

      initialValue = new StringConfigurer(null, "Text:  ", l.label);
      controls.add(initialValue.getControls());

      format = new FormattedStringConfigurer(null, "Name format:  ", new String[]{PIECE_NAME, LABEL});
      format.setValue(l.nameFormat.getFormat());
      controls.add(format.getControls());

      command = new StringConfigurer(null, "Menu Command:  ", l.menuCommand);
      controls.add(command.getControls());

      labelKeyInput = new NamedHotKeyConfigurer(null, "Keyboard Command:  ", l.labelKey);
      controls.add(labelKeyInput.getControls());

      Box b = Box.createHorizontalBox();
      b.add(new JLabel("Font:  "));
      fontFamily = new JComboBox<String>();
      final String[] s = new String[]{
        "Serif", "SansSerif", "Monospaced", "Dialog", "DialogInput"
      };
      for (String value : s) {
        fontFamily.addItem(value);
      }
      fontFamily.setSelectedItem(l.font.getFamily());
      b.add(fontFamily);
      controls.add(b);

      b = Box.createHorizontalBox();
      fontSize = new IntConfigurer(null, "Font size:  ", l.font.getSize());
      b.add(fontSize.getControls());
      b.add(new JLabel("  Bold?"));
      final int fontStyle = l.font.getStyle();
      bold = new BooleanConfigurer(null, "",
        Boolean.valueOf(fontStyle != Font.PLAIN && fontStyle != Font.ITALIC));
      b.add(bold.getControls());
      b.add(new JLabel("  Italic?"));
      italic = new BooleanConfigurer(null, "",
        Boolean.valueOf(fontStyle != Font.PLAIN && fontStyle != Font.BOLD));
      b.add(italic.getControls());
      controls.add(b);

      b = Box.createHorizontalBox();
      fg = new ColorConfigurer(null, "Text Color:  ", l.textFg);
      b.add(fg.getControls());

      bg = new ColorConfigurer(null, "  Background Color:  ", l.textBg);
      b.add(bg.getControls());
      controls.add(b);

      renderer = new MyRenderer();

      final Character[] rightLeft = new Character[]{'c', 'r', 'l'};
      final Character[] topBottom = new Character[]{'c', 't', 'b'};

      b = Box.createHorizontalBox();
      b.add(new JLabel("Vertical position:  "));
      vPos = new JComboBox<Character>(topBottom);
      vPos.setRenderer(renderer);
      vPos.setSelectedItem(l.verticalPos);
      b.add(vPos);
      vOff = new IntConfigurer(null, "  Offset:  ", l.verticalOffset);
      b.add(vOff.getControls());
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Horizontal position:  "));
      hPos = new JComboBox<Character>(rightLeft);
      hPos.setRenderer(renderer);
      hPos.setSelectedItem(l.horizontalPos);
      b.add(hPos);
      hOff = new IntConfigurer(null, "  Offset:  ", l.horizontalOffset);
      b.add(hOff.getControls());
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Vertical text justification:  "));
      vJust = new JComboBox<Character>(topBottom);
      vJust.setRenderer(renderer);
      vJust.setSelectedItem(l.verticalJust);
      b.add(vJust);
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Horizontal text justification:  "));
      hJust = new JComboBox<Character>(rightLeft);
      hJust.setRenderer(renderer);
      hJust.setSelectedItem(l.horizontalJust);
      b.add(hJust);
      controls.add(b);

      rotate = new IntConfigurer(null, "Rotate Text (Degrees):  ", l.rotateDegrees);
      controls.add(rotate.getControls());

      propertyNameConfig = new StringConfigurer(null, "Property Name:  ", l.propertyName);
      controls.add(propertyNameConfig.getControls());
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
        .append(vPos.getSelectedItem().toString());
      i = (Integer) vOff.getValue();
      if (i == null) i = 0;

      se.append(i.toString())
        .append(hPos.getSelectedItem().toString());
      i = (Integer) hOff.getValue();
      if (i == null) i = 0;

      se.append(i.toString())
        .append(vJust.getSelectedItem().toString())
        .append(hJust.getSelectedItem().toString())
        .append(format.getValueString())
        .append(fontFamily.getSelectedItem().toString());
      final int style = Font.PLAIN +
        (bold.booleanValue() ? Font.BOLD : 0) +
        (italic.booleanValue() ? Font.ITALIC : 0);
      se.append(style + "");
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

    private static class MyRenderer extends DefaultListCellRenderer {
      private static final long serialVersionUID = 1L;

      @Override
      public Component getListCellRendererComponent(JList list,
                                                    Object value,
                                                    int index,
                                                    boolean sel,
                                                    boolean focus) {
        super.getListCellRendererComponent(list, value, index, sel, focus);
        switch ((Character) value) {
        case 't':
          setText("Top");
          break;
        case 'b':
          setText("Bottom");
          break;
        case 'c':
          setText("Center");
          break;
        case 'l':
          setText("Left");
          break;
        case 'r':
          setText("Right");
        }
        return this;
      }
    }
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(
        new String[] {labelFormat.getFormat(), nameFormat.getFormat(), menuCommand},
        new String[] {"Label Text", "Label Format", "Change Label Command"});
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
}
