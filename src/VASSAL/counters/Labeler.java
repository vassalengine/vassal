/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
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

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.HashCode;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.imageop.AbstractTileOpImpl;
import VASSAL.tools.imageop.ImageOp;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.ScaleOp;

/**
 * Displays a text label, with content specified by the user at runtime.
 */
public class Labeler extends Decorator implements TranslatablePiece {
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
  private KeyStroke labelKey;
  private String menuCommand = "Change Label";
  private Font font = new Font("Dialog", 0, 10);
  private KeyCommand[] commands;
  private FormattedString nameFormat = new FormattedString("$" + PIECE_NAME + "$ ($" + LABEL + "$)");
  private FormattedString labelFormat = new FormattedString("");
  private static final String PIECE_NAME = "pieceName";
  private static final String LABEL = "label";

  private LabelOp srcOp;
  private ScaleOp scaleOp;

  private JLabel lbl;
  private char verticalJust = 'b';
  private char horizontalJust = 'c';
  private char verticalPos = 't';
  private char horizontalPos = 'c';
  private int verticalOffset = 0;
  private int horizontalOffset = 0;
  protected int rotateDegrees;
  protected String propertyName;
  protected KeyCommand menuKeyCommand;

  public Labeler() {
    this(ID, null);
  }

  public Labeler(String s, GamePiece d) {
    lbl = new JLabel();
    mySetType(s);
    setInner(d);
  }

  public void mySetType(String type) {
    commands = null;
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    labelKey = st.nextKeyStroke(null);
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
    nameFormat.setFormat(st.nextToken("$" + PIECE_NAME + "$ ($" + LABEL + "$)"));
    final String fontFamily = st.nextToken("Dialog");
    final int fontStyle = st.nextInt(Font.PLAIN);
    font = new Font(fontFamily, fontStyle, fontSize);
    rotateDegrees = st.nextInt(0);
    propertyName = st.nextToken("TextLabel");
    lbl.setForeground(textFg);
    lbl.setFont(font);
  }

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
      .append(propertyName);
    return ID + se.getValue();
  }

  public String myGetState() {
    return label;
  }

  public void mySetState(String s) {
    setLabel(s.trim());
  }

  public String getName() {
    if (label.length() == 0) {
      return piece.getName();
    }
    else {
      nameFormat.setProperty(PIECE_NAME, piece.getName());
      nameFormat.setProperty(LABEL, getLabel());
      return nameFormat.getText(Decorator.getOutermost(this));
    }
  }
  
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

  public static void drawLabel(Graphics g, String text, int x, int y, int hAlign, int vAlign, Color fgColor, Color bgColor) {
    drawLabel(g, text, x, y, new Font("Dialog", Font.PLAIN, 10), hAlign, vAlign, fgColor, bgColor, null);
  }

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

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    updateCachedImage();
    piece.draw(g, x, y, obs, zoom);

// FIXME: We should be drawing the text at the right size, not scaling it!
    if (srcOp != null) {
//    if (labelImage != null) {
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

      if (zoom == 1.0) {
        try {
          g2d.drawImage(srcOp.getImage(null), labelX, labelY, obs);
        }
        catch (CancellationException e) {
          e.printStackTrace();
        }
        catch (InterruptedException e) {
          e.printStackTrace();
        }
        catch (ExecutionException e) {
          e.printStackTrace();
        }
      }
      else {
        if (scaleOp == null || scaleOp.getScale() != zoom) {
          scaleOp = Op.scale(srcOp, zoom);
        }
        
        try {
          g2d.drawImage(scaleOp.getImage(null), labelX, labelY, obs);
        }
        catch (CancellationException e) {
          e.printStackTrace();
        }
        catch (InterruptedException e) {
          e.printStackTrace();
        }
        catch (ExecutionException e) {
          e.printStackTrace();
        }
/*
        Image scaled = labelImage;
        scaled = GameModule.getGameModule().getDataArchive().getScaledImage(labelImage, zoom);
        g2d.drawImage(scaled, labelX, labelY, obs);
*/
      }

      if (rotateDegrees != 0) {
        g2d.setTransform(saveXForm);
      }
    }
  }

  protected void updateCachedImage() {
    final String label = getLocalizedLabel();
    if (label != null && !label.equals(lastCachedLabel)) {
//      labelImage = null;
      srcOp = null;
    }
//    if (labelImage == null && label != null && label.length() > 0) {
    if (srcOp == null && label != null && label.length() > 0) {
//      labelImage = createImage(null);
      srcOp = new LabelOp(getLocalizedLabel(), lbl, textBg);
    }
  }

  /**
   * Return the relative position of the upper-left corner of the label, for a piece at position (0,0)
   */
  private Point getLabelPosition() {
    int x = horizontalOffset;
    int y = verticalOffset;

    Rectangle selBnds = piece.getShape().getBounds();
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
        y -= lbl.getHeight();
        break;
      case 'c':
        y -= lbl.getHeight() / 2;
    }
    switch (horizontalJust) {
      case 'c':
        x -= lbl.getWidth() / 2;
        break;
      case 'r':
        x -= lbl.getWidth();
    }
    return new Point(x, y);
  }

  public void setLabel(String s) {
    if (s == null) {
      s = "";
    }
    int index = s.indexOf("$" + propertyName + "$");
    while (index >= 0) {
      s = s.substring(0, index) +
          s.substring(index + propertyName.length() + 2);
      index = s.indexOf("$" + propertyName + "$");
    }
    label = s;
    labelFormat.setFormat(label);
    if (getMap() != null && label != null && label.length() > 0) {
//      labelImage = createImage(getMap().getView());
      srcOp = new LabelOp(getLocalizedLabel(), lbl, textBg);
    }
    else {
//      labelImage = null;
      srcOp = null;
    }
  }

  public void setBackground(Color textBg) {
    this.textBg = textBg;
  }

  public void setForeground(Color textFg) {
    this.textFg = textFg;
  }

  protected class LabelOp extends AbstractTileOpImpl {
    private final String txt;
    private final JLabel lab;
    private final Color bg;
    private final int hash;
    
    public LabelOp(String txt, JLabel lab, Color bg) {
      if (lab == null) throw new IllegalArgumentException();
      this.txt = txt;
      this.lab = lab;
      this.bg = bg;
      hash = HashCode.hash(txt) ^
             HashCode.hash(lab) ^
             HashCode.hash(bg);
    }

// FIXME: not implementing asynchronicity; maybe no point, here.
    public Image apply() throws Exception {
      lab.setText(txt);
      lab.setSize(lab.getPreferredSize());
      
      final int width = lab.getWidth();
      final int height = lab.getHeight();
      final BufferedImage im =
        new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
      final Graphics2D g = im.createGraphics();
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);
      if (bg != null) {
        g.setColor(bg);
        g.fillRect(0, 0, width, height);
      }
      lab.paint(g);
      g.dispose();
      return im;
    }

    protected void fixSize() {
      if ((size = getSizeFromCache()) == null) {
        lab.setText(txt);
        lab.setSize(lab.getPreferredSize());
        size = lab.getSize();
      }
    }

    public ImageOp getSource() {
      return null; 
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) return true;
      if (!(o instanceof LabelOp)) return false;
      LabelOp lop = (LabelOp) o;
      return lab.equals(lop.lab) &&
             ((txt != null && txt.equals(lop.txt)) ||
              (txt == null && lop.txt == null)) &&
             ((bg != null && bg.equals(lop.txt)) ||
              (bg == null && lop.bg == null));
    }

    @Override
    public int hashCode() {
      return hash;
    }
  }

/*
  protected Image createImage(Component obs) {
    lastCachedLabel = getLocalizedLabel();
    lbl.setText(lastCachedLabel);
    lbl.setSize(lbl.getPreferredSize());
    final int width = lbl.getWidth();
    final int height = lbl.getHeight();
    BufferedImage im =
      new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    final Graphics2D g = im.createGraphics();
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                       RenderingHints.VALUE_ANTIALIAS_ON);
    if (textBg != null) {
      g.setColor(textBg);
      g.fillRect(0, 0, width, height);
    }
    lbl.paint(g);

    g.dispose();

    return im;
  }
*/

  public String getLabel() {
    return labelFormat.getText(Decorator.getOutermost(this));
  }
  
  public String getLocalizedLabel() {
    final FormattedString f =
      new FormattedString(getTranslation(labelFormat.getFormat()));
    return f.getLocalizedText(Decorator.getOutermost(this));
  }

  public Rectangle boundingBox() {
    lbl.setText(getLabel());
    lbl.setSize(lbl.getPreferredSize());
    final Rectangle r = piece.boundingBox();
    final Rectangle r2 = piece.getShape().getBounds();
    final Point p2 = getLabelPosition();
    final Rectangle r3 = new Rectangle(p2, lbl.getSize());
    return r.union(r2).union(r3);
  }

  public Shape getShape() {
    if (labelKey != null) {
      return piece.getShape();
    }
    else {
      final Area a = new Area(piece.getShape());
      final Rectangle r = new Rectangle(getLabelPosition(), lbl.getSize());
      a.add(new Area(r));
      return a;
    }
  }

  public KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      menuKeyCommand = new KeyCommand(menuCommand, labelKey, Decorator.getOutermost(this), this);
      if (labelKey == null
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

  public String getDescription() {
    return "Text Label";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Label.htm");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  private static class Ed implements PieceEditor {
    private HotKeyConfigurer labelKeyInput;
    private JPanel controls = new JPanel();
    private StringConfigurer command;
    private StringConfigurer initialValue;
    private ColorConfigurer fg,bg;
    private JComboBox hPos,vPos,hJust,vJust;
    private IntConfigurer hOff,vOff,fontSize;
    private ListCellRenderer renderer;
    private FormattedStringConfigurer format;
    private JComboBox fontFamily;
    private IntConfigurer rotate;
    private BooleanConfigurer bold, italic;
    private StringConfigurer propertyNameConfig;

    public Ed(Labeler l) {
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      initialValue = new StringConfigurer(null, "Text:  ", l.label);
      controls.add(initialValue.getControls());

      format = new FormattedStringConfigurer(null, "Name format:  ", new String[]{PIECE_NAME, LABEL});
      format.setValue(l.nameFormat.getFormat());
      controls.add(format.getControls());

      command = new StringConfigurer(null, "Menu Command:  ", l.menuCommand);
      controls.add(command.getControls());

      labelKeyInput = new HotKeyConfigurer(null, "Keyboard Command:  ", l.labelKey);
      controls.add(labelKeyInput.getControls());

      Box b = Box.createHorizontalBox();
      b.add(new JLabel("Font:  "));
      fontFamily = new JComboBox();
      final String[] s = new String[]{
        "Serif", "SansSerif", "Monospaced", "Dialog", "DialogInput"
      };
      for (int i = 0; i < s.length; ++i) {
        fontFamily.addItem(s[i]);
      }
      fontFamily.setSelectedItem(l.font.getFamily());
      b.add(fontFamily);
      controls.add(b);

      b = Box.createHorizontalBox();
      fontSize = new IntConfigurer(null, "Font size:  ", new Integer(l.font.getSize()));
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

      final Character[] rightLeft = new Character[]{new Character('c'),
                                                    new Character('r'),
                                                    new Character('l')};

      final Character[] topBottom = new Character[]{new Character('c'),
                                                    new Character('t'),
                                                    new Character('b')};

      b = Box.createHorizontalBox();
      b.add(new JLabel("Vertical position:  "));
      vPos = new JComboBox(topBottom);
      vPos.setRenderer(renderer);
      vPos.setSelectedItem(new Character(l.verticalPos));
      b.add(vPos);
      vOff = new IntConfigurer(null, "  Offset:  ", new Integer(l.verticalOffset));
      b.add(vOff.getControls());
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Horizontal position:  "));
      hPos = new JComboBox(rightLeft);
      hPos.setRenderer(renderer);
      hPos.setSelectedItem(new Character(l.horizontalPos));
      b.add(hPos);
      hOff = new IntConfigurer(null, "  Offset:  ", new Integer(l.horizontalOffset));
      b.add(hOff.getControls());
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Vertical text justification:  "));
      vJust = new JComboBox(topBottom);
      vJust.setRenderer(renderer);
      vJust.setSelectedItem(new Character(l.verticalJust));
      b.add(vJust);
      controls.add(b);

      b = Box.createHorizontalBox();
      b.add(new JLabel("Horizontal text justification:  "));
      hJust = new JComboBox(rightLeft);
      hJust.setRenderer(renderer);
      hJust.setSelectedItem(new Character(l.horizontalJust));
      b.add(hJust);
      controls.add(b);

      rotate = new IntConfigurer(null, "Rotate Text (Degrees):  ", new Integer(l.rotateDegrees));
      controls.add(rotate.getControls());

      propertyNameConfig = new StringConfigurer(null, "Property Name:  ", l.propertyName);
      controls.add(propertyNameConfig.getControls());
    }

    public String getState() {
      return initialValue.getValueString();
    }

    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append((KeyStroke) labelKeyInput.getValue())
        .append(command.getValueString());

      Integer i = (Integer) fontSize.getValue();
      if (i == null
          || i.intValue() <= 0) {
        i = new Integer(10);
      }
      se.append(i.toString())
        .append(bg.getValueString())
        .append(fg.getValueString())
        .append(vPos.getSelectedItem().toString());
      i = (Integer) vOff.getValue();
      if (i == null) {
        i = new Integer(0);
      }
      se.append(i.toString())
        .append(hPos.getSelectedItem().toString());
      i = (Integer) hOff.getValue();
      if (i == null) {
        i = new Integer(0);
      }
      se.append(i.toString())
        .append(vJust.getSelectedItem().toString())
        .append(hJust.getSelectedItem().toString())
        .append(format.getValueString())
        .append(fontFamily.getSelectedItem().toString());
      final int style = Font.PLAIN +
        (bold.booleanValue().booleanValue() ? Font.BOLD : 0) +
        (italic.booleanValue().booleanValue() ? Font.ITALIC : 0);
      se.append(style + "");
      i = (Integer) rotate.getValue();
      if (i == null) {
        i = new Integer(0);
      }
      se.append(i.toString())
        .append(propertyNameConfig.getValueString());

      return ID + se.getValue();
    }

    public Component getControls() {
      return controls;
    }

    private static class MyRenderer extends DefaultListCellRenderer {
      private static final long serialVersionUID = 1L;

      public Component getListCellRendererComponent(JList list,
                                                    Object value,
                                                    int index,
                                                    boolean sel,
                                                    boolean focus) {
        super.getListCellRendererComponent(list, value, index, sel, focus);
        switch (((Character) value).charValue()) {
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
  
  public PieceI18nData getI18nData() {
    return getI18nData(
        new String[] {labelFormat.getFormat(), nameFormat.getFormat(), menuCommand}, 
        new String[] {"Label Text", "Label Format", "Change Label Command"});
  }
}
