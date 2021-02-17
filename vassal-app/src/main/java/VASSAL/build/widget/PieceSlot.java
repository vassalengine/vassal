/*
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
 *
 */
package VASSAL.build.widget;

import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;
import VASSAL.build.Widget;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MenuDisplayer;
import VASSAL.build.module.map.PieceMover.AbstractDragHandler;
import VASSAL.command.AddPiece;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.Decorator;
import VASSAL.counters.DragBuffer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.PieceDefiner;
import VASSAL.counters.PlaceMarker;
import VASSAL.counters.Properties;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Resources;
import VASSAL.search.ImageSearchTarget;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.image.LabelUtils;
import VASSAL.tools.swing.SwingUtils;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.RenderingHints;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.awt.image.BufferedImage;
import java.util.Collection;

import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * A Component that displays a GamePiece.
 *
 * Can be added to any Widget but cannot contain any children Keyboard input on
 * a PieceSlot is forwarded to the {@link GamePiece#keyEvent} method for the
 * PieceSlot's GamePiece. Clicking on a PieceSlot initiates a drag
 */
public class PieceSlot extends Widget implements MouseListener, KeyListener {
  public static final String GP_ID = "gpid"; //NON-NLS
  private static final int DEFAULT_SIZE = 64;
  private static final BufferedImage noImage = LabelUtils.noImageBoxImage(DEFAULT_SIZE, DEFAULT_SIZE, 1.0);

  protected GamePiece c;
  protected GamePiece expanded;
  protected String pieceDefinition;
  protected static final Font FONT = new Font(Font.DIALOG, Font.ITALIC, 12);
  protected final JPanel panel;
  protected int width, height;
  protected String gpId = ""; // Unique PieceSlot Id
  protected GpIdSupport gpidSupport;


  public PieceSlot() {
    panel = new PieceSlot.Panel(this);
    panel.addMouseListener(this);
    panel.addKeyListener(this);
  }

  public PieceSlot(PieceSlot piece) {
    this();
    copyFrom(piece);
  }

  public PieceSlot(CardSlot card) {
    this((PieceSlot) card);
  }

  public String getName() {
    return name;
  }

  public String getPieceDefinition() {
    return pieceDefinition;
  }

  // If we're a child of a piece widget that allows scale control, get our scale from that. Otherwise default to 1.0
  @Override
  public double getScale() {
    Widget w = this;
    while ((w = w.getParent()) != null) {
      if (w.hasScale()) {
        return w.getScale();
      }
    }
    return 1.0;
  }

  protected void copyFrom(PieceSlot piece) {
    c = piece.c;
    name = piece.name;
    pieceDefinition = piece.pieceDefinition;
    gpidSupport = piece.gpidSupport;
    gpId = piece.gpId;
  }

  public class Panel extends JPanel {
    private static final long serialVersionUID = 1L;
    protected final PieceSlot pieceSlot;

    public Panel(PieceSlot slot) {
      super();
      setFocusTraversalKeysEnabled(false);
      pieceSlot = slot;
    }

    public PieceSlot getPieceSlot() {
      return pieceSlot;
    }

    @Override
    public void paint(Graphics g) {
      PieceSlot.this.paint(g);
    }

    @Override
    public Dimension getPreferredSize() {
      return PieceSlot.this.getPreferredSize();
    }
  }

  /**
   * Has the PieceSlot been rendered for the first time yet?
   * @return true if slot has been rendered
   */
  public boolean isValid() {
    return panel.isValid() && panel.getGraphics() != null;
  }

  public PieceSlot(GamePiece p) {
    this();
    setPiece(p);
  }

  public void setPiece(GamePiece p) {
    c = p;
    clearExpandedPiece();
    if (c != null) {
      final Dimension size = panel.getSize();
      c.setPosition(new Point(size.width / 2, size.height / 2));
      name = Decorator.getInnermost(c).getName();
    }
    panel.revalidate();
    panel.repaint();
    pieceDefinition = c == null ? null : GameModule.getGameModule().encode(new AddPiece(c));
  }

  /**
   * Return defined GamePiece with prototypes fully expanded.
   *
   * @return expanded piece
   */
  protected GamePiece getExpandedPiece() {
    if (expanded == null) {
      final GamePiece p = getPiece();
      if (p != null) {  // Possible when PlaceMarker is building
        expanded = PieceCloner.getInstance().clonePiece(p);
      }
    }
    return expanded;
  }

  protected void clearExpandedPiece() {
    expanded = null;
  }

  /**
   * Return defined GamePiece with prototypes unexpanded.
   *
   * @return unexpanded piece
   */
  public GamePiece getPiece() {
    if (c == null && pieceDefinition != null) {
      final Command raw = GameModule.getGameModule().decode(pieceDefinition);
      final AddPiece comm = (raw instanceof AddPiece) ? (AddPiece) raw : null;  // In a "bad data" situation this can happen too.
      if ((comm == null) || comm.isNull()) {
        ErrorDialog.dataWarning(new BadDataReport("GamePiece - couldn't build piece -", pieceDefinition));  //NON-NLS
        pieceDefinition = null;
      }
      else {
        c = comm.getTarget();
        c.setState(comm.getState());

        final Dimension size = panel.getSize();
        c.setPosition(new Point(size.width / 2, size.height / 2));
      }
    }

    if (c != null) {
      c.setProperty(Properties.PIECE_ID, getGpId());
    }

    return c;
  }

  @Override
  public void addImageNamesRecursively(Collection<String> s) {
    super.addImageNamesRecursively(s);

    final GamePiece p = getPiece();
    if (p instanceof ImageSearchTarget) {
      ((ImageSearchTarget) p).addImageNamesRecursively(s);
    }
  }

  public void paint(Graphics g) {
    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final AffineTransform orig_t = g2d.getTransform();
    g2d.setTransform(SwingUtils.descaleTransform(orig_t));

    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON);

    final Dimension size = panel.getSize();
    size.width *= os_scale;
    size.height *= os_scale;

    final Color c = g.getColor();
    g.setColor(Color.WHITE);
    g.fillRect(0, 0, size.width, size.height);
    g.setColor(c);

    if (getExpandedPiece() == null ||  getExpandedPiece().boundingBox().width == 0) {
      // No image, draw a visible placeholder
      final BufferedImage noimg = getNullImage(os_scale);
      g2d.drawImage(
        noimg,
        (size.width - noimg.getWidth()) / 2,
        (size.height - noimg.getHeight()) / 2,
        null
      );
    }
    else {
      // We apply both our os_scale and our module-specified scale as factors
      getExpandedPiece().draw(g, size.width / 2, size.height / 2, panel, os_scale * getScale());

      // NB: The piece, not the expanded piece, receives events, so we check
      // the piece, not the expanded piece, for its selection status.
      if (Boolean.TRUE.equals(getPiece().getProperty(Properties.SELECTED))) {
        BasicPiece.getHighlighter().draw(getExpandedPiece(), g,
          size.width / 2, size.height / 2, panel, os_scale * getScale());
      }
    }

    g2d.setTransform(orig_t);
  }

  private BufferedImage getNullImage(double scale) {
    return scale == 1.0 ? noImage :
      LabelUtils.noImageBoxImage(
        (int)(DEFAULT_SIZE * scale),
        (int)(DEFAULT_SIZE * scale),
        scale
      );
  }

  public Dimension getPreferredSize() {
    // Preferred size is affected by our module-specified scale
    if (c != null && panel.getGraphics() != null) {
      final Dimension bound = c.boundingBox().getSize();
      bound.width = (int) ((bound.width == 0 ? getNullImage(1.0).getWidth() : bound.width) * getScale());
      bound.height = (int) ((bound.height == 0 ? getNullImage(1.0).getHeight() : bound.height) * getScale());
      return bound;
    }
    else {
      return new Dimension(
        (int) (getNullImage(1.0).getWidth() * getScale()),
        (int) (getNullImage(1.0).getHeight() * getScale())
      );
    }
  }

  // Puts counter in DragBuffer. Call when mouse gesture recognized
  protected void startDrag() {
    // Recenter piece; panel may have been resized at some point resulting
    // in pieces with inaccurate positional information.
    final GamePiece p = getPiece();
    if (p == null) { // Found new ways to NPE after you've successfully soft-warninged your failed piece build :)
      return;
    }

    final Dimension size = panel.getSize();
    p.setPosition(new Point(size.width / 2, size.height / 2));

    // Erase selection border to avoid leaving selected after mouse dragged out
    p.setProperty(Properties.SELECTED, null);
    panel.repaint();

    KeyBuffer.getBuffer().clear();
    DragBuffer.getBuffer().clear();
    final GamePiece newPiece = PieceCloner.getInstance().clonePiece(getPiece());
    newPiece.setProperty(Properties.PIECE_ID, getGpId());
    DragBuffer.getBuffer().add(newPiece);
  }

  protected void doPopup(MouseEvent e) {
    final JPopupMenu popup = MenuDisplayer.createPopup(getPiece());
    popup.addPopupMenuListener(new PopupMenuListener() {
      @Override
      public void popupMenuCanceled(PopupMenuEvent evt) {
        panel.repaint();
      }

      @Override
      public void popupMenuWillBecomeInvisible(PopupMenuEvent evt) {
        clearExpandedPiece();
        panel.repaint();
      }

      @Override
      public void popupMenuWillBecomeVisible(PopupMenuEvent evt) {
      }
    });
    popup.show(panel, e.getX(), e.getY());
  }

  @Override
  public void mousePressed(MouseEvent e) {
    if (e.isPopupTrigger()) {
      doPopup(e);
    }
    else if (SwingUtils.isMainMouseButtonDown(e)) {
      KeyBuffer.getBuffer().clear();
      Map.clearActiveMap();
      if (getPiece() != null) {
        KeyBuffer.getBuffer().add(getPiece());
      }

      clearExpandedPiece();
      panel.requestFocus();
      panel.repaint();
    }
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    boolean doClear = false;
    if (e.isPopupTrigger()) {
      if (getPiece() != null) {
        doPopup(e);
      }
      doClear = true;
    }
    else if (SwingUtils.isMainMouseButtonDown(e)) {
      doClear = true;
    }

    if (doClear) {
      clearExpandedPiece();
    }
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
    KeyBuffer.getBuffer().remove(getPiece());
    clearExpandedPiece();
    panel.repaint();
  }

  @Override
  public void keyPressed(KeyEvent e) {
    KeyBuffer.getBuffer().keyCommand(SwingUtils.getKeyStrokeForEvent(e));
    e.consume();
    clearExpandedPiece();
    panel.repaint();
  }

  @Override
  public void keyTyped(KeyEvent e) {
    KeyBuffer.getBuffer().keyCommand(SwingUtils.getKeyStrokeForEvent(e));
    e.consume();
    clearExpandedPiece();
    panel.repaint();
  }

  @Override
  public void keyReleased(KeyEvent e) {
    KeyBuffer.getBuffer().keyCommand(SwingUtils.getKeyStrokeForEvent(e));
    e.consume();
    clearExpandedPiece();
    panel.repaint();
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PieceSlot.component_type");
  }

  @Override
  public Component getComponent() {
    return panel;
  }

  /**
   * When building a PieceSlot, the text contents of the XML element are parsed
   * into a String. The String is decoded using {@link GameModule#decode}. The
   * resulting {@link Command} should be an instance of {@link AddPiece}. The
   * piece referred to in the Command becomes the piece contained in the
   * PieceSlot
   */
  @Override
  public void build(Element e) {
    gpidSupport = GameModule.getGameModule().getGpIdSupport();
    if (e != null) {
      name = e.getAttribute(NAME);
      gpId = e.getAttribute(GP_ID);
      if (name.length() == 0) {
        name = null;
      }
      try {
        width = Integer.parseInt(e.getAttribute(WIDTH));
        height = Integer.parseInt(e.getAttribute(HEIGHT));
      }
      catch (final NumberFormatException ex) {
        // Use default values.  Will be overwritten when module is saved
        width = DEFAULT_SIZE;
        height = DEFAULT_SIZE;
      }
      pieceDefinition = Builder.getText(e);
      c = null;
    }
  }

  @Override
  public void addTo(Buildable par) {
    // Need to "keep our Widgets in a row" for scale purposes
    if (par instanceof Widget) {
      parent = (Widget)par;
    }

    panel.setDropTarget(AbstractDragHandler.makeDropTarget(panel, DnDConstants.ACTION_MOVE, null));

    final DragGestureListener dragGestureListener = dge -> {
      if (SwingUtils.isDragTrigger(dge)) {
        startDrag();
        AbstractDragHandler.getTheDragHandler().dragGestureRecognized(dge);
      }
    };

    DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(
      panel, DnDConstants.ACTION_MOVE, dragGestureListener
    );
  }

  @Override
  public Element getBuildElement(Document doc) {
    final Element el = doc.createElement(getClass().getName());
    final String s = getConfigureName();
    if (s != null) {
      el.setAttribute(NAME, s);
    }
    el.setAttribute(GP_ID, gpId);
    el.setAttribute(WIDTH, Integer.toString(getPreferredSize().width));
    el.setAttribute(HEIGHT, Integer.toString(getPreferredSize().height));

    if (c != null || pieceDefinition != null) {
      el.appendChild(doc.createTextNode(
        c == null ? pieceDefinition :
          GameModule.getGameModule().encode(new AddPiece(c))
      ));
    }
    return el;
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public String getConfigureName() {
    if (name != null) {
      return name;
    }
    else if (getPiece() != null) {
      return Decorator.getInnermost(getPiece()).getName();
    }
    else {
      return null;
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePiece.html"); //NON-NLS
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  @Override
  public void setAttribute(String name, Object value) {
  }

  /**
   * @return an array of Configurer objects representing the Buildable children
   *         of this Configurable object
   */
  @Override
  public Configurable[] getConfigureComponents() {
    return new Configurable[0];
  }

  /**
   * @return an array of Configurer objects representing all possible classes of
   *         Buildable children of this Configurable object
   */
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  /*
   * Redirect getAttributeValueString() to return the attribute
   * values for the enclosed pieces
   */
  @Override
  public String getAttributeValueString(String attr) {
    return getI18nData().getLocalUntranslatedValue(attr);
  }


  @Override
  public ComponentI18nData getI18nData() {
    /*
     * Piece can change due to editing, so cannot cache the I18nData
     */
    return new ComponentI18nData(this, getPiece());
  }

  @Override
  public Configurer getConfigurer() {
    return new MyConfigurer(this);
  }

  /**
   * Update the gpid for this PieceSlot, using the given {@link GpIdSupport}
   * to generate the new id.
   */
  public void updateGpId(GpIdSupport s) {
    gpidSupport = s;
    updateGpId();
  }

  public void setGpidSupport(GpIdSupport s) {
    gpidSupport = s;
  }

  /**
   * Allocate a new gpid to this PieceSlot, plus to any PlaceMarker or
   * Replace traits.
   */
  public void updateGpId() {
    gpId = gpidSupport.generateGpId();
    final GamePiece piece = getPiece();
    updateGpId(piece);
    setPiece(piece);
  }

  /**
   * Allocate new gpids in the given GamePiece
   *
   * @param piece GamePiece
   */
  public void updateGpId(GamePiece piece) {
    if (piece == null || piece instanceof BasicPiece) {
      return;
    }
    if (piece instanceof PlaceMarker) {
      ((PlaceMarker) piece).setGpId(gpidSupport.generateGpId());
    }
    updateGpId(((Decorator) piece).getInner());
  }

  public String getGpId() {
    return gpId;
  }

  public void setGpId(String id) {
    gpId = id;
  }

  private static class MyConfigurer extends Configurer {
    private final PieceDefiner definer;

    public MyConfigurer(PieceSlot slot) {
      super(null, slot.getConfigureName(), slot);
      definer = new PieceDefiner(slot.getGpId(), slot.gpidSupport);
      definer.setPiece(slot.getPiece());
    }

    @Override
    public String getValueString() {
      return null;
    }

    @Override
    public void setValue(String s) {
      throw new UnsupportedOperationException("Cannot set from String");
    }

    @Override
    public Object getValue() {
      final PieceSlot slot = (PieceSlot) super.getValue();
      if (slot != null) {
        slot.setPiece(definer.getPiece());
      }
      return slot;
    }

    @Override
    public Component getControls() {
      return definer;
    }
  }

}
