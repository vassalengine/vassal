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
 */
package VASSAL.build.widget;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdSupport;
import VASSAL.build.Widget;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.build.module.documentation.HelpWindowExtension;
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
import VASSAL.tools.swing.SwingUtils;

/**
 * A Component that displays a GamePiece.
 *
 * Can be added to any Widget but cannot contain any children Keyboard input on
 * a PieceSlot is forwarded to the {@link GamePiece#keyEvent} method for the
 * PieceSlot's GamePiece. Clicking on a PieceSlot initiates a drag
 */
public class PieceSlot extends Widget implements MouseListener, KeyListener {
  public static final String GP_ID = "gpid";
  protected GamePiece c;
  protected GamePiece expanded;
  protected String name;
  protected String pieceDefinition;
  protected static Font FONT = new Font("Dialog", 0, 12);
  protected JPanel panel;
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

  protected void copyFrom(PieceSlot piece) {
    c = piece.c;
    name = piece.name;
    pieceDefinition = piece.pieceDefinition;
    gpidSupport = piece.gpidSupport;
    gpId = piece.gpId;
  }

  public class Panel extends JPanel {
    private static final long serialVersionUID = 1L;
    protected PieceSlot pieceSlot;

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
    pieceDefinition = c == null ? null :
      GameModule.getGameModule().encode(new AddPiece(c));
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
      final AddPiece comm =
        (AddPiece) GameModule.getGameModule().decode(pieceDefinition);
      if (comm == null) {
        System.err.println("Couldn't build piece " + pieceDefinition);
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

  public void paint(Graphics g) {
    final Graphics2D g2d = (Graphics2D) g;
    final double os_scale = g2d.getDeviceConfiguration().getDefaultTransform().getScaleX();
    final AffineTransform orig_t = g2d.getTransform();
    g2d.setTransform(SwingUtils.descaleTransform(orig_t));

    final Dimension size = panel.getSize();
    size.width *= os_scale;
    size.height *= os_scale;

    final Color c = g.getColor();
    g.setColor(Color.WHITE);
    g.fillRect(0, 0, size.width, size.height);
    g.setColor(c);

    if (getExpandedPiece() == null) {
      g2d.addRenderingHints(SwingUtils.FONT_HINTS);

      final FontMetrics fm = g.getFontMetrics();
      g.drawRect(0, 0, size.width - 1, size.height - 1);
      g.setFont(FONT.deriveFont((float)(FONT.getSize() * os_scale)));
      g.drawString(" nil ",
        size.width/2 - fm.stringWidth(" nil ")/2,
        size.height/2
      );
    }
    else {
      getExpandedPiece().draw(g, size.width / 2, size.height / 2, panel, os_scale);

      // NB: The piece, not the expanded piece, receives events, so we check
      // the piece, not the expanded piece, for its selection status.
      if (Boolean.TRUE.equals(getPiece().getProperty(Properties.SELECTED))) {
        BasicPiece.getHighlighter().draw(getExpandedPiece(), g,
          size.width / 2, size.height / 2, panel, os_scale);
      }
    }

    g2d.setTransform(orig_t);
  }

  public Dimension getPreferredSize() {
    if (c != null && panel.getGraphics() != null) {
//      c.draw(panel.getGraphics(), 0, 0, panel, 1.0);
      return c.boundingBox().getSize();
    }
    else {
      return new Dimension(width, height);
    }
  }

  // Puts counter in DragBuffer. Call when mouse gesture recognized
  protected void startDrag() {
    // Recenter piece; panel may have been resized at some point resulting
    // in pieces with inaccurate positional information.
    final Dimension size = panel.getSize();
    getPiece().setPosition(new Point(size.width / 2, size.height / 2));

    // Erase selection border to avoid leaving selected after mouse dragged out
    getPiece().setProperty(Properties.SELECTED, null);
    panel.repaint();

    if (getPiece() != null) {
      KeyBuffer.getBuffer().clear();
      DragBuffer.getBuffer().clear();
      GamePiece newPiece = PieceCloner.getInstance().clonePiece(getPiece());
      newPiece.setProperty(Properties.PIECE_ID, getGpId());
      DragBuffer.getBuffer().add(newPiece);
    }
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
    else if (SwingUtils.isLeftMouseButton(e)) {
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
    else if (SwingUtils.isLeftMouseButton(e)) {
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
    KeyBuffer.getBuffer().keyCommand(KeyStroke.getKeyStrokeForEvent(e));
    e.consume();
    clearExpandedPiece();
    panel.repaint();
  }

  @Override
  public void keyTyped(KeyEvent e) {
    KeyBuffer.getBuffer().keyCommand(KeyStroke.getKeyStrokeForEvent(e));
    e.consume();
    clearExpandedPiece();
    panel.repaint();
  }

  @Override
  public void keyReleased(KeyEvent e) {
    KeyBuffer.getBuffer().keyCommand(KeyStroke.getKeyStrokeForEvent(e));
    e.consume();
    clearExpandedPiece();
    panel.repaint();
  }

  public static String getConfigureTypeName() {
    return "Single piece";
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
  public void build(org.w3c.dom.Element e) {
    gpidSupport = GameModule.getGameModule().getGpIdSupport();
    if (e != null) {
      name = e.getAttribute(NAME);
      gpId = e.getAttribute(GP_ID) + "";
      if (name.length() == 0) {
        name = null;
      }
      try {
        width = Integer.parseInt(e.getAttribute(WIDTH));
        height = Integer.parseInt(e.getAttribute(HEIGHT));
      }
      catch (NumberFormatException ex) {
        // Use default values.  Will be overwritten when module is saved
        width = 60;
        height = 60;
      }
      pieceDefinition = Builder.getText(e);
      c = null;
    }
  }

  @Override
  public void addTo(Buildable parent) {
    panel.setDropTarget(AbstractDragHandler.makeDropTarget(panel, DnDConstants.ACTION_MOVE, null));

    DragGestureListener dragGestureListener = new DragGestureListener() {
      @Override
      public void dragGestureRecognized(DragGestureEvent dge) {
        if (SwingUtils.isDragTrigger(dge)) {
          startDrag();
          AbstractDragHandler.getTheDragHandler().dragGestureRecognized(dge);
        }
      }
    };

    DragSource.getDefaultDragSource().createDefaultDragGestureRecognizer(
      panel, DnDConstants.ACTION_MOVE, dragGestureListener
    );
  }

  @Override
  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    final org.w3c.dom.Element el = doc.createElement(getClass().getName());
    final String s = getConfigureName();
    if (s != null) {
      el.setAttribute(NAME, s);
    }
    el.setAttribute(GP_ID, gpId+"");
    el.setAttribute(WIDTH, getPreferredSize().width + "");
    el.setAttribute(HEIGHT, getPreferredSize().height + "");

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
    return HelpFile.getReferenceManualPage("GamePiece.htm");
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

  /**
   * Allocate a new gpid to this PieceSlot, plus to any PlaceMarker or
   * Replace traits.
   */
  public void updateGpId() {
    gpId = gpidSupport.generateGpId();
    GamePiece piece = getPiece();
    updateGpId(piece);
    setPiece(piece);
  }

  /**
   * Allocate new gpid's in the given GamePiece
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

  private static class MyConfigurer extends Configurer implements HelpWindowExtension {
    private PieceDefiner definer;

    public MyConfigurer(PieceSlot slot) {
      super(null, slot.getConfigureName(), slot);
      definer = new PieceDefiner(slot.getGpId(), slot.gpidSupport);
      definer.setPiece(slot.getPiece());
    }

    @Override
    @Deprecated
    public void setBaseWindow(HelpWindow w) {
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
      PieceSlot slot = (PieceSlot) super.getValue();
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
