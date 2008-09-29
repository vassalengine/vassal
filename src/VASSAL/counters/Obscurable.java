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
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.event.InputEvent;
import java.awt.geom.Area;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import VASSAL.build.GameModule;
import VASSAL.build.module.ObscurableOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.PieceAccessConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.SequenceEncoder;

public class Obscurable extends Decorator implements TranslatablePiece {
  public static final String ID = "obs;";
  protected static final char INSET = 'I';
  protected static final char BACKGROUND = 'B';
  protected static final char PEEK = 'P';
  protected static final char IMAGE = 'G';

  protected char obscureKey;
  protected KeyStroke keyCommand;
  protected KeyStroke peekKey;
  protected String imageName;
  protected String obscuredToOthersImage;
  protected String obscuredBy;
  protected String hideCommand = "Mask";
  protected String peekCommand = "Peek";
  protected GamePiece obscuredToMeView;
  protected GamePiece obscuredToOthersView;
  protected boolean peeking;
  protected char displayStyle = INSET; // I = inset, B = background
  protected String maskName = "?";
  protected PieceAccess access = PlayerAccess.getInstance();

  protected KeyCommand[] commandsWithPeek;
  protected KeyCommand[] commandsWithoutPeek;
  protected KeyCommand hide;
  protected KeyCommand peek;

  public Obscurable() {
    this(ID + "M;", null);
  }

  public Obscurable(String type, GamePiece d) {
    mySetType(type);
    setInner(d);
  }

  public void mySetState(String in) {
    obscuredBy = "null".equals(in) ? null : in;
  }

  public void mySetType(String in) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(in, ';');
    st.nextToken();
    keyCommand = st.nextKeyStroke(null);
    imageName = st.nextToken();
    obscuredToMeView = GameModule.getGameModule().createPiece(BasicPiece.ID + ";;" + imageName + ";;");
    hideCommand = st.nextToken(hideCommand);
    if (st.hasMoreTokens()) {
      String s = st.nextToken(String.valueOf(INSET));
      displayStyle = s.charAt(0);
      switch (displayStyle) {
        case PEEK:
          if (s.length() > 1) {
            if (s.length() == 2) {
              peekKey = KeyStroke.getKeyStroke(s.charAt(1),InputEvent.CTRL_MASK);
            }
            else {
              peekKey = HotKeyConfigurer.decode(s.substring(1));
            }
            peeking = false;
          }
          else {
            peekKey = null;
            peeking = true;
          }
          break;
        case IMAGE:
          if (s.length() > 1) {
            obscuredToOthersImage = s.substring(1);
            obscuredToOthersView = GameModule.getGameModule().createPiece(BasicPiece.ID + ";;" + obscuredToOthersImage + ";;");
            obscuredToMeView.setPosition(new Point());
          }
      }
    }
    maskName = st.nextToken(maskName);
    access = PieceAccessConfigurer.decode(st.nextToken(null));
    commandsWithPeek = null;
    hide = null;
    peek = null;
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(keyCommand).append(imageName).append(hideCommand);
    switch (displayStyle) {
      case PEEK:
        if (peekKey == null) {
          se.append(displayStyle);
        }
        else {
          se.append(displayStyle + HotKeyConfigurer.encode(peekKey));
        }
        break;
      case IMAGE:
        se.append(displayStyle + obscuredToOthersImage);
        break;
      default:
        se.append(displayStyle);
    }
    se.append(maskName);
    se.append(PieceAccessConfigurer.encode(access));
    return ID + se.getValue();
  }

  public String myGetState() {
    return obscuredBy == null ? "null" : obscuredBy;
  }

  public Rectangle boundingBox() {
    if (obscuredToMe()) {
      return bBoxObscuredToMe();
    }
    else if (obscuredToOthers()) {
      return bBoxObscuredToOthers();
    }
    else {
      return piece.boundingBox();
    }
  }

  public Shape getShape() {
    if (obscuredToMe()) {
      return obscuredToMeView.getShape();
    }
    else if (obscuredToOthers()) {
      switch (displayStyle) {
        case BACKGROUND:
          return obscuredToMeView.getShape();
        case INSET:
          return piece.getShape();
        case PEEK:
          if (peeking && Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
            return piece.getShape();
          }
          else {
            return obscuredToMeView.getShape();
          }
        case IMAGE:
          Area area = new Area(obscuredToOthersView.getShape());
          area.add(new Area(piece.getShape()));
          return area;
      }
      return piece.getShape();
    }
    else {
      return piece.getShape();
    }
  }

  public boolean obscuredToMe() {
    return !access.currentPlayerHasAccess(obscuredBy);
  }

  public boolean obscuredToOthers() {
    return obscuredBy != null;
  }

  public void setProperty(Object key, Object val) {
    if (ID.equals(key)) {
      if (val instanceof String
          || val == null) {
        obscuredBy = (String) val;
        if ("null".equals(obscuredBy)) {
          obscuredBy = null;
        }
      }
    }
    else if (Properties.SELECTED.equals(key)) {
      if (!Boolean.TRUE.equals(val) && peekKey != null) {
        peeking = false;
      }
      super.setProperty(key, val);
    }
    else if (Properties.OBSCURED_TO_OTHERS.equals(key)) {
      String owner = null;
      if (Boolean.TRUE.equals(val)) {
        owner = access.getCurrentPlayerId();
      }
      obscuredBy = owner;
    }
    else {
      super.setProperty(key, val);
    }
  }

  public Object getProperty(Object key) {
    if (Properties.OBSCURED_TO_ME.equals(key)) {
      return Boolean.valueOf(obscuredToMe());
    }
    else if (Properties.OBSCURED_TO_OTHERS.equals(key)) {
      return Boolean.valueOf(obscuredToOthers());
    }
    else if (ID.equals(key)) {
      return obscuredBy;
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return myGetState()+isPeeking()+piece.getProperty(key);
    }
    // FIXME: Access to Obscured properties 
    // If piece is obscured to me, then mask any properties returned by
    // traits between this one and the innermost BasicPiece. Return directly
    // any properties normally handled by Decorator.getproperty()
    // Global Key Commands acting on Decks over-ride the masking by calling
    // setExposeMaskedProperties()
//    else if (obscuredToMe() && ! exposeMaskedProperties) {
//      if (Properties.KEY_COMMANDS.equals(key)) {
//        return getKeyCommands();
//      }
//      else if (Properties.INNER.equals(key)) {
//        return piece;
//      }
//      else if (Properties.OUTER.equals(key)) {
//        return getOuter();
//      }
//      else if (Properties.VISIBLE_STATE.equals(key)) {
//        return myGetState();
//      }
//      else {
//        return ((BasicPiece) Decorator.getInnermost(this)).getPublicProperty(key);
//      }
//    }
    else {
      return super.getProperty(key);
    }
  }

  public Object getLocalizedProperty(Object key) {
    if (obscuredToMe()) {
      return ((BasicPiece) Decorator.getInnermost(this)).getLocalizedPublicProperty(key);
    }
    else if (Properties.OBSCURED_TO_ME.equals(key)) {
    	return Boolean.valueOf(obscuredToMe());
    }
    else if (Properties.OBSCURED_TO_OTHERS.equals(key)) {
    	return Boolean.valueOf(obscuredToOthers());
    }
    else if (ID.equals(key)) {
    	return obscuredBy;
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
    	return myGetState()+isPeeking()+piece.getProperty(key);
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }
  
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    if (obscuredToMe()) {
      drawObscuredToMe(g, x, y, obs, zoom);
    }
    else if (obscuredToOthers()) {
      drawObscuredToOthers(g, x, y, obs, zoom);
    }
    else {
      piece.draw(g, x, y, obs, zoom);
    }
  }

  protected void drawObscuredToMe(Graphics g, int x, int y, Component obs, double zoom) {
    obscuredToMeView.draw(g, x, y, obs, zoom);
  }

  protected void drawObscuredToOthers(Graphics g, int x, int y, Component obs, double zoom) {
    switch (displayStyle) {
      case BACKGROUND:
        obscuredToMeView.draw(g, x, y, obs, zoom);
        piece.draw(g, x, y, obs, zoom * .5);
        break;
      case INSET:
        piece.draw(g, x, y, obs, zoom);
        Rectangle bounds = piece.getShape().getBounds();
        Rectangle obsBounds = obscuredToMeView.getShape().getBounds();
        obscuredToMeView.draw(g, x - (int) (zoom * bounds.width / 2
                                            - .5 * zoom * obsBounds.width / 2),
                              y - (int) (zoom * bounds.height / 2
                                         - .5 * zoom * obsBounds.height / 2),
                              obs, zoom * 0.5);
        break;
      case PEEK:
        if (peeking && Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
          piece.draw(g, x, y, obs, zoom);
        }
        else {
          obscuredToMeView.draw(g, x, y, obs, zoom);
        }
        break;
      case IMAGE:
        piece.draw(g, x, y, obs, zoom);
        obscuredToOthersView.draw(g, x, y, obs, zoom);
    }
  }

  /** Return true if the piece is currently being "peeked at" */
  public boolean isPeeking() {
    return peeking;
  }

  protected Rectangle bBoxObscuredToMe() {
    return obscuredToMeView.boundingBox();
  }

  protected Rectangle bBoxObscuredToOthers() {
    final Rectangle r;
    switch (displayStyle) {
      case BACKGROUND:
        r = bBoxObscuredToMe();
        break;
      case IMAGE:
        r = piece.boundingBox();
        r.add(obscuredToOthersView.boundingBox());
        break;
      default:
        r = piece.boundingBox();
    }
    return r;
  }

  public String getLocalizedName() {
    String maskedName = maskName == null ? "?" : maskName;
    maskedName = getTranslation(maskedName);
    return getName(maskedName, true);
  }
  
  public String getName() {
    String maskedName = maskName == null ? "?" : maskName;
    return getName(maskedName, false);
  }
  
  protected String getName(String maskedName, boolean localized) {
    if (obscuredToMe()) {
      return maskedName;
    }
    else if (obscuredToOthers()) {
      return (localized ? piece.getLocalizedName() : piece.getName()) + "(" + maskedName + ")";
    }
    else {
      return (localized ? piece.getLocalizedName() : piece.getName());
    }
  }

  public KeyCommand[] getKeyCommands() {
    if (obscuredToMe()) {
      KeyCommand myC[] = myGetKeyCommands();
      KeyCommand c[] = (KeyCommand[]) Decorator.getInnermost(this).getProperty(Properties.KEY_COMMANDS);
      if (c == null) {
        return myC;
      }
      else {
        KeyCommand all[] = new KeyCommand[c.length + myC.length];
        System.arraycopy(myC, 0, all, 0, myC.length);
        System.arraycopy(c, 0, all, myC.length, c.length);
        return all;
      }
    }
    else {
      return super.getKeyCommands();
    }
  }

  public KeyCommand[] myGetKeyCommands() {
    ArrayList<KeyCommand> l = new ArrayList<KeyCommand>();
    GamePiece outer = Decorator.getOutermost(this);
      
    // Hide Command
    if (keyCommand == null) { // Backwards compatibility with VASL classes
      keyCommand = KeyStroke.getKeyStroke(obscureKey, InputEvent.CTRL_MASK);
    }
      
    hide = new KeyCommand(hideCommand, keyCommand, outer, this);
    if (hideCommand.length() > 0 && isMaskable()) {
      l.add(hide);
      commandsWithoutPeek = new KeyCommand[] {hide};        
    }
    else {
      commandsWithoutPeek = new KeyCommand[0];
    }
      
    // Peek Command
    peek = new KeyCommand(peekCommand, peekKey, outer, this);
    if (displayStyle == PEEK && peekKey != null) {
      l.add(peek);
    }
      
    commandsWithPeek = l.toArray(new KeyCommand[l.size()]);

    return obscuredToOthers() &&
           isMaskable() &&
           displayStyle == PEEK &&
           peekKey != null ?
           commandsWithPeek : commandsWithoutPeek;
  }

  /** 
   * Return true if this piece can be masked/unmasked by the current player 
   * @param id ignored
   * @deprecated 
   */
  @Deprecated public boolean isMaskableBy(String id) {
    return isMaskable();
  }

  /** 
   * Return true if this piece can be masked/unmasked by the current player 
   */
  public boolean isMaskable() {
    return access.currentPlayerCanModify(obscuredBy)
    || ObscurableOptions.getInstance().isUnmaskable(obscuredBy);
  }
  
  public Command keyEvent(KeyStroke stroke) {
    if (!obscuredToMe()) {
      return super.keyEvent(stroke);
    }
    else if (isMaskable()){
      return myKeyEvent(stroke);
    }
    else {
      return null;
    }
  }

  public Command myKeyEvent(KeyStroke stroke) {
    Command retVal = null;
    myGetKeyCommands();
    if (hide.matches(stroke)) {
      ChangeTracker c = new ChangeTracker(this);
      if (obscuredToOthers()
          || obscuredToMe()) {
        obscuredBy = null;
      }
      else if (!obscuredToMe()) {
        obscuredBy = access.getCurrentPlayerId();
      }
      retVal = c.getChangeCommand();
    }
    else if (peek.matches(stroke)) {
      if (obscuredToOthers() && Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
        peeking = true;
      }
    }
    // For the "peek" display style with no key command (i.e. appears face-up whenever selected)
    // It looks funny if we turn something face down but we can still see it.
    // Therefore, un-select the piece if turning it face down
    if (retVal != null
      && PEEK == displayStyle
      && peekKey == null
      && obscuredToOthers()) {
      Runnable runnable = new Runnable() {
        public void run() {
          KeyBuffer.getBuffer().remove(Decorator.getOutermost(Obscurable.this));
        }
      };
      SwingUtilities.invokeLater(runnable);
    }
    return retVal;
  }

  public String getDescription() {
    return "Mask";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Mask.htm");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * If true, then all masked pieces are considered masked to all players.
   * Used to temporarily draw pieces as they appear to other players
   * @param allHidden
   * @deprecated
   */
  @Deprecated public static void setAllHidden(boolean allHidden) {
    if (allHidden) {
      PieceAccess.GlobalAccess.hideAll();
    }
    else {
      PieceAccess.GlobalAccess.revertAll();
    }
  }

  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {hideCommand, maskName, peekCommand}, new String[] {"Mask command", "Name when masked", "Peek command"});
  }
  
  private static class Ed implements PieceEditor {
    private ImagePicker picker;
    private HotKeyConfigurer obscureKeyInput;
    private StringConfigurer obscureCommandInput, maskNameInput;
    private StringEnumConfigurer displayOption;
    private HotKeyConfigurer peekKeyInput;
    private JPanel controls = new JPanel();
    private String[] optionNames = new String[]{"Background", "Plain", "Inset", "Use Image"};
    private char[] optionChars = new char[]{BACKGROUND, PEEK, INSET, IMAGE};
    private ImagePicker imagePicker;
    private PieceAccessConfigurer accessConfig;

    public Ed(Obscurable p) {
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      obscureCommandInput = new StringConfigurer(null, "Mask Command:  ", p.hideCommand);
      box.add(obscureCommandInput.getControls());
      obscureKeyInput = new HotKeyConfigurer(null,"  Keyboard Command:  ",p.keyCommand);
      box.add(obscureKeyInput.getControls());
      controls.add(box);
      
      accessConfig = new PieceAccessConfigurer(null,"Can be masked by:  ",p.access);
      controls.add(accessConfig.getControls());

      box = Box.createHorizontalBox();
      box.add(new JLabel("View when masked: "));
      picker = new ImagePicker();
      picker.setImageName(p.imageName);
      box.add(picker);
      controls.add(box);

      box = Box.createHorizontalBox();
      maskNameInput = new StringConfigurer(null, "Name when masked:  ", p.maskName);
      box.add(maskNameInput.getControls());
      controls.add(box);

      box = Box.createHorizontalBox();
      displayOption = new StringEnumConfigurer(null, "Display style:  ", optionNames);
      for (int i = 0; i < optionNames.length; ++i) {
        if (p.displayStyle == optionChars[i]) {
          displayOption.setValue(optionNames[i]);
          break;
        }
      }
      box.add(displayOption.getControls());

      final JPanel showDisplayOption = new JPanel() {
        private static final long serialVersionUID = 1L;

        public Dimension getMinimumSize() {
          return new Dimension(60, 60);
        }

        public Dimension getPreferredSize() {
          return new Dimension(60, 60);
        }

        public void paint(Graphics g) {
          g.clearRect(0, 0, getWidth(), getHeight());
          switch (displayOption.getValueString().charAt(0)) {
            case BACKGROUND:
              g.setColor(Color.black);
              g.fillRect(0, 0, 60, 60);
              g.setColor(Color.white);
              g.fillRect(15, 15, 30, 30);
              break;
            case INSET:
              g.setColor(Color.white);
              g.fillRect(0, 0, 60, 60);
              g.setColor(Color.black);
              g.fillRect(0, 0, 30, 30);
              break;
            case PEEK:
              g.setColor(Color.black);
              g.fillRect(0, 0, 60, 60);
              break;
          }
        }
      };

      box.add(showDisplayOption);
      controls.add(box);

      peekKeyInput = new HotKeyConfigurer(null,"Peek Command:  ",p.peekKey);
      peekKeyInput.getControls().setVisible(p.displayStyle == PEEK);
      controls.add(peekKeyInput.getControls());

      imagePicker = new ImagePicker();
      imagePicker.setImageName(p.obscuredToOthersImage);
      imagePicker.setVisible(p.displayStyle == IMAGE);
      controls.add(imagePicker);

      displayOption.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          showDisplayOption.repaint();
          peekKeyInput.getControls().setVisible(optionNames[1].equals(evt.getNewValue()));
          imagePicker.setVisible(optionNames[3].equals(evt.getNewValue()));
          Window w = SwingUtilities.getWindowAncestor(controls);
          if (w != null) {
            w.pack();
          }
        }
      });
    }

    public String getState() {
      return "null";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append((KeyStroke)obscureKeyInput.getValue())
          .append(picker.getImageName())
          .append(obscureCommandInput.getValueString());
      char optionChar = INSET;
      for (int i = 0; i < optionNames.length; ++i) {
        if (optionNames[i].equals(displayOption.getValueString())) {
          optionChar = optionChars[i];
          break;
        }
      }
      switch (optionChar) {
        case PEEK:
          String valueString = peekKeyInput.getValueString();
          if (valueString != null) {
            se.append(optionChar + valueString);
          }
          else {
            se.append(optionChar);
          }
          break;
        case IMAGE:
          se.append(optionChar + imagePicker.getImageName());
          break;
        default:
          se.append(optionChar);
      }
      se.append(maskNameInput.getValueString());
      se.append(accessConfig.getValueString());
      return ID + se.getValue();
    }

    public Component getControls() {
      return controls;
    }
  }
}
