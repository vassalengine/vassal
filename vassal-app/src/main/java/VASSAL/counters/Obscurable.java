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
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import org.apache.commons.lang3.ArrayUtils;

import VASSAL.build.GameModule;
import VASSAL.build.module.ObscurableOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PieceAccessConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

public class Obscurable extends Decorator implements TranslatablePiece {
  public static final String ID = "obs;";
  protected static final char INSET = 'I';
  protected static final char BACKGROUND = 'B';
  protected static final char PEEK = 'P';
  protected static final char IMAGE = 'G';
  protected static final String DEFAULT_PEEK_COMMAND = "Peek";

  protected char obscureKey;
  protected NamedKeyStroke keyCommand;
  protected NamedKeyStroke peekKey;
  protected String imageName;
  protected String obscuredToOthersImage;
  protected String obscuredBy;
  protected ObscurableOptions obscuredOptions;
  protected String hideCommand = "Mask";
  protected String peekCommand = DEFAULT_PEEK_COMMAND;
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

  @Override
  public void mySetState(String in) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(in, ';');
    String token = sd.nextToken("null");
    obscuredBy = "null".equals(token) ? null : token;
    token = sd.nextToken("");
    obscuredOptions = (obscuredBy == null ? null : new ObscurableOptions(token));
  }

  @Override
  public void mySetType(String in) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(in, ';');
    st.nextToken();
    keyCommand = st.nextNamedKeyStroke(null);
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
            peekKey = NamedKeyStroke.getNamedKeyStroke(s.charAt(1),InputEvent.CTRL_DOWN_MASK);
          }
          else {
            peekKey = NamedHotKeyConfigurer.decode(s.substring(1));
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
    peekCommand = st.nextToken(DEFAULT_PEEK_COMMAND);
    commandsWithPeek = null;
    hide = null;
    peek = null;
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(keyCommand).append(imageName).append(hideCommand);
    switch (displayStyle) {
    case PEEK:
      if (peekKey == null) {
        se.append(displayStyle);
      }
      else {
        se.append(displayStyle + NamedHotKeyConfigurer.encode(peekKey));
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
    se.append(peekCommand);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(obscuredBy == null ? "null" : obscuredBy);
    se.append((obscuredBy == null || obscuredOptions == null) ? "" : obscuredOptions.encodeOptions());
    return se.getValue();
  }

  @Override
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

  @Override
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
        final Area area = new Area(obscuredToOthersView.getShape());
        final Shape innerShape = piece.getShape();
        if (innerShape instanceof Area) {
          area.add((Area) innerShape);
        }
        else {
          area.add(new Area(innerShape));
        }
        return area;
      default:
        return piece.getShape();
      }
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

  @Override
  public void setProperty(Object key, Object val) {
    if (ID.equals(key)) {
      if (val instanceof String
          || val == null) {
        obscuredBy = (String) val;
        if ("null".equals(obscuredBy)) {
          obscuredBy = null;
          obscuredOptions = null;
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
      obscuredOptions = new ObscurableOptions(ObscurableOptions.getInstance().encodeOptions());
    }
    else {
      super.setProperty(key, val);
    }
  }

  @Override
  public Object getProperty(Object key) {
    if (Properties.OBSCURED_TO_ME.equals(key)) {
      return obscuredToMe();
    }
    else if (Properties.OBSCURED_TO_OTHERS.equals(key)) {
      return obscuredToOthers();
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

  @Override
  public Object getLocalizedProperty(Object key) {
    if (obscuredToMe()) {
      return ((BasicPiece) Decorator.getInnermost(this)).getLocalizedPublicProperty(key);
    }
    else if (Properties.OBSCURED_TO_ME.equals(key)) {
      return obscuredToMe();
    }
    else if (Properties.OBSCURED_TO_OTHERS.equals(key)) {
      return obscuredToOthers();
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

  @Override
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

  @Override
  public String getLocalizedName() {
    String maskedName = maskName == null ? "?" : maskName;
    maskedName = getTranslation(maskedName);
    return getName(maskedName, true);
  }

  @Override
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

  @Override
  public KeyCommand[] getKeyCommands() {
    if (obscuredToMe()) {
      final KeyCommand[] myC = myGetKeyCommands();
      final KeyCommand[] c = (KeyCommand[])
        Decorator.getInnermost(this).getProperty(Properties.KEY_COMMANDS);

      if (c == null) return myC;
      else return ArrayUtils.addAll(myC, c);
    }
    else {
      return super.getKeyCommands();
    }
  }

  @Override
  public KeyCommand[] myGetKeyCommands() {
    ArrayList<KeyCommand> l = new ArrayList<>();
    GamePiece outer = Decorator.getOutermost(this);

    // Hide Command
    if (keyCommand == null) { // Backwards compatibility with VASL classes
      keyCommand = NamedKeyStroke.getNamedKeyStroke(obscureKey, InputEvent.CTRL_DOWN_MASK);
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
    if (displayStyle == PEEK && peekKey != null && peekCommand.length() > 0) {
      l.add(peek);
    }

    commandsWithPeek = l.toArray(new KeyCommand[0]);

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
    // Check if piece is owned by us. Returns true if we own the piece, or if it
    // is not currently masked
    if (access.currentPlayerCanModify(obscuredBy)) {
      return true;
    }

    // Check ObscurableOptions in play when piece was Obscured
    if (obscuredOptions != null) {
      return obscuredOptions.isUnmaskable(obscuredBy);
    }

    // Fall-back, use current global ObscurableOptions
    return ObscurableOptions.getInstance().isUnmaskable(obscuredBy);
  }

  @Override
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

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    Command retVal = null;
    myGetKeyCommands();

    if (hide.matches(stroke)) {
      final ChangeTracker c = new ChangeTracker(this);
      if (obscuredToOthers() || obscuredToMe()) {
        obscuredBy = null;
        obscuredOptions = null;
      }
      else if (!obscuredToMe()) {
        obscuredBy = access.getCurrentPlayerId();
        obscuredOptions = new ObscurableOptions(ObscurableOptions.getInstance().encodeOptions());
      }

      retVal = c.getChangeCommand();
    }
    else if (peek.matches(stroke)) {
      if (obscuredToOthers() &&
          Boolean.TRUE.equals(getProperty(Properties.SELECTED))) {
        peeking = true;
      }
    }

    // For the "peek" display style with no key command (i.e. appears
    // face-up whenever selected).
    //
    // It looks funny if we turn something face down but we can still see it.
    // Therefore, un-select the piece if turning it face down
    if (retVal != null && PEEK == displayStyle &&
        peekKey == null && obscuredToOthers()) {
      // FIXME: This probably causes a race condition. Can we do this directly?
      Runnable runnable = new Runnable() {
        @Override
        public void run() {
          KeyBuffer.getBuffer().remove(Decorator.getOutermost(Obscurable.this));
        }
      };
      SwingUtilities.invokeLater(runnable);
    }
    return retVal;
  }

  @Override
  public String getDescription() {
    return "Mask";
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Mask.htm");
  }

  @Override
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

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {hideCommand, maskName, peekCommand}, new String[] {"Mask command", "Name when masked", "Peek command"});
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    final ArrayList<String> l = new ArrayList<>();
    l.add(Properties.OBSCURED_TO_OTHERS);
    l.add(Properties.OBSCURED_TO_ME);
    return l;
  }

  private static class Ed implements PieceEditor {
    private ImagePicker picker;
    private NamedHotKeyConfigurer obscureKeyInput;
    private StringConfigurer obscureCommandInput, maskNameInput;
    private StringEnumConfigurer displayOption;
    private NamedHotKeyConfigurer peekKeyInput;
    private StringConfigurer peekCommandInput;
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
      obscureKeyInput = new NamedHotKeyConfigurer(null,"  Keyboard Command:  ",p.keyCommand);
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

        @Override
        public Dimension getMinimumSize() {
          return new Dimension(60, 60);
        }

        @Override
        public Dimension getPreferredSize() {
          return new Dimension(60, 60);
        }

        @Override
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

      peekKeyInput = new NamedHotKeyConfigurer(null,"Peek Key:  ",p.peekKey);
      peekKeyInput.getControls().setVisible(p.displayStyle == PEEK);
      controls.add(peekKeyInput.getControls());

      peekCommandInput = new StringConfigurer(null, "Peek Command:  ", p.peekCommand);
      peekCommandInput.getControls().setVisible(p.displayStyle == PEEK);
      controls.add(peekCommandInput.getControls());

      imagePicker = new ImagePicker();
      imagePicker.setImageName(p.obscuredToOthersImage);
      imagePicker.setVisible(p.displayStyle == IMAGE);
      controls.add(imagePicker);

      displayOption.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          showDisplayOption.repaint();
          peekKeyInput.getControls().setVisible(optionNames[1].equals(evt.getNewValue()));
          peekCommandInput.getControls().setVisible(optionNames[1].equals(evt.getNewValue()));
          imagePicker.setVisible(optionNames[3].equals(evt.getNewValue()));
          Window w = SwingUtilities.getWindowAncestor(controls);
          if (w != null) {
            w.pack();
          }
        }
      });
    }

    @Override
    public String getState() {
      return "null";
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(obscureKeyInput.getValueString())
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
      se.append(peekCommandInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public Component getControls() {
      return controls;
    }
  }
}
