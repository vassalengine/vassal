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

import VASSAL.build.GameModule;
import VASSAL.build.module.ObscurableOptions;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.ImageSelector;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.PieceAccessConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.SequenceEncoder;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.awt.geom.Area;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.ArrayUtils;

public class Obscurable extends Decorator implements TranslatablePiece {
  public static final String ID = "obs;"; //$NON-NLS-1$//
  protected static final char INSET = 'I';
  protected static final char BACKGROUND = 'B';
  protected static final char PEEK = 'P';
  protected static final char IMAGE = 'G';
  protected static final String DEFAULT_PEEK_COMMAND = Resources.getString("Editor.Obscurable.default_peek_command");

  protected char obscureKey;
  protected NamedKeyStroke keyCommand;
  protected NamedKeyStroke peekKey;
  protected String imageName;
  protected String obscuredToOthersImage;
  protected String obscuredBy;
  protected ObscurableOptions obscuredOptions;
  protected String hideCommand = Resources.getString("Editor.Obscurable.default_mask_command");
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
  protected String description = "";

  public Obscurable() {
    this(ID + "M;", null); //$NON-NLS-1$//
  }

  public Obscurable(String type, GamePiece d) {
    mySetType(type);
    setInner(d);
  }

  @Override
  public void mySetState(String in) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(in, ';');
    String token = sd.nextToken("null"); //$NON-NLS-1$//
    obscuredBy = "null".equals(token) ? null : token; //$NON-NLS-1$//
    token = sd.nextToken("");
    obscuredOptions = (obscuredBy == null ? null : new ObscurableOptions(token));
  }

  @Override
  public void mySetType(String in) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(in, ';');
    st.nextToken();
    keyCommand = st.nextNamedKeyStroke(null);
    imageName = st.nextToken();
    obscuredToMeView = GameModule.getGameModule().createPiece(BasicPiece.ID + ";;" + imageName + ";;");
    hideCommand = st.nextToken(hideCommand);
    if (st.hasMoreTokens()) {
      final String s = st.nextToken(String.valueOf(INSET));
      displayStyle = s.charAt(0);
      switch (displayStyle) {
      case PEEK:
        if (s.length() > 1) {
          if (s.length() == 2) {
            peekKey = NamedKeyStroke.of(s.charAt(1), InputEvent.CTRL_DOWN_MASK);
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
          obscuredToOthersImage = s.substring(1).intern();
          obscuredToOthersView = GameModule.getGameModule().createPiece(BasicPiece.ID + ";;" + obscuredToOthersImage + ";;");
          obscuredToMeView.setPosition(new Point());
        }
      }
    }
    maskName = st.nextToken(maskName);
    access = PieceAccessConfigurer.decode(st.nextToken(null));
    peekCommand = st.nextToken(DEFAULT_PEEK_COMMAND);
    description = st.nextToken("");
    commandsWithPeek = null;
    hide = null;
    peek = null;
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
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
    se.append(description);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(obscuredBy == null ? "null" : obscuredBy); //$NON-NLS-1$//
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
      if (val instanceof String || val == null) {
        obscuredBy = (String) val;
        if ("null".equals(obscuredBy)) { //$NON-NLS-1$//
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
      return myGetState() + isPeeking() + getProperty(Properties.SELECTED) + piece.getProperty(key);
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
      return myGetState() + isPeeking() + piece.getProperty(key);
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
      final Rectangle bounds = piece.getShape().getBounds();
      final Rectangle obsBounds = obscuredToMeView.getShape().getBounds();
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
    final String maskedName = maskName == null ? "?" : maskName;
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
    final ArrayList<KeyCommand> l = new ArrayList<>();
    final GamePiece outer = Decorator.getOutermost(this);

    // Hide Command
    if (keyCommand == null) { // Backwards compatibility with VASL classes
      keyCommand = NamedKeyStroke.of(obscureKey, InputEvent.CTRL_DOWN_MASK);
    }

    hide = new KeyCommand(hideCommand, keyCommand, outer, this);
    if (hideCommand.length() > 0 && isMaskable()) {
      l.add(hide);
      commandsWithoutPeek = new KeyCommand[] {hide};
    }
    else {
      commandsWithoutPeek = KeyCommand.NONE;
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
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public boolean isMaskableBy(@SuppressWarnings("unused") String id) {
    ProblemDialog.showDeprecated("2020-08-06");
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
    else if (isMaskable()) {
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
      final Runnable runnable = () -> KeyBuffer.getBuffer().remove(Decorator.getOutermost(this));
      SwingUtilities.invokeLater(runnable);
    }
    return retVal;
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.Obscurable.trait_description", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Mask.html"); //$NON-NLS-1$//
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * If true, then all masked pieces are considered masked to all players.
   * Used to temporarily draw pieces as they appear to other players
   * @param allHidden True to hide all pieces
   * @deprecated
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static void setAllHidden(boolean allHidden) {
    ProblemDialog.showDeprecated("2020-08-06");
    if (allHidden) {
      PieceAccess.GlobalAccess.hideAll();
    }
    else {
      PieceAccess.GlobalAccess.revertAll();
    }
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(new String[] {hideCommand, maskName, peekCommand},
      new String[] {
        Resources.getString("Editor.Obscurable.mask_command"),
        Resources.getString("Editor.Obscurable.name_when_masked"),
        Resources.getString("Editor.Obscurable.peek_command")});
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

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof Obscurable)) return false;
    final Obscurable c = (Obscurable) o;
    if (! Objects.equals(keyCommand, c.keyCommand)) return false;
    if (! Objects.equals(imageName, c.imageName)) return false;
    if (! Objects.equals(hideCommand, c.hideCommand)) return false;
    if (! Objects.equals(displayStyle, c.displayStyle)) return false;
    switch (displayStyle) {
    case PEEK:
      if (!Objects.equals(peekKey, c.peekKey))
        return false;
      break;
    case IMAGE:
      if (!Objects.equals(obscuredToOthersImage, c.obscuredToOthersImage))
        return false;
      break;
    default:
      break;
    }
    if (! Objects.equals(maskName, c.maskName)) return false;
    if (! Objects.equals(PieceAccessConfigurer.encode(access), PieceAccessConfigurer.encode(c.access))) return false;
    if (! Objects.equals(peekCommand, c.peekCommand)) return false;

    if (! Objects.equals(obscuredBy, c.obscuredBy)) return false;
    final boolean noOptions = obscuredBy == null || obscuredOptions == null;
    final boolean noOptions2 = c.obscuredBy == null || c.obscuredOptions == null;
    if (! Objects.equals(noOptions, noOptions2)) return false;
    if (!noOptions && !noOptions2) {
      return Objects.equals(obscuredOptions.encodeOptions(), c.obscuredOptions.encodeOptions());
    }
    return true;
  }

  private static class Ed implements PieceEditor {
    private final ImageSelector picker;
    private final NamedHotKeyConfigurer obscureKeyInput;
    private final StringConfigurer obscureCommandInput, maskNameInput;
    private final TranslatingStringEnumConfigurer displayOption;
    private final NamedHotKeyConfigurer peekKeyInput;
    private final StringConfigurer peekCommandInput;
    private final TraitConfigPanel controls = new TraitConfigPanel();
    private final String[] optionNames = {"B", "P", "I", "U"}; // NON-NLS
    private final String[] optionKeys = {
      "Editor.Obscurable.background",
      "Editor.Obscurable.plain",
      "Editor.Obscurable.inset",
      "Editor.Obscurable.use_image"
    };
    private final char[] optionChars = {BACKGROUND, PEEK, INSET, IMAGE};
    private final ImageSelector imagePicker;
    private final PieceAccessConfigurer accessConfig;
    private final JPanel showDisplayOption;
    private final JLabel peekKeyLabel;
    private final JLabel peekCommandLabel;
    private final StringConfigurer descInput;

    public Ed(Obscurable p) {

      descInput = new StringConfigurer(p.description);
      descInput.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descInput);

      obscureCommandInput = new StringConfigurer(p.hideCommand);
      obscureCommandInput.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.menu_command", obscureCommandInput);

      obscureKeyInput = new NamedHotKeyConfigurer(p.keyCommand);
      controls.add("Editor.keyboard_command", obscureKeyInput);

      accessConfig = new PieceAccessConfigurer(p.access);
      controls.add("Editor.Obscurable.can_be_masked_by", accessConfig);

      picker = new ImageSelector(p.imageName, 512, 512);
      controls.add("Editor.Obscurable.view_when_masked", picker);

      maskNameInput = new StringConfigurer(p.maskName);
      maskNameInput.setHintKey("Editor.Obscurable.name_when_masked_hint");
      controls.add("Editor.Obscurable.name_when_masked", maskNameInput);

      final JPanel displayPanel = new JPanel(new MigLayout("ins 0,hidemode 3", "[][][]")); // NON-NLS
      final JLabel displayLabel = new JLabel(Resources.getString("Editor.Obscurable.display_style"));
      displayLabel.setLabelFor(displayPanel);

      displayOption = new TranslatingStringEnumConfigurer(optionNames, optionKeys);
      for (int i = 0; i < optionNames.length; ++i) {
        if (p.displayStyle == optionChars[i]) {
          displayOption.setValue(optionNames[i]);
          break;
        }
      }
      controls.add(displayLabel);
      displayPanel.add(displayOption.getControls());

      showDisplayOption = new JPanel() {
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

      displayPanel.add(showDisplayOption);

      imagePicker = new ImageSelector(p.obscuredToOthersImage, 512, 512);
      imagePicker.getControls().setVisible(p.displayStyle == IMAGE);
      displayPanel.add(imagePicker.getControls());

      controls.add(displayPanel, "wrap"); // NON-NLS

      peekKeyLabel = new JLabel(Resources.getString("Editor.Obscurable.peek_keyboard_command"));
      peekKeyInput = new NamedHotKeyConfigurer(p.peekKey);
      peekKeyLabel.setVisible(p.displayStyle == PEEK);
      peekKeyInput.getControls().setVisible(p.displayStyle == PEEK);
      controls.add(peekKeyLabel, peekKeyInput);

      peekCommandLabel = new JLabel(Resources.getString("Editor.Obscurable.peek_menu_command"));
      peekCommandInput = new StringConfigurer(p.peekCommand);
      peekCommandInput.setHintKey("Editor.menu_command_hint");
      peekCommandLabel.setVisible(p.displayStyle == PEEK);
      peekCommandInput.getControls().setVisible(p.displayStyle == PEEK);
      controls.add(peekCommandLabel, peekCommandInput);

      displayOption.addPropertyChangeListener(evt -> {
        showDisplayOption.repaint();
        peekKeyLabel.setVisible(optionNames[1].equals(evt.getNewValue()));
        peekKeyInput.getControls().setVisible(optionNames[1].equals(evt.getNewValue()));
        peekCommandLabel.setVisible(optionNames[1].equals(evt.getNewValue()));
        peekCommandInput.getControls().setVisible(optionNames[1].equals(evt.getNewValue()));
        imagePicker.getControls().setVisible(optionNames[3].equals(evt.getNewValue()));
        showDisplayOption.setVisible(!optionNames[3].equals(evt.getNewValue()));
        repack(controls);
      });
    }

    @Override
    public String getState() {
      return "null"; //$NON-NLS-1$//
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(obscureKeyInput.getValueString())
        .append(picker.getValueString())
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
        final String valueString = peekKeyInput.getValueString();
        if (valueString != null) {
          se.append(optionChar + valueString);
        }
        else {
          se.append(optionChar);
        }
        break;
      case IMAGE:
        se.append(optionChar + imagePicker.getValueString());
        break;
      default:
        se.append(optionChar);
      }
      se.append(maskNameInput.getValueString());
      se.append(accessConfig.getValueString());
      se.append(peekCommandInput.getValueString());
      se.append(descInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public Component getControls() {
      return controls;
    }
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(keyCommand, peekKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(hideCommand, peekCommand);
  }

  /**
   * @return a list of any Message Format strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(maskName);
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (imageName != null) s.add(imageName);
    if (obscuredToOthersImage != null) s.add(obscuredToOthersImage);
  }
}
