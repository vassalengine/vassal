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

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.DrawPile;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.DeckSelectionConfigurer;
import VASSAL.configure.DeckSelector;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;

/**
 * GamePiece trait that returns a piece to a {@link DrawPile}
 */
public class ReturnToDeck extends Decorator implements TranslatablePiece {
  public static final String ID = "return;"; // NON-NLS
  public static final int RtDversion = 2;
  protected String deckId;
  protected String returnCommand;
  protected String selectDeckPrompt = Resources.getString("Editor.ReturnToDeck.select_destination");
  protected NamedKeyStroke returnKey;
  protected DrawPile deck;

  protected KeyCommand[] commands;
  protected KeyCommand myCommand;

  protected String description = "";

  protected String deckName;

  protected boolean deckSelect = true;
  protected FormattedString deckExpression = new FormattedString("");

  public ReturnToDeck() {
    this(ID + Resources.getString("Editor.ReturnToDeck.default_command") + ";R;", null); // NON-NLS
  }

  public ReturnToDeck(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      myCommand = new KeyCommand(returnCommand, returnKey, Decorator.getOutermost(this), this);
      if (returnCommand.length() > 0 && returnKey != null && !returnKey.isNull()) {
        commands =
            new KeyCommand[]{myCommand};
      }
      else {
        commands = KeyCommand.NONE;
      }
    }
    return commands;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public void mySetType(String s) {
    if (s.startsWith(ID)) {
      s = s.substring(ID.length());
      final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
      returnCommand = st.nextToken();
      returnKey = st.nextNamedKeyStroke(null);
      deckId = st.nextToken("");
      selectDeckPrompt = st.nextToken(selectDeckPrompt);
      description = st.nextToken("");

      final int version = st.nextInt(0);
      if (version < 2) {
        updateDeckName();

        //BR// if loading an "old ID" format trait, create values needed for new format.
        deckSelect = (deckId == null || deckId.isEmpty());
        deckExpression.setFormat("");

        if (deckId != null) {
          final DrawPile p = DrawPile.findDrawPile(deckId);
          if (p != null) {
            final String name = p.getConfigureName();
            if (name != null) {
              deckExpression.setFormat(name);
            }
            else {
              deckExpression.setFormat(deckId);
            }
          }
        }
      }
      else {
        deckSelect = st.nextBoolean(true);
        deckExpression.setFormat(st.nextToken(""));
      }
    }
  }

  private static String getDeckName(String deckId) {
    String dn = "[" + Resources.getString("Editor.ReturnToDeck.none") + "]";
    if (deckId != null) {
      final DrawPile p = DrawPile.findDrawPile(deckId);
      if (p != null) {
        dn = p.getConfigureName();
      }
    }
    return dn != null ? dn.intern() : null;
  }

  private void updateDeckName() {
    deckName = getDeckName(deckId);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    return ID + se.append(returnCommand).append(returnKey).append(deckId).append(selectDeckPrompt).append(description).append(RtDversion).append(deckSelect).append(deckExpression.getFormat()).getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    Command comm = null;
    if (myCommand.matches(stroke)) {
      final DrawPile pile;

      if (deckSelect) {
        pile = promptForDrawPile();
      }
      else {
        final String evalName = deckExpression.getText(this);
        pile = DrawPile.findDrawPile(evalName);
        if (pile == null) {
          ErrorDialog.dataWarning(new BadDataReport("Deck Not Found for Return-to-Deck trait: " + evalName, deckExpression.getFormat())); //NON-NLS
        }
      }

      if (pile == null) {
        return null;
      }

      final Map preMap = getMap();
      final Point prePos = getPosition();
      comm = putOldProperties(this);
      comm = comm.append(pile.addToContents(Decorator.getOutermost(this)));
      // Apply Auto-move key if the piece has moved
      final Map m = pile.getMap();
      if (m != null && m.getMoveKey() != null && (m != preMap || !getPosition().equals(prePos))) {
        comm.append(Decorator.getOutermost(this).keyEvent(m.getMoveKey()));
      }
      if (m != null) {
        m.repaint();
      }
    }
    return comm;
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  private DrawPile promptForDrawPile() {
    final JDialog d = new JDialog(GameModule.getGameModule().getPlayerWindow(), true);

    d.setTitle(Decorator.getInnermost(this).getName()); //$NON-NLS-1$
    d.setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));

    class AvailableDeck {
      private final DrawPile pile;

      public AvailableDeck(DrawPile pile) {
        this.pile = pile;
      }

      @Override
      public String toString() {
        return pile.getConfigureName();
      }
    }

    final List<DrawPile> piles =
      GameModule.getGameModule().getAllDescendantComponentsOf(DrawPile.class);

    if (piles.isEmpty()) {
      reportDataError(this, "No decks in module."); // NON-NLS
      return null;
    }

    final AvailableDeck[] decks = new AvailableDeck[piles.size()];
    int i = 0;
    for (final DrawPile p : piles)
      decks[i++] = new AvailableDeck(p);

    final JList<AvailableDeck> list = new JList<>(decks);
    list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    final JLabel prompt = new JLabel(selectDeckPrompt);
    prompt.setAlignmentX(0.5f);
    d.add(prompt); //$NON-NLS-1$
    d.add(new ScrollPane(list));
    final Box box = Box.createHorizontalBox();
    box.setAlignmentX(0.5F);
    JButton b = new JButton(Resources.getString(Resources.OK));
    b.addActionListener(e -> {
      final AvailableDeck selection = list.getSelectedValue();
      if (selection != null)
        deck = selection.pile;
      d.dispose();
    });
    box.add(b);
    b = new JButton(Resources.getString(Resources.CANCEL));
    b.addActionListener(e -> d.dispose());
    box.add(b);
    d.add(box);
    d.pack();
    d.setLocationRelativeTo(d.getOwner());
    d.setVisible(true);
    // don't cache -- ask again next time
    final DrawPile pile = deck;
    deck = null;
    return pile;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.ReturnToDeck.trait_description", deckSelect ? "" : deckExpression.getFormat(), description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ReturnToDeck.html"); // NON-NLS
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(returnCommand, Resources.getString("Editor.ReturnToDeck.return_to_deck_command"));
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof ReturnToDeck)) return false;
    final ReturnToDeck c = (ReturnToDeck) o;
    if (! Objects.equals(returnCommand, c.returnCommand)) return false;
    if (! Objects.equals(returnKey, c.returnKey)) return false;
    if (! Objects.equals(deckExpression, c.deckExpression)) return false;
    if (! Objects.equals(deckSelect, c.deckSelect)) return false;
    if (! Objects.equals(deckId, c.deckId)) return false;
    return Objects.equals(selectDeckPrompt, c.selectDeckPrompt);
  }

  private static class Ed implements PieceEditor {
    private final StringConfigurer menuName;
    private final NamedHotKeyConfigurer menuKey;
    private final TraitConfigPanel controls;
    private final String deckId;
    private final JLabel promptLabel;
    private final StringConfigurer promptText;
    private final BooleanConfigurer prompt;
    private final JLabel selectLabel;
    private final StringConfigurer description;
    private final DeckSelectionConfigurer deckExp;


    public Ed(ReturnToDeck p) {
      controls = new TraitConfigPanel();
      deckId = p.deckId;

      description = new StringConfigurer(p.description);
      description.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", description);

      menuName = new StringConfigurer(p.returnCommand);
      menuName.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.menu_command", menuName);

      menuKey = new NamedHotKeyConfigurer(p.returnKey);
      controls.add("Editor.keyboard_command", menuKey);

      prompt = new BooleanConfigurer(p.deckSelect);
      prompt.addPropertyChangeListener(e -> updateVisibility());
      controls.add("Editor.ReturnToDeck.choose_destination_deck_at_game_time", prompt);

      selectLabel = new JLabel(Resources.getString("Editor.ReturnToDeck.deck_name"));
      deckExp = new DeckSelectionConfigurer(p.deckExpression.getFormat(), p);
      controls.add(selectLabel);
      controls.add(deckExp.getControls(), "split 2"); //NON-NLS

      promptLabel = new JLabel(Resources.getString("Editor.ReturnToDeck.prompt_for_destination_deck"));
      promptText = new StringConfigurer(p.selectDeckPrompt);
      controls.add(promptLabel, promptText);

      updateVisibility();
    }

    private void updateVisibility() {
      promptLabel.setVisible(prompt.getValueBoolean());
      promptText.getControls().setVisible(prompt.getValueBoolean());
      deckExp.getControls().setVisible(!prompt.getValueBoolean());
      selectLabel.setVisible(!prompt.getValueBoolean());
      repack(controls);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(menuName.getValueString())
        .append(menuKey.getValueString())
        .append(deckId)
        .append(promptText.getValueString())
        .append(description.getValueString())
        .append(RtDversion)
        .append(prompt.getValueBoolean())
        .append(deckExp.getValueString());
      return ID + se.getValue();
    }
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return List.of(deckExpression.getFormat());
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Collections.singletonList(returnKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(returnCommand);
  }

  /**
   * @return a list of any Message Format strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return List.of(selectDeckPrompt);
  }
}
