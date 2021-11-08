/*
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */

package VASSAL.build.module;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdChecker;
import VASSAL.build.GpIdSupport;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.SetupStack;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.command.RemovePiece;
import VASSAL.configure.ConfigurerLayout;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Mat;
import VASSAL.counters.MatCargo;
import VASSAL.counters.MatHolder;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.swing.SwingUtils;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import net.miginfocom.swing.MigLayout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * GameRefresher Replace all counters in the same game with the current version
 * of the counters defined in the module
 *
 * Note: Counters that are Hidden or Obscured to us cannot be updated.
 *
 */
public final class GameRefresher implements CommandEncoder, GameComponent {

  private static final Logger logger = LoggerFactory.getLogger(GameRefresher.class);

  private static final char DELIMITER = '\t'; //$NON-NLS-1$
  public  static final String COMMAND_PREFIX = "DECKREPOS" + DELIMITER; //$NON-NLS-1$
  private String id;
  private Point newPosition;

  private Action refreshAction;
  private final GpIdSupport gpIdSupport;
  private GpIdChecker  gpIdChecker;
  //  private List<GamePiece> pieces;
  private RefreshDialog dialog;
  //  private boolean testMode;
//  private boolean useLabelerName;
  private int updatedCount;
  private int notFoundCount;
  private int noStackCount;
  private int noMapCount;

  private final GameModule theModule;
  //private String player;
  //private final Chatter chatter;
  private final Set<String> options = new HashSet<>();

  public List<DrawPile>  getModuleDrawPiles() {
    return  theModule.getAllDescendantComponentsOf(DrawPile.class);
  }

  public GameRefresher(GpIdSupport gpIdSupport) {
    this.gpIdSupport = gpIdSupport;
    theModule = GameModule.getGameModule();
    //player = GlobalOptions.getInstance().getPlayerId();
    //FIXME add messge abt player if in Player mode and not owned are found
    //chatter = theModule.getChatter();
    //msg = new Chatter.DisplayText(chatter, "----------"); //$NON-NLS-1$
  }

  @Override
  public String encode(final Command c) {
    if (c instanceof DeckRepositionCommand) {
      final DeckRepositionCommand drc = (DeckRepositionCommand) c;
      final SequenceEncoder se = new SequenceEncoder(DELIMITER);
      se.append(drc.id)
        .append(drc.newPosition.x)
        .append(drc.newPosition.y)
        .append(drc.oldPosition.x)
        .append(drc.oldPosition.y);
      return COMMAND_PREFIX + se.getValue();
    }
    return null;
  }

  /**
   * Deserializes string command info into a Deck Reposition Command.
   * @param s String for a Deck Reposition command string
   * @return Deck Reposition Command object
   */
  @Override
  public Command decode(final String s) {
    if (s.startsWith(COMMAND_PREFIX)) { // Make sure this command is for a Deck Reposition
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, DELIMITER);
      sd.nextToken(); // Skip over the Command Prefix
      final String id = sd.next();
      final int x = sd.nextInt(0);
      final int y = sd.nextInt(0);
      final Point newPosition = new Point(x, y);
      final int xold = sd.nextInt(0);
      final int yold = sd.nextInt(0);
      final Point oldPosition = new Point(xold, yold);
      return new DeckRepositionCommand(id, newPosition, oldPosition);
    }
    return null;
  }


  public void addTo(AbstractConfigurable parent) {
    refreshAction = new AbstractAction(Resources.getString("GameRefresher.refresh_counters")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        new GameRefresher(gpIdSupport).start();
      }
    };
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
    refreshAction.setEnabled(false);
  }

/*
  */
/**
   * Removes this component from a Buildable parent.
   * @param parent - the Map to remove the Flare from.
   *//*

  public void removeFrom(final Buildable parent) {
    if (parent instanceof Map) {
      GameModule.getGameModule().removeCommandEncoder(this);
    }
    //idMgr.remove(this);
  }
*/

  public Action getRefreshAction() {
    return refreshAction;
  }

  public boolean isTestMode() {
    return options.contains("TestMode"); //$NON-NLS-1$
  }

  public boolean isDeleteNoMap() {
    return options.contains("DeleteNoMap"); //$NON-NLS-1$
  }

  public void start() {
    dialog = new RefreshDialog(this);
    dialog.setVisible(true);
    dialog = null;
  }

  public void log(String message) {
    // ex for dialog msg dialog.addMessage(Resources.getString("GameRefresher.counters_refreshed_test", updatedCount));
    // Log to chatter
    GameModule.getGameModule().warn(message);
    //msg.append(new Chatter.DisplayText(chatter, message));
    logger.info(message);
  }

  public List<GamePiece>  getCurrentGameRefresherPieces() {
    final List<GamePiece> pieces = new ArrayList<>();
    int totalCount = 0;
    int notOwnedCount = 0;
    int notVisibleCount = 0;

    for (final GamePiece piece : theModule.getGameState().getAllPieces()) {
      if (piece instanceof Deck) {
        for (final Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext();) {
          totalCount++;
          pieces.add(0, i.next());
        }
      }
      else if (piece instanceof Stack) {
        for (final Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext();) {
          final GamePiece p = i.next();
          if (!Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))
            && !Boolean.TRUE.equals(p.getProperty(Properties.OBSCURED_TO_ME))) {
            totalCount++;
            pieces.add(0, p);
          }
          else {
            if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
              notVisibleCount++;
            }
            else {
              notOwnedCount++;
            }
          }
        }
      }
      else if (piece.getParent() == null) {
        if (!Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
          && !Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
          totalCount++;
          pieces.add(0, piece);
        }
        else {
          if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
            notVisibleCount++;
          }
          else {
            notOwnedCount++;
          }
        }
      }
//      else {
//      All pieces in stacks and decks seem to be returned as pieces with parent so these are duplicates to ignore        //
//    }
    }
    log(Resources.getString("GameRefresher.get_all_pieces"));
    log(Resources.getString("GameRefresher.counters_total", totalCount));
    log(Resources.getString("GameRefresher.counters_kept", totalCount - notOwnedCount - notVisibleCount));
    log(Resources.getString("GameRefresher.counters_not_owned", notOwnedCount));
    log(Resources.getString("GameRefresher.counters_not_visible", notVisibleCount));
    log("-"); //$NON-NLS-1$
//   msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_total", totalCount));
//   msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_not_owned", notOwnedCount));
    return pieces;
  }


  /**
   * This method is used by PredefinedSetup.refresh() to update a PredefinedSetup in a GameModule
   * The default execute() method calls: GameModule.getGameModule().getGameState().getAllPieces()
   * to set the pieces list, this method provides an alternative way to specify which pieces should be refreshed.
   * @throws IllegalBuildException - if we get a gpIdChecker error
   */
  public void execute(Set<String> options, Command command) throws IllegalBuildException {
    if (command == null) {
      command = new NullCommand();
    }
    if (!options.isEmpty()) {
      this.options.addAll(options);
    }
    notFoundCount = 0;
    updatedCount = 0;
    noMapCount = 0;
    noStackCount = 0;
    /*
     * 1. Use the GpIdChecker to build a cross-reference of all available
     * PieceSlots and PlaceMarker's in the module.
     */
    if (Objects.isNull(gpIdChecker)) { //Only setup gpIdChecker once and keep it in the instance of GameRefresher.
      gpIdChecker = new GpIdChecker(options);
      for (final PieceSlot slot : theModule.getAllDescendantComponentsOf(PieceSlot.class)) {
        gpIdChecker.add(slot);
      }
  
      // Add any PieceSlots in Prototype Definitions
      for (final PrototypesContainer pc : theModule.getComponentsOf(PrototypesContainer.class)) {
        pc.getDefinitions().forEach(gpIdChecker::add);
      }

      if (gpIdChecker.hasErrors()) {
        // Any gpid errors should have been resolved by the GpId check when the editor is run.
        // If a module created before gpIDChecker was setup is run on a vassal version with gmIDChecker
        // is run in the player, errors might still be present.
        // Inform user that he must upgrade the module to the latest vassal version before running Refresh
        gpIdChecker = null;
        log(Resources.getString("GameRefresher.gpid_error_message"));
        return;
        //throw new IllegalBuildException("GameRefresher.execute: gpIdChecker has errors"); //$NON-NLS-1$
      }
    }

    /*
     * 2. Collect the game pieces
     */
    final List<GamePiece> pieces = getCurrentGameRefresherPieces();

    /*
     * 2.1 Pull out any Mats and their Cargo for special treatment
     */
    if (GameModule.getGameModule().isMatSupport()) {
      final List<GamePiece> otherPieces = new ArrayList<>();
      final List<GamePiece> cargo = new ArrayList<>();
      final List<MatRefresher> mats = new ArrayList<>();

      // Split the pieces out based on their type
      for (final GamePiece piece : pieces) {
        if (Boolean.TRUE.equals(piece.getProperty(MatCargo.IS_CARGO))) {
          // Cargo
          cargo.add(piece);
        }
        else if (piece.getProperty(Mat.MAT_ID) != null) {
          // Mat
          mats.add(new MatRefresher(piece));
        }
        else {
          // Other Pieces
          otherPieces.add(piece);
        }
      }

      // Remove any cargo currently on a mat from the general cargo list and add to its MatRefresher
      for (final MatRefresher mh : mats) {
        mh.grabCargo(cargo);
      }

      // Refresh non-mat pieces
      for (final GamePiece piece : otherPieces) {
        processGamePiece(piece, command);
      }

      // Refresh Cargo not on Mats
      for (final GamePiece piece : cargo) {
        processGamePiece(piece, command);
      }

      // Refresh the Mats and their contained Cargo
      for (final MatRefresher mh : mats) {
        mh.refresh(command);
      }
    }
    else {
      /*
       * 3. Generate the commands to update the pieces
       */
      for (final GamePiece piece : pieces) {
        processGamePiece(piece, command);
      }
    }

    log(Resources.getString("GameRefresher.run_refresh_counters_v3", theModule.getGameVersion()));
    log(Resources.getString("GameRefresher.counters_refreshed", updatedCount));
    log(Resources.getString("GameRefresher.counters_not_found", notFoundCount));
    log(Resources.getString("GameRefresher.counters_no_map", noMapCount));
    log("----------"); //$NON-NLS-1$
    log(Resources.getString("GameRefresher.counters_no_stack", noStackCount));
    log("----------"); //$NON-NLS-1$


    /*
     * 4/ Refresh properties of decks in the game
     */
    if (options.contains("RefreshDecks")) { //NON-NLS
      final GameModule gm = GameModule.getGameModule();
      final VASSAL.command.Logger logger = gm.getLogger();
      if (gm.isMultiplayerConnected() || ((logger instanceof BasicLogger) && ((BasicLogger)logger).isLogging())) {
        // If somebody feels like packaging all these things into Commands, help yourself...
        log(Resources.getString("GameRefresher.deck_refresh_during_multiplayer"));
      }
      else {

        //Drawpiles have the module definition of the Deck in the dummy child object
        //  and a link to the actual Deck in the game.
        final List<Deck> decksToDelete = new ArrayList<>();
        final List<DrawPile> drawPiles = getModuleDrawPiles();
        final List<DrawPile> foundDrawPiles = new ArrayList<>();
        final List<DrawPile> decksToAdd = new ArrayList<>();

        int refreshable = 0;
        int deletable = 0;
        int addable = 0;

        log("----------");
        log(Resources.getString("GameRefresher.refreshing_decks"));
        log("----------");
        for (final Map map : Map.getMapList()) {
          for (final GamePiece pieceOrStack : map.getPieces()) {
            if (pieceOrStack instanceof Deck) {
              final Deck deck = (Deck) pieceOrStack;
              // Match with a DrawPile if possible
              boolean deckFound = false;
              for (final DrawPile drawPile : drawPiles) {
                final String deckName = deck.getDeckName();
                if (deckName.equals(drawPile.getAttributeValueString(SetupStack.NAME))) {
                  final Board pileBoard = drawPile.getConfigureBoard(true);
                  final Board deckBoard = map.findBoard(deck.getPosition());
                  if (deckBoard == pileBoard) {
                    deckFound = true;
                    foundDrawPiles.add(drawPile);

                    final String drawPileName = drawPile.getAttributeValueString(SetupStack.NAME);
                    log(Resources.getString("GameRefresher.refreshing_deck", deckName, drawPileName));

                    // This refreshes the existing deck with all the up-to-date drawPile fields from the module
                    deck.myRefreshType(drawPile.getDeckType());

                    // Make sure the deck is in the right place
                    final Point pt = drawPile.getPosition();
                    deck.setPosition(pt);
                    for (final GamePiece piece : deck.asList()) {
                      piece.setPosition(pt);
                    }

                    refreshable++;
                    break;
                  }
                }
              }
              if (!deckFound) {
                deletable++;
                decksToDelete.add(deck);
              }
            }
          }
        }

        if (options.contains("DeleteOldDecks")) { //NON-NLS
          //log("List of Decks to remove");
          for (final Deck deck : decksToDelete) {
            log(Resources.getString("GameRefresher.deleting_old_deck", deck.getDeckName()));

            final Stack newStack = new Stack();

            // First let's remove all the pieces from the deck and put them in a new stack.
            boolean any = false;
            for (final GamePiece piece : deck.asList()) {
              newStack.add(piece);
              if (!any) {
                any = true;
                newStack.setPosition(piece.getPosition());
              }
            }

            // Now, the deck goes bye-bye
            deck.removeAll();
            if (deck.getMap() != null) {
              deck.getMap().removePiece(deck);
              deck.setMap(null);
            }

            // If there were any pieces left in the deck, add the new stack to the map
            if ((newStack.getPieceCount() > 0) && (newStack.getMap() != null)) {
              GameModule.getGameModule().getGameState().addPiece(newStack);
              newStack.getMap().placeAt(newStack, newPosition);
            }
          }
        }
        else if (!decksToDelete.isEmpty()) {
          log(Resources.getString("GameRefresher.deletable_with_option"));
          for (final Deck deck : decksToDelete) {
            log(deck.getDeckName());
          }
        }

        // Figure out if any decks need to be added
        for (final DrawPile drawPile : drawPiles) {
          boolean matchFound = false;

          final Map map = drawPile.getMap();
          final Collection<Board> boards = map.getBoards();
          final String boardName = drawPile.getOwningBoardName();
          final Board board = drawPile.getConfigureBoard(true);
          if ((boardName == null) || boards.contains(board)) {
            for (final DrawPile drawPile2 : foundDrawPiles) {
              if (drawPile.getAttributeValueString(SetupStack.NAME).equals(drawPile2.getAttributeValueString(SetupStack.NAME))) {
                matchFound = true;
                break;
              }
            }
            if (!matchFound) {
              decksToAdd.add(drawPile);
              addable++;
            }
          }
        }

        if (!decksToAdd.isEmpty()) {
          if (options.contains("AddNewDecks")) { //NON-NLS
            for (final DrawPile drawPile : decksToAdd) {
              log(Resources.getString("GameRefresher.adding_new_deck", drawPile.getAttributeValueString(SetupStack.NAME)));

              final Deck newDeck = drawPile.makeDeck();
              final Map newMap = drawPile.getMap();
              if (newMap != null) {
                GameModule.getGameModule().getGameState().addPiece(newDeck);
                newMap.placeAt(newDeck, newDeck.getPosition());
              }
            }
          }
          else {
            log(Resources.getString("GameRefresher.addable_with_option"));
            for (final DrawPile drawPile : decksToAdd) {
              log(drawPile.getAttributeValueString(SetupStack.NAME));
            }
          }
        }

        log("----------"); //$NON-NLS-1$
        log(Resources.getString("GameRefresher.refreshable_decks", refreshable));
        log(Resources.getString(options.contains("DeleteOldDecks") ? "GameRefresher.deletable_decks" : "GameRefresher.deletable_decks_2", deletable)); //NON-NLS
        log(Resources.getString(options.contains("AddNewDecks") ? "GameRefresher.addable_decks" : "GameRefresher.addable_decks_2", addable)); //NON-NLS
      }
    }
  }

  private GamePiece processGamePiece(GamePiece piece, Command command) {
    // Piece needs to be on a map. Else how do we put it back.
    final Map map = piece.getMap();
    if (map == null) {
      noMapCount++;
      log(Resources.getString("GameRefresher.refresh_error_nomap1", piece.getName(), piece.getId()));
      // If Option "Delete pieces with no map" is set to true. Get rid of this piece
      if (isDeleteNoMap()) {
        log(Resources.getString("GameRefresher.refresh_error_nomap2", piece.getName(), piece.getId()));
        final Command remove = new RemovePiece(Decorator.getOutermost(piece));
        remove.execute();
        command.append(remove);
      }
      return piece;
    }

    // Piece should have a parent stack (Decks are extensions of Stacks)
    // Except pieces that return TRUE for the NO_STACK property.
    final Stack oldStack = piece.getParent();
    if (oldStack == null) {
      if (!Boolean.TRUE.equals(Decorator.getOutermost(piece).getProperty(Properties.NO_STACK))) {
        noStackCount++;
        log(Resources.getString("GameRefresher.refresh_error_nostack", piece.getName(), piece.getId()));
      }
    }


    //create a new piece. If returns null, it failed
    final GamePiece newPiece = gpIdChecker.createUpdatedPiece(piece);
    if (newPiece == null) {
      notFoundCount++;
      log(Resources.getString("GameRefresher.refresh_error_nomatch_pieceslot", piece.getName(), piece.getId()));
      return piece;
    }

    updatedCount++;

    if (isTestMode()) {
      // Test mode. Do not replace old pieces with new. Just get rid of the new piece.
      // Delete the old piece
      final Command remove = new RemovePiece(Decorator.getOutermost(newPiece));
      remove.execute();
      command.append(remove);

    }
    else {
      // Refreshing is done. This section is for non test mode, to replace all the old pieces with the new pieces
      final Point piecePosition = piece.getPosition();
      final Point hiddenPosition = new Point(-100, -100);
      Point tempPosition = piecePosition;
      final int oldStackIndex = oldStack == null ? 0 : oldStack.indexOf(piece);

      // Delete old piece 1st. Doing that after placing the new piece causes errors if the old piece has no stack
      // as same pos as new piece, it somehow deleted the new stack too!

      // Place new piece on the map at position -100 -100 (this is to fix bug 14440). For some reason
      // the new piece misbehaves in a stack (not visible when stack expanded) when 1st placed on the map (or game)
      // By placing in -100 -100 1st the error seems to disappear

      // Pieces in Decks
      // Here we use a workaround for placing pieces into decks. If 2 decks are defined
      // with the same x,y position, the piece placement will not be able to determine
      // the target deck. So 1st move the deck of the piece being replaced to -1, -1
      // then put it back to its original position

      final Stack stack = piece.getParent();
      Deck deck = null;
      String id = "";
      final boolean isDeck = (stack instanceof  Deck);
      if (isDeck) {
        deck = (Deck) stack;
        id = deck.getId();
        tempPosition = getDeckFreePosition(deck);
        final Command deckRepositionCommand = new DeckRepositionCommand(id, tempPosition, piecePosition);
        deckRepositionCommand.execute();
        command.append(deckRepositionCommand);
      }

      // Delete the old piece
      final Command remove = new RemovePiece(Decorator.getOutermost(piece));
      remove.execute();
      command.append(remove);

      Command place = map.placeOrMerge(newPiece, hiddenPosition);
      command.append(place);
      place = map.placeOrMerge(newPiece, tempPosition);
      command.append(place);

      if (isDeck) {
        final Command deckRepositionCommand = new DeckRepositionCommand(id, piecePosition, tempPosition);
        deckRepositionCommand.execute();
        command.append(deckRepositionCommand);
      }

      // Move to the correct position in the stack
      final Stack newStack = newPiece.getParent();
      if ((newStack != null) && (newStack == oldStack)) {
        final int newPos = newStack.indexOf(newPiece);
        if (newPos >= 0 && oldStackIndex >= 0 && newPos != oldStackIndex) {
          final String oldState = newStack.getState();
          newStack.insert(newPiece, oldStackIndex);
          command.append(new ChangePiece(newStack.getId(), oldState, newStack.getState()));
        }
      }
    }
    return newPiece;
  }

  private Point getDeckFreePosition(Deck deck) {
    Point tempPosition = new Point(-1, -1);
    Boolean correctTempPositionNotFound;
    final GamePiece[] pieces;
    pieces = deck.getMap().getAllPieces();
    do {
      correctTempPositionNotFound = false; //Assuming scan of pieces finds no match on tempPosition
      for (final GamePiece piece : pieces) {
        final Point piecePosition = piece.getPosition();
        if (piecePosition.equals(tempPosition)) {
          tempPosition.x -= 1;
          correctTempPositionNotFound = true;
          break;
        }
      }
    } while (correctTempPositionNotFound);
    return tempPosition;
  }


  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * Enable Refresh menu item when game is running only.
   */
  @Override
  public void setup(boolean gameStarting) {
    refreshAction.setEnabled(gameStarting);
  }

  static class RefreshDialog extends JDialog {
    private static final long serialVersionUID = 1L;
    private final GameRefresher refresher;
    private JTextArea results;
    private JCheckBox nameCheck;
    private JCheckBox testModeOn;
    private JCheckBox labelerNameCheck;
    private JCheckBox layerNameCheck;
    private JCheckBox deletePieceNoMap;
    private JCheckBox refreshDecks;
    private JCheckBox deleteOldDecks;
    private JCheckBox addNewDecks;
    private final Set<String> options = new HashSet<>();
    JButton runButton;

    RefreshDialog(GameRefresher refresher) {
      super(GameModule.getGameModule().getPlayerWindow());
      this.refresher = refresher;
      setTitle(Resources.getString("GameRefresher.refresh_counters"));
      setModal(true);
      initComponents();
    }

    protected void initComponents() {
      setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent we) {
          exit();
        }
      });
      setLayout(new MigLayout("wrap 1", "[fill]")); //NON-NLS


      final JPanel panel = new JPanel(new MigLayout("hidemode 3,wrap 1" + "," + ConfigurerLayout.STANDARD_GAPY, "[fill]")); // NON-NLS
      panel.setBorder(BorderFactory.createEtchedBorder());

      final FlowLabel header = new FlowLabel(Resources.getString("GameRefresher.header"));
      panel.add(header);

      final JPanel buttonPanel = new JPanel(new MigLayout("ins 0", "push[]rel[]rel[]push")); // NON-NLS


      runButton = new JButton(Resources.getString("General.run"));
      runButton.addActionListener(e -> run());

      final JButton exitButton = new JButton(Resources.getString("General.cancel"));
      exitButton.addActionListener(e -> exit());

      final JButton helpButton = new JButton(Resources.getString("General.help"));
      helpButton.addActionListener(e -> help());

      buttonPanel.add(runButton, "tag ok,sg 1"); // NON-NLS
      buttonPanel.add(exitButton, "tag cancel,sg 1"); // NON-NLS
      buttonPanel.add(helpButton, "tag help,sg 1"); // NON-NLS

      nameCheck = new JCheckBox(Resources.getString("GameRefresher.use_basic_name"));
      panel.add(nameCheck);
      labelerNameCheck = new JCheckBox(Resources.getString("GameRefresher.use_labeler_descr"));
      panel.add(labelerNameCheck);
      layerNameCheck = new JCheckBox(Resources.getString("GameRefresher.use_layer_descr"));
      panel.add(layerNameCheck);
      testModeOn = new JCheckBox(Resources.getString("GameRefresher.test_mode"));
      panel.add(testModeOn);
      deletePieceNoMap = new JCheckBox(Resources.getString("GameRefresher.delete_piece_no_map"));
      deletePieceNoMap.setSelected(true);
      panel.add(deletePieceNoMap);

      refreshDecks = new JCheckBox(Resources.getString("GameRefresher.refresh_decks"));
      refreshDecks.setSelected(false);
      refreshDecks.addChangeListener(new ChangeListener() {
        @Override
        public void stateChanged(ChangeEvent e) {
          deleteOldDecks.setVisible(refreshDecks.isSelected());
          addNewDecks.setVisible(refreshDecks.isSelected());
        }
      });
      panel.add(refreshDecks);

      deleteOldDecks = new JCheckBox(Resources.getString("GameRefresher.delete_old_decks"));
      deleteOldDecks.setSelected(false);
      panel.add(deleteOldDecks);

      addNewDecks = new JCheckBox(Resources.getString("GameRefresher.add_new_decks"));
      addNewDecks.setSelected(false);
      panel.add(addNewDecks);

      panel.add(buttonPanel, "grow"); // NON-NLS

      add(panel, "grow"); // NON-NLS

      SwingUtils.repack(this);

      deleteOldDecks.setVisible(refreshDecks.isSelected());
      addNewDecks.setVisible(refreshDecks.isSelected());
    }

    protected void  setOptions() {
      options.clear();
      if (nameCheck.isSelected()) {
        options.add("UseName"); //$NON-NLS-1$
      }
      if (labelerNameCheck.isSelected()) {
        options.add("UseLabelerName"); //$NON-NLS-1$
      }
      if (layerNameCheck.isSelected()) {
        options.add("UseLayerName"); //$NON-NLS-1$
      }
      if (testModeOn.isSelected()) {
        options.add("TestMode"); //$NON-NLS-1$
      }
      if (deletePieceNoMap.isSelected()) {
        options.add("DeleteNoMap"); //$NON-NLS-1$
      }
      if (refreshDecks.isSelected()) {
        options.add("RefreshDecks"); //NON-NLS
        if (deleteOldDecks.isSelected()) {
          options.add("DeleteOldDecks"); //NON-NLS
        }
        if (addNewDecks.isSelected()) {
          options.add("AddNewDecks"); //NON-NLS
        }
      }
    }

    protected void exit() {
      setVisible(false);
    }

/*
    protected void test() {
      setOptions();
      options.add("TestMode");
      refresher.log(Resources.getString("GameRefresher.refresh_counters_test_mode"));
      refresher.execute(options, null);
    }
*/

    private boolean hasAlreadyRun = false;

    protected void run() {
      if (hasAlreadyRun) {
        return;
      }
      hasAlreadyRun = true;
      final GameModule g = GameModule.getGameModule();
      final Command command = new NullCommand();
      final String player = GlobalOptions.getInstance().getPlayerId();
      setOptions();
      if (refresher.isTestMode()) {
        refresher.log(Resources.getString("GameRefresher.refresh_counters_test_mode"));
      }
      else {
        // Test refresh does not need to be in the log.
        final Command msg = new Chatter.DisplayText(g.getChatter(), Resources.getString("GameRefresher.run_refresh_counters_v2", player, g.getGameVersion()));
        msg.execute();
        command.append(msg);
//FIXME list options in chatter for opponents to see

      }
      refresher.execute(options, command);

      // Send the update to other clients (only done in Player mode)
      g.sendAndLog(command);

      exit();
    }

    protected void help() {
      File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
      dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
      final File theFile = new File(dir, "GameRefresher.html"); //$NON-NLS-1$
      HelpFile h = null;
      try {
        h = new HelpFile(null, theFile, "#top"); //$NON-NLS-1$
      }
      catch (MalformedURLException e) {
        ErrorDialog.bug(e);
      }
      BrowserSupport.openURL(h.getContents().toString());
    }

    public void addMessage(String mess) {
      results.setText(results.getText() + "\n" + mess); //NON-NLS
    }
  }

 /**
    * A {@link Command} for sending {@link GameRefresher} DeckReposition actions to other clients
 */
  private class DeckRepositionCommand extends Command {
    private final String id;
    private final Point newPosition;
    private final Point oldPosition;

    /**
     */
    public DeckRepositionCommand(String id, Point newPosition, Point oldPosition) {
      this.id = id;
      this.newPosition = newPosition;
      this.oldPosition = oldPosition;
    }

    /**
     * Executes the command (starts a Flare at the specified location)
     */
    @Override
    protected void executeCommand() {
      Deck deck = null;
      //Scan pieces to find the Deck
      for (final GamePiece piece : GameModule.getGameModule().getGameState().getAllPieces()) {
        if (piece instanceof Deck) {
          final String deckId = ((Deck) piece).getId();
          if (deckId.equals(id)) {
            deck =  (Deck) piece;
            break;
          }
        }
      }
      if (deck == null) {
        return;
      }
      deck.setPosition(this.newPosition);
    }

    /**
     */
    @Override
    protected Command myUndoCommand() {
      return new DeckRepositionCommand(this.id, this.oldPosition, this.newPosition);
    }
  }

  /**
   * Class to hold and refresh a Mat and it's cargo
   */
  private class MatRefresher extends MatHolder {

    public MatRefresher(GamePiece piece) {
      super(piece);
    }

    /**
     * Refresh this Mat and it's cargo
     * 1. Remove all cargo from the Mat
     * 2. Refresh the Mat
     * 3. Refresh each Cargo and place back on the Mat
     * @param command
     */
    public void refresh(Command command) {
      // Remove any existing cargo
      command.append(getMat().makeRemoveAllCargoCommand());

      // Refresh the Mat piece
      final GamePiece newMatPiece = processGamePiece(getMatPiece(), command);

      // Now refresh each cargo piece and add it back to the mat
      for (final GamePiece c : getCargo()) {
        final GamePiece newCargo = processGamePiece(c, command);
        command.append(((Mat) Decorator.getDecorator(newMatPiece, Mat.class)).makeAddCargoCommand(newCargo));
      }
    }
  }
}




