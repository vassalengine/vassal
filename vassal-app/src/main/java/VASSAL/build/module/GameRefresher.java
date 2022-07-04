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
import VASSAL.command.AddPiece;
import VASSAL.command.AlertCommand;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.NullCommand;
import VASSAL.command.RemovePiece;
import VASSAL.configure.ConfigurerLayout;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Mat;
import VASSAL.counters.MatHolder;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.swing.SwingUtils;
import net.miginfocom.swing.MigLayout;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

/**
 * GameRefresher Replace all counters in the same game with the current version
 * of the counters defined in the module
 * <p>
 * Note: Counters that are Hidden or Obscured to us cannot be updated.
 */
public final class GameRefresher implements CommandEncoder, GameComponent {

  private static final Logger logger = LoggerFactory.getLogger(GameRefresher.class);

  private static final char DELIMITER = '\t'; //$NON-NLS-1$
  public static final String COMMAND_PREFIX = "DECKREPOS" + DELIMITER; //$NON-NLS-1$

  private Action refreshAction;
  private final GpIdSupport gpIdSupport;
  private GpIdChecker gpIdChecker;
  private RefreshDialog dialog;
  private int updatedCount;
  private int notFoundCount;
  private int noStackCount;
  private int noMapCount;

  private final GameModule theModule;
  private final Set<String> options = new HashSet<>();

  public List<DrawPile> getModuleDrawPiles() {
    return theModule.getAllDescendantComponentsOf(DrawPile.class);
  }

  public GameRefresher(GpIdSupport gpIdSupport) {
    this.gpIdSupport = gpIdSupport;
    theModule = GameModule.getGameModule();
  }

  @Override
  public String encode(final Command c) {
    return null;
  }

  @Override
  public Command decode(final String s) {
    return null;
  }

  public void addTo(AbstractConfigurable parent) {
    refreshAction = new AbstractAction(Resources.getString("GameRefresher.refresh_counters")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {

        final BasicLogger bl = GameModule.getGameModule().getBasicLogger();
        if ((bl != null) && bl.isReplaying()) {
          final Command ac = new AlertCommand(Resources.getString("GameRefresher.game_is_replaying"));
          ac.execute();
        }
        else {
          new GameRefresher(gpIdSupport).start();
        }
      }
    };
    GameModule.getGameModule().getGameState().addGameComponent(this);
    GameModule.getGameModule().addCommandEncoder(this);
    refreshAction.setEnabled(false);
  }

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
    logger.info(message);
  }

  /**
   * Build a list of all the Refreshables in the module in Visual order (bottom to top) so that we can ensure
   * the visibility order of the refreshed pieces does not change.
   * A Refreshable can be one of
   * - Stack
   * - Deck
   * - Mat with contained Cargo
   * - Single non-Mat unstacked piece
   *
   * @return
   */
  public List<Refresher> getRefreshables() {
    final List<Refresher> refreshables = new ArrayList<>();
    final List<MatRefresher> loadedMats = new ArrayList<>();
    int totalCount = 0;
    int notOwnedCount = 0;
    int notVisibleCount = 0;

    // Process map by map
    for (final Map map : Map.getMapList()) {

      // Get the pieces on this map in visual order
      for (final GamePiece piece : map.getAllPieces()) {

        // A Deck. Pieces in a Deck can always be refreshed
        if (piece instanceof Deck) {
          final Deck deck = (Deck) piece;
          totalCount += deck.getPieceCount();
          refreshables.add(new DeckRefresher(deck));
        }

        // A standard Stack
        else if (piece instanceof Stack) {
          for (final Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext(); ) {
            final GamePiece p = i.next();
            if (!Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))
              && !Boolean.TRUE.equals(p.getProperty(Properties.OBSCURED_TO_ME))) {
              totalCount++;
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
          if (((Stack) piece).getMap() != null) {
            refreshables.add(new StackRefresher((Stack) piece));
          }
        }

        // An Unstacked piece
        else {
          final GamePiece p = (GamePiece) piece;

          // Only visible, unobscured pieces are refreshable
          if (!Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
            && !Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
            totalCount++;

            // Mats with loaded cargo need to be handled separately
            if (p.getProperty(Mat.MAT_ID) != null && !"0".equals(p.getProperty(Mat.MAT_NUM_CARGO))) {
              final MatRefresher mr = new MatRefresher(p);
              refreshables.add(mr);
              loadedMats.add(mr);
            }
            else {
              refreshables.add(new PieceRefresher(p, true));
            }
          }
          else {
            if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))) {
              notVisibleCount++;
            }
            else {
              notOwnedCount++;
            }
            // Add as non-refreshable
            refreshables.add(new PieceRefresher(p, false));
          }
        }
      }

      // If there are any loaded Mats, then find the Stacks of their cargo in the general Refeshables list,
      // remove them and add them to the MatRefresher.
      for (final MatRefresher mr : loadedMats) {
        mr.grabMyCargo(refreshables);
      }

    }

    log(Resources.getString("GameRefresher.get_all_pieces"));
    log(Resources.getString("GameRefresher.counters_total", totalCount));
    log(Resources.getString("GameRefresher.counters_kept", totalCount - notOwnedCount - notVisibleCount));
    log(Resources.getString("GameRefresher.counters_not_owned", notOwnedCount));
    log(Resources.getString("GameRefresher.counters_not_visible", notVisibleCount));
    log("-"); //$NON-NLS-1$

    return refreshables;
  }

  private boolean isGameActive() {
    final GameModule gm = GameModule.getGameModule();
    final BasicLogger logger = gm.getBasicLogger();
    return gm.isMultiplayerConnected() || ((logger != null) && logger.isLogging());
  }


  /**
   * This method is used by PredefinedSetup.refresh() to update a PredefinedSetup in a GameModule
   * The default execute() method calls: GameModule.getGameModule().getGameState().getAllPieces()
   * to set the pieces list, this method provides an alternative way to specify which pieces should be refreshed.
   *
   * @throws IllegalBuildException - if we get a gpIdChecker error
   */
  public void execute(Set<String> options, Command command) throws IllegalBuildException {
    final List<Deck> decks = new ArrayList<>();

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
      }
    }

    /*
     * 2. Build a list in visual order of all stacks, decks, mats and other pieces that need refreshing
     */
    final List<Refresher> refreshables = getRefreshables();

    /*
     * And refresh them. Keep a list of the Decks in case we need to update their attributes
     */
    for (final Refresher refresher : refreshables) {
      refresher.refresh(command);
      if (refresher instanceof DeckRefresher) {
        decks.add(((DeckRefresher) refresher).getDeck());
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
      if (isGameActive()) {
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

                  // If drawPile is owned by a specific board, then we can only match it if that board is active in this game
                  if (drawPile.getOwningBoardName() != null) {
                    if (map.getBoardByName(drawPile.getOwningBoardName()) == null) {
                      continue;
                    }

                    // If the drawPile is on a map that doesn't have its current owning board active, then we
                    // cannot match that drawPile.
                    if (drawPile.getMap().getBoardByName(drawPile.getOwningBoardName()) == null) {
                      continue;
                    }
                  }

                  deckFound = true;
                  foundDrawPiles.add(drawPile);

                  final String drawPileName = drawPile.getAttributeValueString(SetupStack.NAME);
                  log(Resources.getString("GameRefresher.refreshing_deck", deckName, drawPileName));

                  // This refreshes the existing deck with all the up-to-date drawPile fields from the module
                  deck.removeListeners();
                  deck.myRefreshType(drawPile.getDeckType());
                  deck.addListeners();

                  // Make sure the deck is in the right place
                  final Point pt = drawPile.getPosition();
                  final Map newMap = drawPile.getMap();
                  if (newMap != map) {
                    map.removePiece(deck);
                    newMap.addPiece(deck);
                  }
                  deck.setPosition(pt);
                  for (final GamePiece piece : deck.asList()) {
                    piece.setMap(newMap);
                    piece.setPosition(pt);
                  }

                  refreshable++;
                  break;
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
            newStack.setMap(deck.getMap());

            // First let's remove all the pieces from the deck and put them in a new stack.
            for (final GamePiece piece : deck.asList()) {
              newStack.add(piece);
            }
            newStack.setPosition(deck.getPosition());

            // Now, the deck goes bye-bye
            deck.removeAll();
            if (deck.getMap() != null) {
              deck.removeListeners();
              deck.getMap().removePiece(deck);
              deck.setMap(null);
            }

            // If there were any pieces left in the deck, add the new stack to the map
            if ((newStack.getPieceCount() > 0) && (newStack.getMap() != null)) {
              GameModule.getGameModule().getGameState().addPiece(newStack);
              newStack.getMap().placeAt(newStack, newStack.getPosition());
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
                drawPile.setDeck(newDeck);
                GameModule.getGameModule().getGameState().addPiece(newDeck);
                newMap.placeAt(newDeck, drawPile.getPosition());
                if (GameModule.getGameModule().getGameState().isGameStarted()) {
                  newDeck.addListeners();
                }
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

      // Default actions on Enter/ESC
      SwingUtils.setDefaultButtons(getRootPane(), runButton, exitButton);

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

      if (refresher.isGameActive()) {
        refreshDecks.setSelected(false);
        refreshDecks.setEnabled(false);
        deleteOldDecks.setEnabled(false);
        addNewDecks.setEnabled(false);

        final FlowLabel nope = new FlowLabel(Resources.getString("GameRefresher.but_game_is_active"));
        panel.add(nope);
      }

      panel.add(buttonPanel, "grow"); // NON-NLS

      add(panel, "grow"); // NON-NLS

      SwingUtils.repack(this);

      deleteOldDecks.setVisible(refreshDecks.isSelected());
      addNewDecks.setVisible(refreshDecks.isSelected());
    }

    protected void setOptions() {
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
      Command command = new NullCommand();
      final String player = GlobalOptions.getInstance().getPlayerId();
      setOptions();
      if (refresher.isTestMode()) {
        refresher.log(Resources.getString("GameRefresher.refresh_counters_test_mode"));
      }
      else {
        // Test refresh does not need to be in the log.
        final Command msg = new Chatter.DisplayText(g.getChatter(), Resources.getString("GameRefresher.run_refresh_counters_v2", player, g.getGameVersion()));
        msg.execute();
        command = command.append(msg);
//FIXME list options in chatter for opponents to see

      }
      refresher.execute(options, command);

      // Send the update to other clients (only done in Player mode)
      g.sendAndLog(command);

      if (options.contains("RefreshDecks") && !refresher.isGameActive()) {
        final BasicLogger log = GameModule.getGameModule().getBasicLogger();
        if (log != null) {
          log.blockUndo(1);
        }
      }

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

  private interface Refresher {
    void refresh(Command command);
    List<GamePiece> getPieces();
    List<GamePiece> getRefreshedPieces();
  }

  /**
   * Class to hold and refresh a Mat and it's cargo
   */
  private class MatRefresher extends MatHolder implements Refresher {

    private final List<Refresher> loadedCargo = new ArrayList<>();

    public MatRefresher(GamePiece piece) {
      super(piece);
    }

    private boolean refresher_for_cargo(Refresher refresher) {
      final GamePiece p = refresher instanceof PieceRefresher ?
        ((PieceRefresher) refresher).getPiece() :
        (refresher instanceof StackRefresher ?
          ((StackRefresher) refresher).getStack().getPieceAt(0) : null);

      return p != null && getMat().hasCargo(p);
    }

    /**
     * Search through the list of Refreshables for any individual pieces, or Stacks that contain pieces,
     * that are loaded onto this Mat. Remove the Refreshable from the supplied List and record it here
     * @param refreshables List of refreshables
     */
    public void grabMyCargo(List<Refresher> refreshables) {
      refreshables.removeIf(refresher -> {
        if (refresher_for_cargo(refresher)) {
          loadedCargo.add(refresher);
          return true;
        }
        return false;
      });
    }

    @Override
    public List<GamePiece> getPieces() {
      return null;
    }

    @Override
    public List<GamePiece> getRefreshedPieces() {
      return null;
    }

    /**
     * Refresh this Mat and it's cargo
     * 1. Remove all cargo from the Mat
     * 2. Refresh the Mat
     * 3. Refresh each Cargo and place back on the Mat
     *
     * @param command
     */
    @Override
    public void refresh(Command command) {
      // Remove any existing cargo
      command = command.append(getMat().makeRemoveAllCargoCommand());

      // Refresh the Mat piece
      final PieceRefresher pr = new PieceRefresher(getMatPiece(), true);
      pr.refresh(command);
      final GamePiece newMatPiece = pr.getRefreshedPieces().get(0);

      // Now refresh each cargo stack or piece
      for (final Refresher r : loadedCargo) {
        r.refresh(command);
      }

      // And add the cargo back onto the mat
      for (final Refresher r : loadedCargo) {
        for (final GamePiece refreshedCargo : r.getRefreshedPieces()) {
          command = command.append(((Mat) Decorator.getDecorator(newMatPiece, Mat.class)).makeAddCargoCommand(refreshedCargo));
        }
      }
    }
  }

  /**
   * Class to refresh a Deck of GamePieces
   */
  private class DeckRefresher implements Refresher {
    private final Deck deck;
    private final List<GamePiece> refreshedPieces = new ArrayList<>();

    public DeckRefresher(Deck deck) {
      this.deck = deck;
    }

    public Deck getDeck() {
      return deck;
    }

    @Override
    public List<GamePiece> getPieces() {
      return deck.asList();
    }

    @Override
    public List<GamePiece> getRefreshedPieces() {
      return refreshedPieces;
    }

    @Override
    public void refresh(Command command) {
      if (isTestMode()) {
        final List<GamePiece> pieces = deck.asList();
        for (final GamePiece piece : pieces) {
          // Create a new, updated piece
          if (gpIdChecker.createUpdatedPiece(piece) == null) {
            notFoundCount++;
            log(Resources.getString("GameRefresher.refresh_error_nomatch_pieceslot", piece.getName(), piece.getId()));
          }
          else {
            updatedCount++;
          }
        }
        return;
      }

      // Prevent any listeners or hot Keys firing while we fiddle the Deck
      deck.removeListeners();
      final boolean saveHotKeyOnEmpty = deck.isHotkeyOnEmpty();
      deck.setHotkeyOnEmpty(false);

      // Take a copy of the pieces in the Deck
      final List<GamePiece> pieces = deck.asList();

      // Remove all the pieces from the Deck  and create a set of fresh, updated pieces
      for (final GamePiece piece : pieces) {

        // Remove the existing piece from the Deck, the Map and the GameState
        final Command remove = new RemovePiece(piece);
        remove.execute();
        command = command.append(remove);

        // Create a new, updated piece
        GamePiece newPiece = gpIdChecker.createUpdatedPiece(piece);
        if (newPiece == null) {
          notFoundCount++;
          log(Resources.getString("GameRefresher.refresh_error_nomatch_pieceslot", piece.getName(), piece.getId()));
          // Could not create a new piece for some reason, use the old piece
          newPiece = piece;
        }
        else {
          updatedCount++;
        }

        // Keep a list of the new pieces to add back into the Deck
        refreshedPieces.add(newPiece);

        // Add the new pieces back into the GameState
        final Command add = new AddPiece(newPiece);
        add.execute();
        command = command.append(add);
      }

      // Load the new pieces back into the Deck in the same order
      for (final GamePiece piece : refreshedPieces) {
        command = command.append(deck.getMap().placeOrMerge(piece, deck.getPosition()));
      }

      deck.addListeners();
      deck.setHotkeyOnEmpty(saveHotKeyOnEmpty);
      deck.getMap().getPieceCollection().moveToFront(deck);
    }
  }

  /**
   * Class to refresh a Stack of GamePieces
   */
  private class StackRefresher implements Refresher {
    private final Stack stack;
    private final List<GamePiece> refreshedPieces = new ArrayList<>();

    public StackRefresher(Stack stack) {
      this.stack = stack;
    }

    public Stack getStack() {
      return stack;
    }

    @Override
    public List<GamePiece> getPieces() {
      return stack.asList();
    }

    @Override
    public List<GamePiece> getRefreshedPieces() {
      return refreshedPieces;
    }

    @Override
    public void refresh(Command command) {

      // Test mode, just try to create a new piece for each visible to me piece in the Stack
      if (isTestMode()) {
        final List<GamePiece> pieces = stack.asList();
        for (final GamePiece piece : pieces) {
          if (!Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
            && !Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
            // Create a new, updated piece
            if (gpIdChecker.createUpdatedPiece(piece) == null) {
              notFoundCount++;
              log(Resources.getString("GameRefresher.refresh_error_nomatch_pieceslot", piece.getName(), piece.getId()));
            }
            else {
              updatedCount++;
            }
          }
        }
        return;
      }

      // Take a copy of the pieces in the Deck
      final List<GamePiece> pieces = stack.asList();

      // Remove all the pieces from the Deck  and create a set of fresh, updated pieces
      for (final GamePiece piece : pieces) {

        // Remove the existing piece from the Deck, the Map and the GameState
        final Command remove = new RemovePiece(piece);
        remove.execute();
        command = command.append(remove);

        GamePiece newPiece;
        if (!Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
          && !Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
          // Create a new, updated piece
          newPiece = gpIdChecker.createUpdatedPiece(piece);
          if (newPiece == null) {
            notFoundCount++;
            log(Resources.getString("GameRefresher.refresh_error_nomatch_pieceslot", piece.getName(), piece.getId()));
            // Could not create a new piece for some reason, use the old piece
            newPiece = piece;
          }
          else {
            updatedCount++;
          }
        }
        else {
          newPiece = piece;
        }

        // Keep a list of the new pieces to add back into the Deck
        refreshedPieces.add(newPiece);

        // Add the new pieces back into the GameState
        final Command add = new AddPiece(newPiece);
        add.execute();
        command = command.append(add);
      }

      // Load the new pieces back into the Stack in the same order
      for (final GamePiece piece : refreshedPieces) {
        command = command.append(stack.getMap().placeOrMerge(piece, stack.getPosition()));
        piece.setMap(stack.getMap());
      }

      stack.getMap().getPieceCollection().moveToFront(stack);
    }
  }

  /**
   * Class to refresh an individual non-stacking piece.
   * If the piece is not refreshable, do not attempt to refresh it.
   */
  private class PieceRefresher implements Refresher {
    private final GamePiece piece;
    private GamePiece refreshedPiece;
    private final boolean refreshable; // Is this piece refreshable by this player?

    public PieceRefresher(GamePiece piece, boolean refreshable) {
      this.piece = piece;
      this.refreshable = refreshable;
    }

    public GamePiece getPiece() {
      return piece;
    }

    public boolean isRefreshable() {
      return refreshable;
    }

    @Override
    public List<GamePiece> getPieces() {
      return List.of(piece);
    }

    @Override
    public List<GamePiece> getRefreshedPieces() {
      return List.of(refreshedPiece);
    }

    @Override
    public void refresh(Command command) {
      final Point position = piece.getPosition();
      final Map map = piece.getMap();
      refreshedPiece = piece;

      if (refreshable) {
        // Test mode, just try to create a new piece for each visible to me piece in the Stack
        if (isTestMode()) {
          if (!Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
            && !Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
            // Create a new, updated piece
            if (gpIdChecker.createUpdatedPiece(piece) == null) {
              notFoundCount++;
              log(Resources.getString("GameRefresher.refresh_error_nomatch_pieceslot", piece.getName(), piece.getId()));
            }
            else {
              updatedCount++;
            }
          }
          return;
        }

        // Remove the existing piece the Map and the GameState
        final Command remove = new RemovePiece(piece);
        remove.execute();
        command = command.append(remove);

        // Refresh it
        if (!Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
          && !Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
          // Create a new, updated piece
          refreshedPiece = gpIdChecker.createUpdatedPiece(piece);
          if (refreshedPiece == null) {
            notFoundCount++;
            log(Resources.getString("GameRefresher.refresh_error_nomatch_pieceslot", piece.getName(), piece.getId()));
            // Could not create a new piece for some reason, use the old piece
            refreshedPiece = piece;
          }
          else {
            updatedCount++;
          }
        }
        else {
          refreshedPiece = piece;
        }

        // Add the new pieces back into the GameState
        final Command add = new AddPiece(refreshedPiece);
        add.execute();
        command = command.append(add);

        // Place the piece back in the correct position
        command = command.append(map.placeOrMerge(refreshedPiece, position));
      }

      map.getPieceCollection().moveToFront(refreshedPiece);

    }
  }
}




