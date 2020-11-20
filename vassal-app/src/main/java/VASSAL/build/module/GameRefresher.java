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

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.command.RemovePiece;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
import VASSAL.tools.BrowserSupport;
import VASSAL.build.IllegalBuildException;
import net.miginfocom.swing.MigLayout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdChecker;
import VASSAL.build.GpIdSupport;
import VASSAL.build.widget.PieceSlot;

import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;


/**
 * GameRefresher Replace all counters in the same game with the current version
 * of the counters defined in the module
 *
 * Note: Counters that are Hidden or Obscured to us cannot be updated.
 *
 */
public final class GameRefresher implements GameComponent {

  private static final Logger logger = LoggerFactory.getLogger(GameRefresher.class);

  private Action refreshAction;
  private final GpIdSupport gpIdSupport;
  private GpIdChecker  gpIdChecker;
  private RefreshDialog dialog;
  private boolean testMode;
  private boolean useLabelerName;
  private int updatedCount;
  private int notFoundCount;
  private GameModule theModule;
  private String player;
  private Chatter chatter;
  private Command msg;

  public GameRefresher(GpIdSupport gpIdSupport) {
    this.gpIdSupport = gpIdSupport;
    theModule = GameModule.getGameModule();
    player = GlobalOptions.getInstance().getPlayerId();
    chatter = theModule.getChatter();
    msg = new Chatter.DisplayText(chatter, "----------"); //$NON-NLS-1$
  }

  public void addTo(AbstractConfigurable parent) {
    refreshAction = new AbstractAction(Resources.getString("GameRefresher.refresh_counters")) { //$NON-NLS-1$
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed(ActionEvent e) {
        new GameRefresher(gpIdSupport).start();
      }
    };
    refreshAction.setEnabled(false);
    GameModule.getGameModule().getGameState().addGameComponent(this);
  }

  public Action getRefreshAction() {
    return refreshAction;
  }

  public boolean isTestMode() {
    return testMode;
  }

  public void start() {
    dialog = new RefreshDialog(this);
    dialog.setVisible(true);
    dialog = null;
  }

 public void log(String message) {
   // ex for dialog msg dialog.addMessage(Resources.getString("GameRefresher.counters_refreshed_test", updatedCount));
   // Log to chatter
   msg.append(new Chatter.DisplayText(chatter, message));
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
           pieces.add(0, i.next());
           totalCount++;
         }
       }
       else if (piece instanceof Stack) {
         for (final Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext();) {
          final GamePiece p = i.next();
          totalCount++;
          if (!Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))
            && !Boolean.TRUE.equals(p.getProperty(Properties.OBSCURED_TO_ME))) {
            pieces.add(0, p);
          }
          else {
            if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME)))
            {
              notVisibleCount++;
            } else {
              notOwnedCount++;
            }
          }
        }
      }
      else if (piece.getParent() == null) {
        if (!Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
          && !Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
          pieces.add(0, piece);
        }
        else {
          if (Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME)))
          {
            notVisibleCount++;
          } else {
            notOwnedCount++;
          }
       }
     }
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
   * @param pieces - list of pieces to be refreshed, if null defaults to all pieces
   * @param useName - tell the gpIdChecker to use the piece name
   * @param pieces - list of pieces to be refreshed, if null defaults to all pieces
   * @throws IllegalBuildException - if we get a gpIdChecker error
   */
  public void execute(boolean testMode,  boolean useName,  boolean useLabelerName, List<GamePiece> pieces) throws IllegalBuildException {
    this.testMode = testMode;
    this.useLabelerName = useLabelerName;
    notFoundCount = 0;
    updatedCount = 0;


    /*
     * 1. Use the GpIdChecker to build a cross-reference of all available
     * PieceSlots and PlaceMarker's in the module.
     */
    gpIdChecker = new GpIdChecker(useName, useLabelerName);
    for (final PieceSlot slot : theModule.getAllDescendantComponentsOf(PieceSlot.class)) {
      gpIdChecker.add(slot);
    }

    // Add any PieceSlots in Prototype Definitions
    for (final PrototypesContainer pc : theModule.getComponentsOf(PrototypesContainer.class)) {
      pc.getDefinitions().forEach(gpIdChecker::add);
    }

    if (gpIdChecker.hasErrors()) {
      // Any errors should have been resolved by the GpId check at startup, so
      // this error indicates
      // a bug in GpIdChecker.fixErrors().
      gpIdChecker = null;
      throw new IllegalBuildException("GameRefresher.executeHeadless: gpIdChecker has errors");
    }

    /*
     * 2. If we haven't been given a list of pieces, we use the default
     */
    if (Objects.isNull(pieces)) {
      pieces = getCurrentGameRefresherPieces();
    }

    /*
     * 3. Generate the commands to update the pieces
     */
    final Command command = new NullCommand();
    for (final GamePiece piece : pieces) {
      if (isTestMode()) {
        testGamePiece(piece);
      }
      else {
        processGamePiece(piece, command);
      }
    }
   /* if (isTestMode()) {
      if (dialog != null) { // Could be null if dialog box got closed while we were still running - see start() method above
        dialog.addMessage(Resources.getString("GameRefresher.counters_refreshed", updatedCount));
        if (notFoundCount > 0) {
          dialog.addMessage(Resources.getString("GameRefresher.counters_not_found", notFoundCount));
        }
      }
    }
    else {*/

      log(Resources.getString("GameRefresher.run_refresh_counters"));
      log(Resources.getString("GameRefresher.counters_refreshed", updatedCount));
      log(Resources.getString("GameRefresher.counters_not_found", notFoundCount));
      log("----------"); //$NON-NLS-1$
      // msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.run_refresh_counters", player)));
      // msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_refreshed", player, updatedCount)));

      //if (notFoundCount > 0) {
      //  msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_not_found", player, notFoundCount)));
      //}

      //msg.append(new Chatter.DisplayText(chatter, "----------")); //NON-NLS
      msg.execute();
      command.append(msg);

      // Send the update to other clients
      theModule.sendAndLog(command);
    //}

    //logger.info("Refreshed pieces: " + updatedCount + ", Not found: " + notFoundCount); //NON-NLS
    gpIdChecker = null;
  }

/*
  public void execute(boolean testMode, boolean useName, boolean useLabelerName) {
    this.testMode = testMode;
    this.useLabelerName = useLabelerName;

    final GameModule theModule = GameModule.getGameModule();
    updatedCount = 0;
    notFoundCount = 0;
    notOwnedCount = 0;
    */
/*
     * 1. Use the GpIdChecker to build a cross-reference of all available
     * PieceSlots and PlaceMarker's in the module.
     *//*

    gpIdChecker = new GpIdChecker(useName, useLabelerName);
    for (final PieceSlot slot : theModule.getAllDescendantComponentsOf(PieceSlot.class)) {
      gpIdChecker.add(slot);
    }

    // Add any PieceSlots in Prototype Definitions
    for (final PrototypesContainer pc : theModule.getComponentsOf(PrototypesContainer.class)) {
      pc.getDefinitions().forEach(gpIdChecker::add);
    }

    if (gpIdChecker.hasErrors()) {
      // Any errors should have been resolved by the GpId check at startup, so
      // this error indicates
      // a bug in GpIdChecker.fixErrors().
      ErrorDialog.show("GameRefresher.no_gpids"); //$NON-NLS-1$
      gpIdChecker = null;
      return;
    }

    */
/*
     * 2. Make a list of all pieces in the game that we have access to
     *//*

    final Command command = new NullCommand();
    final ArrayList<GamePiece> pieces = new ArrayList<>();

    for (final GamePiece piece : theModule.getGameState().getAllPieces()) {
      if (piece instanceof Deck) {
        for (final Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext();) {
          pieces.add(0, i.next());
        }
      }
      else if (piece instanceof Stack) {
        for (final Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext();) {
          final GamePiece p = i.next();
          if (!Boolean.TRUE.equals(p.getProperty(Properties.INVISIBLE_TO_ME))
            && !Boolean.TRUE.equals(p.getProperty(Properties.OBSCURED_TO_ME))) {
            pieces.add(0, p);
          }
          else {
            notOwnedCount++;
          }
        }
      }
      else if (piece.getParent() == null) {
        if (!Boolean.TRUE.equals(piece.getProperty(Properties.INVISIBLE_TO_ME))
          && !Boolean.TRUE.equals(piece.getProperty(Properties.OBSCURED_TO_ME))) {
          pieces.add(0, piece);
        }
        else {
          notOwnedCount++;
        }
      }
    }

    */
/*
     * 3. Generate the commands to update the pieces
     *//*

    for (final GamePiece piece : pieces) {
      if (isTestMode()) {
        testGamePiece(piece);
      }
      else {
        processGamePiece(piece, command);
      }
    }

    if (isTestMode()) {
      if (dialog != null) { // Could be null if dialog box got closed while we were still running - see start() method above
        dialog.addMessage(Resources.getString("GameRefresher.counters_refreshed_test", updatedCount));
        if (notOwnedCount > 0) {
          dialog.addMessage(Resources.getString("GameRefresher.counters_not_owned_test", notOwnedCount));
        }
        if (notFoundCount > 0) {
          dialog.addMessage(Resources.getString("GameRefresher.counters_not_found_test", notFoundCount));
        }
      }
    }
    else {
      final String player = GlobalOptions.getInstance().getPlayerId();
      final Chatter chatter = theModule.getChatter();
      final Command msg = new Chatter.DisplayText(chatter, "----------"); //NON-NLS
      msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.run_refresh_counters", player)));
      msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_refreshed", player, updatedCount)));

      if (notOwnedCount > 0) {
        msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_not_owned", player, notOwnedCount)));
      }

      if (notFoundCount > 0) {
        msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_not_found", player, notFoundCount)));
      }
      msg.append(new Chatter.DisplayText(chatter, "----------")); //NON-NLS
      msg.execute();
      command.append(msg);

      // Send the update to other clients
      theModule.sendAndLog(command);
    }

    gpIdChecker = null;
  }
*/

  private void processGamePiece(GamePiece piece, Command command) {

    final Map map = piece.getMap();
    if (map == null) {
      logger.error("Can't refresh piece " + piece.getName() + ": No Map"); //NON-NLS
      return;
    }

    final Point pos = piece.getPosition();
    final GamePiece newPiece = gpIdChecker.createUpdatedPiece(piece);

    final Stack oldStack = piece.getParent();
    final int oldPos = oldStack == null ? 0 : oldStack.indexOf(piece);

    // Remove the old Piece if different
    if (piece.equals(newPiece)) {
      notFoundCount++;
      logger.error("Can't refresh piece " + piece.getName() + ": Can't find matching Piece Slot"); //NON-NLS
    }
    else {
      updatedCount++;

      if (! isTestMode()) {
        // Place the new Piece.
        final Command place = map.placeOrMerge(newPiece, pos);
        command.append(place);

        // Delete the old piece
        final Command remove = new RemovePiece(Decorator.getOutermost(piece));
        remove.execute();
        command.append(remove);
      }
    }

    if (! isTestMode()) {
      // If still in the same stack, move to correct position
      final Stack newStack = newPiece.getParent();
      if ((newStack != null) && (newStack == oldStack)) {
        final int newPos = newStack.indexOf(newPiece);
        if (newPos >= 0 && oldPos >= 0 && newPos != oldPos) {
          final String oldState = newStack.getState();
          newStack.insert(newPiece, oldPos);
          command.append(new ChangePiece(newStack.getId(), oldState, newStack.getState()));
        }
      }
    }
  }

  private void testGamePiece(GamePiece piece) {

    final Map map = piece.getMap();
    if (map == null) {
      logger.error("Can't refresh piece " + piece.getName() + ": No Map"); //NON-NLS
      return;
    }

    if (gpIdChecker.findUpdatedPiece(piece)) {
      updatedCount++;
    }
    else {
      notFoundCount++;
      logger.error("Can't refresh piece " + piece.getName() + ": Can't find matching Piece Slot"); //NON-NLS
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
    private JCheckBox labelerNameCheck;

    RefreshDialog(GameRefresher refresher) {
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
      setLayout(new MigLayout("wrap 1", "[center]")); //NON-NLS

      final JPanel buttonPanel = new JPanel(new MigLayout());

      final JButton testButton = new JButton(Resources.getString("General.test"));
      testButton.addActionListener(e -> test());

      final JButton runButton = new JButton(Resources.getString("General.run"));
      runButton.addActionListener(e -> run());

      final JButton exitButton = new JButton(Resources.getString("General.exit"));
      exitButton.addActionListener(e -> exit());

      final JButton helpButton = new JButton(Resources.getString("General.help"));
      helpButton.addActionListener(e -> help());

      buttonPanel.add(testButton);
      buttonPanel.add(runButton);
      buttonPanel.add(exitButton);
      buttonPanel.add(helpButton);

      add(buttonPanel);

      results = new JTextArea(7, 40);
      results.setEditable(false);
      add(results);

      nameCheck = new JCheckBox(Resources.getString("GameRefresher.use_basic_name"));
      add(nameCheck);
      labelerNameCheck = new JCheckBox(Resources.getString("GameRefresher.use_labeler_descr"));
      add(labelerNameCheck);

      pack();
    }

    protected void exit() {
      setVisible(false);
    }

    protected void test() {
      results.setText(Resources.getString("GameRefresher.refresh_counters_test"));
      refresher.execute(true, nameCheck.isSelected(), labelerNameCheck.isSelected(), null);
    }

    protected void run() {
      results.setText("");
      refresher.execute(false, nameCheck.isSelected(), labelerNameCheck.isSelected(), null);
      exit();
    }

    protected void help() {
      File dir = VASSAL.build.module.Documentation.getDocumentationBaseDir();
      dir = new File(dir, "ReferenceManual"); //$NON-NLS-1$
      final File theFile = new File(dir, "HelpMenu.html"); //$NON-NLS-1$
      HelpFile h = null;
      try {
        h = new HelpFile(null, theFile, "#HelpFile"); //$NON-NLS-1$
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
}
