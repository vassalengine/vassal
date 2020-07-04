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
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.WindowConstants;

import net.miginfocom.swing.MigLayout;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.GameModule;
import VASSAL.build.GpIdChecker;
import VASSAL.build.GpIdSupport;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.command.RemovePiece;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Properties;
import VASSAL.counters.Stack;
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
  protected GpIdSupport gpIdSupport;
  protected GpIdChecker gpIdChecker;
  protected int updatedCount;
  protected int notFoundCount;
  protected int notOwnedCount;
  protected RefreshDialog dialog;
  protected boolean testMode;

  public GameRefresher(GpIdSupport gpIdSupport) {
    this.gpIdSupport = gpIdSupport;
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

  public void execute(boolean testMode, boolean useName) {
    this.testMode = testMode;

    final GameModule theModule = GameModule.getGameModule();
    updatedCount = 0;
    notFoundCount = 0;
    notOwnedCount = 0;
    /*
     * 1. Use the GpIdChecker to build a cross-reference of all available
     * PieceSlots and PlaceMarker's in the module.
     */
    gpIdChecker = new GpIdChecker(useName);
    for (PieceSlot slot : theModule.getAllDescendantComponentsOf(PieceSlot.class)) {
      gpIdChecker.add(slot);
    }
    if (gpIdChecker.hasErrors()) {
      // Any errors should have been resolved by the GpId check at startup, so
      // this error indicates
      // a bug in GpIdChecker.fixErrors().
      ErrorDialog.show("GameRefresher.no_gpids"); //$NON-NLS-1$
      gpIdChecker = null;
      return;
    }

    /*
     * 2. Make a list of all pieces in the game that we have access to
     */
    final Command command = new NullCommand();
    final ArrayList<GamePiece> pieces = new ArrayList<>();

    for (GamePiece piece : theModule.getGameState().getAllPieces()) {
      if (piece instanceof Deck) {
        for (Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext();) {
          pieces.add(0, i.next());
        }
      }
      else if (piece instanceof Stack) {
        for (Iterator<GamePiece> i = ((Stack) piece).getPiecesInVisibleOrderIterator(); i.hasNext();) {
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

    /*
     * 3. Generate the commands to update the pieces
     */
    for (GamePiece piece : pieces) {
      if (isTestMode()) {
        testGamePiece(piece);
      }
      else {
        processGamePiece(piece, command);
      }
    }

    if (isTestMode()) {
      dialog.addMessage(Resources.getString("GameRefresher.counters_refreshed_test", updatedCount));
      if (notOwnedCount > 0) {
        dialog.addMessage(Resources.getString("GameRefresher.counters_not_owned_test", notOwnedCount));
      }
      if (notFoundCount > 0) {
        dialog.addMessage(Resources.getString("GameRefresher.counters_not_found_test", notFoundCount));
      }
    }
    else {
      final String player = GlobalOptions.getInstance().getPlayerId();
      final Chatter chatter = theModule.getChatter();
      final Command msg = new Chatter.DisplayText(chatter, "----------");
      msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.run_refresh_counters", player)));
      msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_refreshed", player, updatedCount)));

      if (notOwnedCount > 0) {
        msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_not_owned", player, notOwnedCount)));
      }

      if (notFoundCount > 0) {
        msg.append(new Chatter.DisplayText(chatter, Resources.getString("GameRefresher.counters_not_found", player, notFoundCount)));
      }
      msg.append(new Chatter.DisplayText(chatter, "----------"));
      msg.execute();
      command.append(msg);

      // Send the update to other clients
      theModule.sendAndLog(command);
    }

    gpIdChecker = null;
  }

  private void processGamePiece(GamePiece piece, Command command) {

    final Map map = piece.getMap();
    if (map == null) {
      logger.error("Can't refresh piece " + piece.getName() + ": No Map");
      return;
    }

    final Point pos = piece.getPosition();
    GamePiece newPiece = gpIdChecker.createUpdatedPiece(piece);

    final Stack oldStack = piece.getParent();
    final int oldPos = oldStack == null ? 0 : oldStack.indexOf(piece);

    // Remove the old Piece if different
    if (piece.equals(newPiece)) {
      notFoundCount++;
      logger.error("Can't refresh piece " + piece.getName() + ": Can't find matching Piece Slot");
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
      if (newStack != null && oldStack != null && newStack == oldStack) {
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
      logger.error("Can't refresh piece " + piece.getName() + ": No Map");
      return;
    }

    if (gpIdChecker.findUpdatedPiece(piece)) {
      updatedCount++;
    }
    else {
      notFoundCount++;
      logger.error("Can't refresh piece " + piece.getName() + ": Can't find matching Piece Slot");
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

  class RefreshDialog extends JDialog {
    private static final long serialVersionUID = 1L;
    private GameRefresher refresher;
    private JTextArea results;
    private JCheckBox nameCheck;

    RefreshDialog (GameRefresher refresher) {
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
      setLayout(new MigLayout("wrap 1","[center]"));

      final JPanel buttonPanel = new JPanel(new MigLayout());

      final JButton testButton = new JButton(Resources.getString("General.test"));
      testButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          test();
        }});

      final JButton runButton = new JButton(Resources.getString("General.run"));
      runButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          run();
        }});

      final JButton exitButton = new JButton(Resources.getString("General.exit"));
      exitButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          exit();
        }});

      buttonPanel.add(testButton);
      buttonPanel.add(runButton);
      buttonPanel.add(exitButton);

      add(buttonPanel);

      results = new JTextArea(7, 40);
      results.setEditable(false);
      add(results);

      nameCheck = new JCheckBox(Resources.getString("GameRefresher.use_basic_name"));
      add(nameCheck);

      pack();
    }

    protected void exit() {
      setVisible(false);
    }

    protected void test() {
      results.setText(Resources.getString("GameRefresher.refresh_counters_test"));
      refresher.execute (true, nameCheck.isSelected());
    }

    protected void run() {
      results.setText("");
      refresher.execute (false, nameCheck.isSelected());
      exit();
    }

    public void addMessage(String mess) {
      results.setText(results.getText()+"\n"+mess);
    }
  }
}