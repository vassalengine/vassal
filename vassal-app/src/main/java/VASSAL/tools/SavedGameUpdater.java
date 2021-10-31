/*
 *
 * Copyright (c) 2005 by Rodney Kinney
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
package VASSAL.tools;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.Command;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.Replace;
import VASSAL.counters.Stack;
import VASSAL.i18n.Resources;

public class SavedGameUpdater {
  /**
   * Returns a mapping of GamePiece type to the id of a PieceSlot in the module
   * This information is exported from an old module version, then imported into a new module version to update saved games
   * @return Returns a mapping of GamePiece type to the id of a PieceSlot in the module
   */
  public Properties getPieceSlotsMap() {
    final Properties p = new Properties();
    final ArrayList<Configurable> l = new ArrayList<>();
    findPieceSlots(l, p);
    return p;
  }

  /**
   *
   * @param pieceSlot the imported piece-slot map from an earlier version of the module
   * @param savedGame the save game to update.  The file gets overwritten.
   * @throws IOException oops
   */
  public void updateSavedGame(Properties pieceSlot, File savedGame)
                                                           throws IOException {
    final GameState gs = GameModule.getGameModule().getGameState();

    gs.setup(false, true);
    //gs.loadGameInBackground(savedGame);
    gs.loadGameInForeground(savedGame);

    /*
    // FIXME: spin locks are bad, wait on a Future instead
    while (!gs.isGameStarted()) {
      try {
        Thread.sleep(100);
      }
      catch (InterruptedException e) {
      }
    }
    */

    final GamePiece[] gp_array = gs.getAllPieces().toArray(new GamePiece[0]);
    for (final GamePiece p : gp_array) {
      if (!(p instanceof Stack)) {
        final String slotId = pieceSlot.getProperty(p.getType());
        if (slotId != null) {
          Configurable[] path = null;
          try {
            path = ComponentPathBuilder.getInstance().getPath(slotId);
            if (path != null &&
                path.length > 0 &&
                path[path.length - 1] instanceof PieceSlot) {
              final PieceSlot slot = (PieceSlot) path[path.length - 1];
              if (!slot.getPiece().getType().equals(p.getType())) {
                if (!(p instanceof Decorator)) {
                  GameModule.getGameModule().getChatter().show(Resources.getString("Editor.SavedGameUpdater.basic_only", p.getName()));
                }
                else {
                  final ReplaceTrait r = new ReplaceTrait(p, slot.getPiece());
                  r.replacePiece();
                }
              }
            }
          }
          // FIXME: review error message
          catch (ComponentPathBuilder.PathFormatException ex) {
            GameModule.getGameModule().getChatter().show(Resources.getString("Editor.SavedGameUpdater.unable", p.getName(), ex.getMessage()));
          }
        }
        else {
          GameModule.getGameModule().getChatter().show(Resources.getString("Editor.SavedGameUpdater.no_slot", p.getName()));
          GameModule.getGameModule().getChatter().show(p.getType());
        }
      }
    }

    gs.saveGame(savedGame);
    gs.updateDone();
  }

  protected void findPieceSlots(List<Configurable> l, Properties p) {
    final Configurable last = l.isEmpty() ?
      GameModule.getGameModule() : l.get(l.size() - 1);

    if (last instanceof PieceSlot) {
      final PieceSlot slot = (PieceSlot) last;

      // Resolve prototypes
      final GamePiece clone =
        PieceCloner.getInstance().clonePiece(slot.getPiece());

      p.setProperty(clone.getType(),
        ComponentPathBuilder.getInstance().getId(
          l.toArray(new Configurable[0])));
    }
    else {
      final Configurable[] children = last.getConfigureComponents();
      for (final Configurable child : children) {
        l.add(child);
        findPieceSlots(l, p);
        l.remove(child);
      }
    }
  }

  private static class ReplaceTrait extends Replace {
    private final GamePiece replacement;

    public ReplaceTrait(GamePiece original, GamePiece replacement) {
      super(Replace.ID + "Replace;R;dummy;;0;0;true", original); //NON-NLS
      setProperty(VASSAL.counters.Properties.OUTER, original);
      original.setProperty(VASSAL.counters.Properties.OUTER, null);
      this.replacement = replacement;
    }

    @Override
    public GamePiece createMarker() {
      final GamePiece marker = PieceCloner.getInstance().clonePiece(replacement);
      if (matchRotation) {
        matchTraits(getInner(), marker);
      }
      return marker;
    }

    @Override
    public Command replacePiece() {
      return super.replacePiece();
    }
  }
}
