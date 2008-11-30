/*
 * $Id$
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
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.Command;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceCloner;
import VASSAL.counters.Replace;
import VASSAL.counters.Stack;

public class SavedGameUpdater {
  /**
   * Returns a mapping of GamePiece type to the id of a PieceSlot in the module
   * This information is exported from an old module version, then imported into a new module version to update saved games
   * @return
   */
  public Properties getPieceSlotsMap() {
    Properties p = new Properties();
    ArrayList<Configurable> l = new ArrayList<Configurable>();
    findPieceSlots(l, p);
    return p;
  }

  /**
   *
   * @param pieceSlot the imported piece-slot map from an earlier version of the module
   * @param savedGame the save game to update.  The file gets overwritten.
   * @throws IOException
   */
  public void updateSavedGame(Properties pieceSlot, File savedGame) throws IOException {
    GameModule.getGameModule().getGameState().setup(false);
    GameModule.getGameModule().getGameState().loadGameInBackground(savedGame);
    while (!GameModule.getGameModule().getGameState().isGameStarted()) {
      try {
        Thread.sleep(100);
      }
      catch (InterruptedException e) {
      }
    }
    
    GamePiece[] gp_array = GameModule.getGameModule().getGameState().getAllPieces().toArray(new GamePiece[0]);
    for (GamePiece p : gp_array) {
      if (!(p instanceof Stack)) {
        String slotId = pieceSlot.getProperty(p.getType());
        if (slotId != null) {
          Configurable[] path = null;
          try {
            path = ComponentPathBuilder.getInstance().getPath(slotId);
            if (path != null &&
                path.length > 0 &&
                path[path.length - 1] instanceof PieceSlot) {
              final PieceSlot slot = (PieceSlot) path[path.length - 1];
              if (!slot.getPiece().getType().equals(p.getType())) {
                ReplaceTrait r = new ReplaceTrait(p,slot.getPiece());
                r.replacePiece();
              }
            }
          }
          // FIXME: review error message
          catch (ComponentPathBuilder.PathFormatException ex) {
            System.err.println("Unable to replace " + p.getName() + ": " + ex.getMessage());
          }
        }
        else {
          System.err.println("Unable to find slot for " + p.getName());
          System.out.println(p.getType());
        }
      }
    }
    GameModule.getGameModule().getGameState().saveGame(savedGame);
  }

  protected void findPieceSlots(List<Configurable> l, Properties p) {
    Object last = l.size() == 0 ?
                  GameModule.getGameModule() : l.get(l.size() - 1);

    if (last instanceof PieceSlot) {
      PieceSlot slot = (PieceSlot) last;
      GamePiece clone = PieceCloner.getInstance().clonePiece(slot.getPiece()); // Resolve prototypes
      p.setProperty(clone.getType(),
        ComponentPathBuilder.getInstance().getId(
          l.toArray(new Configurable[l.size()])));
    }
    else if (last instanceof Configurable) {
      Configurable c = (Configurable) last;
      Configurable[] children = c.getConfigureComponents();
      for (int i = 0; i < children.length; ++i) {
        l.add(children[i]);
        findPieceSlots(l, p);
        l.remove(children[i]);
      }
    }
  }

  private static class ReplaceTrait extends Replace {
    private GamePiece replacement;

    public ReplaceTrait(GamePiece original, GamePiece replacement) {
      super(Replace.ID + "Replace;R;dummy;;0;0;true", original);
      setProperty(VASSAL.counters.Properties.OUTER, original);
      original.setProperty(VASSAL.counters.Properties.OUTER, null);
      this.replacement = replacement;
    }

    public GamePiece createMarker() {
      GamePiece marker = PieceCloner.getInstance().clonePiece(replacement);
      if (matchRotation) {
        matchTraits(getInner(), marker);
      }
      return marker;
    }

    public Command replacePiece() {
      return super.replacePiece();
    }
  }
}
