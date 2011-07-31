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

import javax.swing.KeyStroke;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;

/**
 * GamePiece trait that replaces a GamePiece with another one
 */
public class Replace extends PlaceMarker {
  public static final String ID = "replace;";

  public Replace() {
    this(ID + "Replace;R;null", null);
  }

  public Replace(String type, GamePiece inner) {
    super(type, inner);
  }

  public Command myKeyEvent(KeyStroke stroke) {
    Command c = null;
    if (command.matches(stroke)) {
      c = replacePiece();
    }
    return c;
  }

  protected Command replacePiece() {
    Command c;
    c = placeMarker();
    if (c == null) {
      reportDataError(this, Resources.getString("Error.bad_replace"));
    }
    else {
      Command remove = new RemovePiece(Decorator.getOutermost(this));
      remove.execute();
      c.append(remove);
    }
    return c;
  }

  protected void selectMarker(GamePiece marker) {
    KeyBuffer.getBuffer().add(marker);
  }

  public String getDescription() {
    String d = "Replace with Other";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Replace.htm");
  }

  public String myGetType() {
    return ID + super.myGetType().substring(PlaceMarker.ID.length());
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public GamePiece createMarker() {
    GamePiece marker = super.createMarker();
    if (marker != null && matchRotation) {
      if (above) {
        matchTraits(this, marker);
      }
      else {
        matchTraits(Decorator.getOutermost(this), marker);
      }
    }
    return marker;
  }

  protected void matchTraits(GamePiece base, GamePiece marker) {
    if (!(base instanceof Decorator)
        || !(marker instanceof Decorator)) {
      return;
    }
    Decorator currentTrait = (Decorator) base;
    Decorator lastMatch = (Decorator) marker;
    while (currentTrait != null) {
      Decorator candidate = lastMatch;
      while (candidate != null) {
        candidate = (Decorator) Decorator.getDecorator(candidate, currentTrait.getClass());
        if (candidate != null) {
          if (candidate.myGetType().equals(currentTrait.myGetType())) {
            candidate.mySetState(currentTrait.myGetState());
            lastMatch = candidate;
            candidate = null;
          }
          else {
            GamePiece inner = candidate.getInner();
            if (inner instanceof Decorator) {
              candidate = (Decorator) inner;
            }
            else {
              candidate = null;
            }
          }
        }
      }
      if (currentTrait.getInner() instanceof Decorator) {
        currentTrait = (Decorator) currentTrait.getInner();
      }
      else {
        currentTrait = null;
      }
    }
  }

  public PieceI18nData getI18nData() {
    return getI18nData(command.getName(), getCommandDescription(description, "Replace command"));
  }

  protected static class Ed extends PlaceMarker.Ed {

    public Ed(Replace piece) {
      super(piece);
      defineButton.setText("Define Replacement");
    }

    protected BooleanConfigurer createMatchRotationConfig() {
      return new BooleanConfigurer(null, "Match Current State?");
    }

    protected BooleanConfigurer createAboveConfig() {
      return new BooleanConfigurer(null, "Only match states above this trait?");
    }

    public String getType() {
      return ID + super.getType().substring(PlaceMarker.ID.length());
    }
  }
}
