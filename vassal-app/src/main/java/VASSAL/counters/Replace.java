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
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;

import javax.swing.KeyStroke;

/**
 * GamePiece trait that replaces a GamePiece with another one
 */
public class Replace extends PlaceMarker {
  public static final String ID = "replace;"; // NON-NLS

  public Replace() {
    this(ID + Resources.getString("Editor.Replace.default_command") + ";R;null", null); // NON-NLS
  }

  public Replace(String type, GamePiece inner) {
    super(type, inner);
  }

  @Override
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
      if (GameModule.getGameModule().isMatSupport()) {
        final GamePiece outer = Decorator.getOutermost(this);

        // If a cargo piece has been deleted remove it from any mat
        if (Boolean.TRUE.equals(outer.getProperty(MatCargo.IS_CARGO))) { //NON-NLS
          final MatCargo cargo = (MatCargo) Decorator.getDecorator(outer, MatCargo.class);
          if (cargo != null) {
            c = c.append(cargo.makeClearMatCommand());
          }
        }

        // If a mat has been deleted remove any cargo from it
        final String matName = (String)outer.getProperty(Mat.MAT_NAME);
        if (matName != null && !"".equals(matName)) {
          final Mat mat = (Mat) Decorator.getDecorator(outer, Mat.class);
          if (mat != null) {
            c = c.append(mat.makeRemoveAllCargoCommand());
          }
        }
      }

      final Command remove = new RemovePiece(Decorator.getOutermost(this));
      remove.execute();
      c.append(remove);
    }
    return c;
  }

  @Override
  protected void selectMarker(GamePiece marker) {
    KeyBuffer.getBuffer().add(marker);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.Replace.trait_description", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Replace.html"); // NON-NLS
  }

  @Override
  public String myGetType() {
    return ID + super.myGetType().substring(PlaceMarker.ID.length());
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public GamePiece createMarker() {
    final GamePiece marker = super.createMarker();
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
            final GamePiece inner = candidate.getInner();
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

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(command.getName(), getCommandDescription(description, Resources.getString("Editor.Replace.replace_command")));
  }

  //@Override
  //public boolean testEquals(Object o) {
  //  return super.testEquals(o);
  //}

  protected static class Ed extends PlaceMarker.Ed {

    public Ed(Replace piece) {
      super(piece);
      defineButton.setText(Resources.getString("Editor.Replace.define_replacement"));
    }

    @Override
    protected BooleanConfigurer createMatchRotationConfig() {
      return new BooleanConfigurer(null, Resources.getString("Editor.Replace.match_current_state"));
    }

    @Override
    protected BooleanConfigurer createAboveConfig() {
      return new BooleanConfigurer(null, Resources.getString("Editor.Replace.only_match_above"));
    }

    @Override
    public String getType() {
      return ID + super.getType().substring(PlaceMarker.ID.length());
    }
  }
}
