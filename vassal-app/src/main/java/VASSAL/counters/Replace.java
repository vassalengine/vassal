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
import java.util.ArrayList;
import java.util.List;

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
    c = placeMarker(true);
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

      Command remove = GameModule.getGameModule().getGameState().getAttachmentManager().removeAttachments(this);
      remove = remove.append(new RemovePiece(Decorator.getOutermost(this)));
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
    String s = buildDescription("Editor.Replace.trait_description", description);
    s += getCommandDesc(command.getName(), key);

    updateDescString();

    if ((descString != null) && !descString.isEmpty()) {
      s += " - " + descString;
    }

    return s;
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.Replace.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return description;
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

  /**
   * Match trait states from the supplied base Decorator to the marker
   *  - Markers are never matched
   *  - Dynamic Properties are matched on Name only
   *  - All other traits matched on full Type
   *
   * @param base    Decorator in Base marker to start matching
   * @param marker  Marker to set matched values into
   *
   */
  protected void matchTraits(GamePiece base, GamePiece marker) {
    if (!(base instanceof Decorator) || !(marker instanceof Decorator)) {
      return;
    }

    // Process each trait in the Replacement
    Decorator currentMarker = (Decorator) marker;
    while (currentMarker != null) {

      // Markers are never state matched
      if (! (currentMarker instanceof Marker)) {

        // Search for matching traits in the source piece, starting at the supplied trait
        Decorator candidate = (Decorator) base;
        while (candidate != null) {

          // Find the next trait of the same type as the Replacement trait we are working on
          candidate = (Decorator) Decorator.getDecorator(candidate, currentMarker.getClass());
          if (candidate != null) {

            // Match DP's on property name only if Copy by name option is selected
            if (candidate instanceof DynamicProperty && copyDPsByName) {
              if (((DynamicProperty) candidate).getKey().equals(((DynamicProperty) currentMarker).getKey())) {
                currentMarker.mySetState(candidate.myGetState());
                candidate = null;
              }
            }
            // Labels are only state matched if they are adjustable. Note this matches the behaviour of the Game Refresher
            else if (currentMarker instanceof Labeler && candidate.myGetType().equals(currentMarker.myGetType())) {
              if (((Labeler) currentMarker).canChange()) {
                currentMarker.mySetState(candidate.myGetState());
                candidate = null;
              }
            }
            // Match all other Decorators on full type
            else {
              if (candidate.myGetType().equals(currentMarker.myGetType())) {
                currentMarker.mySetState(candidate.myGetState());
                candidate = null;
              }
            }

            // Repeat from next inner Decorator in the base
            if (candidate != null && candidate.getInner() instanceof Decorator) {
              candidate = (Decorator) candidate.getInner();
            }
            else {
              candidate = null;
            }
          }
        }
      }

      // Process the next inner Decorator of the target marker
      if (currentMarker.getInner() instanceof Decorator) {
        currentMarker = (Decorator) currentMarker.getInner();
      }
      else {
        currentMarker = null;
      }
    }
  }

  @Override
  public List<String> getPropertyNames() {
    return new ArrayList<>();
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(command.getName(), getCommandDescription(description, Resources.getString("Editor.Replace.replace_command")));
  }

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
    protected BooleanConfigurer createCopyConfig() {
      return new BooleanConfigurer(null, Resources.getString("Editor.Replace.copy_dps_by_name"));
    }

    @Override
    public String getType() {
      return ID + super.getType().substring(PlaceMarker.ID.length());
    }
  }
}
