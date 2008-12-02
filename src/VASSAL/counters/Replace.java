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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.command.RemovePiece;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.tools.SequenceEncoder;

/**
 * GamePiece trait that replaces a GamePiece with another one
 */
public class Replace extends PlaceMarker {
  public static final String ID = "replace;";
  protected boolean above;

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
    Command remove = new RemovePiece(Decorator.getOutermost(this));
    remove.execute();
    c.append(remove);
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

  public void mySetType(String type) {
	  SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
	  st.nextToken();
	  String name = st.nextToken();
	  key = st.nextKeyStroke(null);
	  command = new KeyCommand(name, key, this, this);
	  if (name.length() > 0 && key != null) {
		  commands = new KeyCommand[]{command};
	  }
	  else {
		  commands = new KeyCommand[0];
	  }
	  markerSpec = st.nextToken();
	  if ("null".equals(markerSpec)) {
		  markerSpec = null;
	  }
	  markerText = st.nextToken("null");
	  if ("null".equals(markerText)) {
		  markerText = null;
	  }
	  xOffset = st.nextInt(0);
	  yOffset = st.nextInt(0);
	  matchRotation = st.nextBoolean(false);
	  afterBurnerKey = st.nextKeyStroke(null);
	  description = st.nextToken("");
	  setGpId(st.nextToken(""));
	  above = st.nextBoolean(false);
	  gpidSupport = GameModule.getGameModule().getGpIdSupport();
  }

  
  public String myGetType() {
    return ID + super.myGetType().substring(PlaceMarker.ID.length()) + ";" + String.valueOf(above);
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
	  protected BooleanConfigurer aboveConfig = new BooleanConfigurer(null, "Only match states above this trait?");
	  
    public Ed(Replace piece) {
      super(piece);
      defineButton.setText("Define Replacement");
      JPanel p = (JPanel) getControls();
      p.add(aboveConfig.getControls(), 7);
      aboveConfig.setValue(Boolean.valueOf(piece.above));
      ((JCheckBox) matchRotationConfig.getControls()).addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			aboveConfig.getControls().setVisible(((JCheckBox) matchRotationConfig.getControls()).isSelected());
		}
      });
    }

    protected BooleanConfigurer createMatchRotationConfig() {
      return new BooleanConfigurer(null, "Match Current State?");
    }

    public String getType() {
      String s = super.getType();
      s = ID + s.substring(PlaceMarker.ID.length()) + ";" + aboveConfig.getValueString();
      return s;
    }
  }
}
