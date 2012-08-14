/*
 * $Id$
 *
 * Copyright (c) 2004 by Rodney Kinney
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

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.KeyStroke;

import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.PrototypesContainer;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.RecursionLimitException;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.RecursionLimiter.Loopable;
import VASSAL.tools.SequenceEncoder;

/**
 * This trait is a placeholder for a pre-defined series of traits specified in a
 * {@link VASSAL.build.module.PrototypeDefinition} object. When a piece that uses a prototype is defined in a module, it
 * is simply assigned the name of a particular prototype definition. When that piece is during a game, the UsePrototype
 * trait is substituted for the list of traits in the prototype definition. From that point on, the piece has no record
 * that those traits were defined in a prototype instead of assigned to piece directly. This is necessary so that
 * subsequent changes to a prototype definition don't invalidate games that were saved using previous versions of the
 * module.
 *
 */
public class UsePrototype extends Decorator implements EditablePiece, Loopable {
  public static final String ID = "prototype;";
  private String prototypeName;
  private String lastCachedPrototype;
  private GamePiece prototype;
  private PropertySource properties;
  private String type;

  public UsePrototype() {
    this(ID, null);
  }

  public UsePrototype(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  public String getDescription() {
    return prototypeName != null && prototypeName.length() > 0 ? "Prototype - " + prototypeName : "Prototype";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("UsePrototype.htm");
  }

  public void mySetType(String type) {
    this.type = type;
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type.substring(ID.length()), ';');
    prototypeName = st.nextToken("");
    if (st.hasMoreTokens()) {
      final java.util.Properties p = new java.util.Properties();
      SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(st.nextToken(), ',');
      while (st2.hasMoreTokens()) {
        SequenceEncoder.Decoder st3 = new SequenceEncoder.Decoder(st2.nextToken(), '=');
        if (st3.hasMoreTokens()) {
          String key = st3.nextToken();
          if (st3.hasMoreTokens()) {
            String value = st3.nextToken();
            p.setProperty(key, value);
          }
        }
      }
      properties = new PropertySource() {
        public Object getProperty(Object key) {
          return p.getProperty(String.valueOf(key));
        }

        public Object getLocalizedProperty(Object key) {
          return getProperty(key);
        }
      };
    }
    lastCachedPrototype = null;
  }

  protected KeyCommand[] myGetKeyCommands() {
    return new KeyCommand[0];
  }

  protected KeyCommand[] getKeyCommands() {
    return (KeyCommand[]) getExpandedInner().getProperty(Properties.KEY_COMMANDS);
  }

  public void setInner(GamePiece p) {
    super.setInner(p);
    lastCachedPrototype = null;
  }

  protected void buildPrototype() {
    final PrototypeDefinition def =
      PrototypesContainer.getPrototype(prototypeName);
    if (def != null) {
      final GamePiece expandedPrototype = def.getPiece(properties);

      // Check to see if prototype definition has changed
      final String type = expandedPrototype.getType();
      if (!type.equals(lastCachedPrototype)) {
        lastCachedPrototype = type;
        try {
          RecursionLimiter.startExecution(this);

          prototype = PieceCloner.getInstance().clonePiece(expandedPrototype);
          final Decorator outer = (Decorator)
            Decorator.getInnermost(prototype).getProperty(Properties.OUTER);
          if (outer != null) { // Will be null for an empty prototype
            outer.setInner(piece);
            prototype.setProperty(Properties.OUTER, this);
          }
          else {
            prototype = null;
          }
        }
        catch (RecursionLimitException e) {
          RecursionLimiter.infiniteLoop(e);
          prototype = null;
        }
        finally {
          RecursionLimiter.endExecution();
        }
      }
    }
    else {
      prototype = null;
    }
  }

  /**
   * Build a new GamePiece instance based on the traits in the referenced {@link PrototypeDefinition}. Substitute the
   * new instance for {@link #getInner} and return it. If the referenced definition does not exist, return the default
   * inner piece.
   *
   * @return the new instance
   */
  public GamePiece getExpandedInner() {
    buildPrototype();
    return prototype != null ? prototype : piece;
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    return type;
  }

  public Command keyEvent(KeyStroke stroke) {
    return getExpandedInner().keyEvent(stroke);
  }

  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return getExpandedInner().boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getExpandedInner().draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return getExpandedInner().getName();
  }

  public Shape getShape() {
    return getExpandedInner().getShape();
  }

  public String getPrototypeName() {
    return prototypeName;
  }

  public PieceEditor getEditor() {
    return new Editor(this);
  }
  public static class Editor implements PieceEditor {
    private StringConfigurer nameConfig;

    public Editor(UsePrototype up) {
      nameConfig = new StringConfigurer(null, "Prototype name:  ", up.type.substring(ID.length()));
    }

    public Component getControls() {
      return nameConfig.getControls();
    }

    public String getState() {
      return "";
    }

    public String getType() {
      return ID + nameConfig.getValueString();
    }
  }

  // Implement Loopable
  public String getComponentName() {
    return piece.getName();
  }

  public String getComponentTypeName() {
    return getDescription();
  }
}
