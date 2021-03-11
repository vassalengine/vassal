/*
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
import java.util.List;

import java.util.Objects;
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
  public static final String ID = "prototype;"; // NON-NLS
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

  @Override
  public String getDescription() {
    return buildDescription("Editor.UsePrototype.trait_description", prototypeName);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("UsePrototype.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    this.type = type;
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type.substring(ID.length()), ';');
    prototypeName = st.nextToken("");
    if (st.hasMoreTokens()) {
      final java.util.Properties p = new java.util.Properties();
      final SequenceEncoder.Decoder st2 = new SequenceEncoder.Decoder(st.nextToken(), ',');
      while (st2.hasMoreTokens()) {
        final SequenceEncoder.Decoder st3 = new SequenceEncoder.Decoder(st2.nextToken(), '=');
        if (st3.hasMoreTokens()) {
          final String key = st3.nextToken();
          if (st3.hasMoreTokens()) {
            final String value = st3.nextToken();
            p.setProperty(key, value);
          }
        }
      }
      properties = new PropertySource() {
        @Override
        public Object getProperty(Object key) {
          return p.getProperty(String.valueOf(key));
        }

        @Override
        public Object getLocalizedProperty(Object key) {
          return getProperty(key);
        }
      };
    }
    lastCachedPrototype = null;
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  protected KeyCommand[] getKeyCommands() {
    return (KeyCommand[]) getExpandedInner().getProperty(Properties.KEY_COMMANDS);
  }

  @Override
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
        lastCachedPrototype = type.intern();
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

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    return type;
  }

  @Override
  public Command keyEvent(KeyStroke stroke) {
    return getExpandedInner().keyEvent(stroke);
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    return null;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Rectangle boundingBox() {
    return getExpandedInner().boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    getExpandedInner().draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return getExpandedInner().getName();
  }

  @Override
  public Shape getShape() {
    return getExpandedInner().getShape();
  }

  public String getPrototypeName() {
    return prototypeName;
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof UsePrototype)) return false;
    final UsePrototype c = (UsePrototype) o;
    return Objects.equals(prototypeName, c.prototypeName);
  }

  @Override
  public PieceEditor getEditor() {
    return new Editor(this);
  }

  public static class Editor implements PieceEditor {
    private final TraitConfigPanel controls;
    private final StringConfigurer nameConfig;

    public Editor(UsePrototype up) {
      controls = new TraitConfigPanel();

      nameConfig = new StringConfigurer(up.type.substring(ID.length()));
      controls.add("Editor.UsePrototype.prototype_name", nameConfig);

    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getState() {
      return "";
    }

    @Override
    public String getType() {
      return ID + nameConfig.getValueString();
    }
  }

  // Implement Loopable
  @Override
  public String getComponentName() {
    return piece.getName();
  }

  @Override
  public String getComponentTypeName() {
    return getDescription();
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return List.of(prototypeName);
  }
}
