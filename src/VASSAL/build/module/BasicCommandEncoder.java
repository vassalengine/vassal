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
package VASSAL.build.module;

import java.awt.Point;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.command.AddPiece;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.MovePiece;
import VASSAL.command.NullCommand;
import VASSAL.command.PlayAudioClipCommand;
import VASSAL.command.RemovePiece;
import VASSAL.counters.ActionButton;
import VASSAL.counters.AreaOfEffect;
import VASSAL.counters.BasicPiece;
import VASSAL.counters.CalculatedProperty;
import VASSAL.counters.Clone;
import VASSAL.counters.CounterGlobalKeyCommand;
import VASSAL.counters.Deck;
import VASSAL.counters.Decorator;
import VASSAL.counters.Delete;
import VASSAL.counters.DynamicProperty;
import VASSAL.counters.Embellishment;
import VASSAL.counters.Embellishment0;
import VASSAL.counters.Footprint;
import VASSAL.counters.FreeRotator;
import VASSAL.counters.GamePiece;
import VASSAL.counters.GlobalHotKey;
import VASSAL.counters.Hideable;
import VASSAL.counters.Immobilized;
import VASSAL.counters.Labeler;
import VASSAL.counters.Marker;
import VASSAL.counters.MovementMarkable;
import VASSAL.counters.NonRectangular;
import VASSAL.counters.Obscurable;
import VASSAL.counters.Pivot;
import VASSAL.counters.PlaceMarker;
import VASSAL.counters.PlaySound;
import VASSAL.counters.PropertySheet;
import VASSAL.counters.Replace;
import VASSAL.counters.ReportState;
import VASSAL.counters.RestrictCommands;
import VASSAL.counters.Restricted;
import VASSAL.counters.ReturnToDeck;
import VASSAL.counters.SendToLocation;
import VASSAL.counters.SetGlobalProperty;
import VASSAL.counters.Stack;
import VASSAL.counters.SubMenu;
import VASSAL.counters.TableInfo;
import VASSAL.counters.Translate;
import VASSAL.counters.TriggerAction;
import VASSAL.counters.UsePrototype;
import VASSAL.tools.SequenceEncoder;

/**
 * A {@link CommandEncoder} that handles the basic commands: {@link AddPiece},
 * {@link RemovePiece}, {@link ChangePiece}, {@link MovePiece}. If a module
 * defines custom {@link GamePiece} classes, then this class may be overriden
 * and imported into the module. Subclasses should override the
 * {@link #createDecorator} method or, less often, the {@link #createBasic} or
 * {@link #createPiece} methods to allow instantiation of the custom
 * {@link GamePiece} classes.
 */
public class BasicCommandEncoder implements CommandEncoder, Buildable {
  private static final Logger logger =
    LoggerFactory.getLogger(BasicCommandEncoder.class);

  private Map<String,BasicPieceFactory> basicFactories =
    new HashMap<String,BasicPieceFactory>();
  private Map<String,DecoratorFactory> decoratorFactories =
    new HashMap<String,DecoratorFactory>();

  public BasicCommandEncoder() {
    basicFactories.put(Stack.TYPE, new BasicPieceFactory() {
      public GamePiece createBasicPiece(String type) {
        return new Stack();
      }
    });
    basicFactories.put(BasicPiece.ID, new BasicPieceFactory() {
      public GamePiece createBasicPiece(String type) {
        return new BasicPiece(type);
      }
    });
    basicFactories.put(Deck.ID, new BasicPieceFactory() {
      public GamePiece createBasicPiece(String type) {
        return new Deck(type);
      }
    });
    decoratorFactories.put(Immobilized.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Immobilized(inner, type);
      }
    });
    decoratorFactories.put(Embellishment.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        final Embellishment e = new Embellishment(type, inner);
        if (e.getVersion() == Embellishment.BASE_VERSION) {
          return new Embellishment0(type, inner);
        }
        return e;
      }
    });
    decoratorFactories.put(Embellishment.OLD_ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Embellishment(type, inner);
      }
    });
    decoratorFactories.put(Hideable.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Hideable(type, inner);
      }
    });
    decoratorFactories.put(Obscurable.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Obscurable(type, inner);
      }
    });
    decoratorFactories.put(Labeler.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Labeler(type, inner);
      }
    });
    decoratorFactories.put(TableInfo.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new TableInfo(type, inner);
      }
    });
    decoratorFactories.put(PropertySheet.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new PropertySheet(type, inner);
      }
    });
    decoratorFactories.put(FreeRotator.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new FreeRotator(type, inner);
      }
    });
    decoratorFactories.put(Pivot.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Pivot(type, inner);
      }
    });
    decoratorFactories.put(NonRectangular.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new NonRectangular(type, inner);
      }
    });
    decoratorFactories.put(Marker.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Marker(type, inner);
      }
    });
    decoratorFactories.put(Restricted.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Restricted(type, inner);
      }
    });
    decoratorFactories.put(PlaceMarker.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new PlaceMarker(type, inner);
      }
    });
    decoratorFactories.put(Replace.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Replace(type, inner);
      }
    });
    decoratorFactories.put(ReportState.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new ReportState(type, inner);
      }
    });
    decoratorFactories.put(MovementMarkable.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new MovementMarkable(type, inner);
      }
    });
    decoratorFactories.put(Footprint.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Footprint(type, inner);
      }
    });
    decoratorFactories.put(ReturnToDeck.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new ReturnToDeck(type, inner);
      }
    });
    decoratorFactories.put(SendToLocation.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new SendToLocation(type, inner);
      }
    });
    decoratorFactories.put(UsePrototype.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new UsePrototype(type, inner);
      }
    });
    decoratorFactories.put(Clone.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Clone(type, inner);
      }
    });
    decoratorFactories.put(Delete.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Delete(type, inner);
      }
    });
    decoratorFactories.put(SubMenu.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new SubMenu(type, inner);
      }
    });
    decoratorFactories.put(Translate.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new Translate(type, inner);
      }
    });
    decoratorFactories.put(AreaOfEffect.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new AreaOfEffect(type, inner);
      }
    });
    decoratorFactories.put(CounterGlobalKeyCommand.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new CounterGlobalKeyCommand(type, inner);
      }
    });
    decoratorFactories.put(TriggerAction.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new TriggerAction(type, inner);
      }
    });
    decoratorFactories.put(DynamicProperty.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new DynamicProperty(type, inner);
      }
    });
    decoratorFactories.put(CalculatedProperty.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new CalculatedProperty(type, inner);
      }
    });
    decoratorFactories.put(SetGlobalProperty.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new SetGlobalProperty(type, inner);
      }
    });
    decoratorFactories.put(RestrictCommands.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new RestrictCommands(type, inner);
      }
    });
    decoratorFactories.put(PlaySound.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new PlaySound(type, inner);
      }
    });
    decoratorFactories.put(ActionButton.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new ActionButton(type, inner);
      }
    });
    decoratorFactories.put(GlobalHotKey.ID, new DecoratorFactory() {
      public Decorator createDecorator(String type, GamePiece inner) {
        return new GlobalHotKey(type, inner);
      }
    });
  }

  /**
   * Creates a {@link Decorator} instance
   *
   * @param type
   *          the type of the Decorator to be created. Once created, the
   *          Decorator should return this value from its
   *          {@link Decorator#myGetType} method.
   *
   * @param inner
   *          the inner piece of the Decorator
   * @see Decorator
   */
  public Decorator createDecorator(String type, GamePiece inner) {
    Decorator d = null;
    String prefix = type.substring(0,type.indexOf(';')+1);
    if (prefix.length() == 0) {
      prefix = type;
    }
    DecoratorFactory f = decoratorFactories.get(prefix);
    if (f != null) {
      d = f.createDecorator(type, inner);
    }
    else {
      System.err.println("Unknown type "+type); //$NON-NLS-1$
      d = new Marker(Marker.ID,inner);
    }
    return d;
  }

  /**
   * Create a GamePiece instance that is not a Decorator
   *
   * @param type
   *          the type of the GamePiece. The created piece should return this
   *          value from its {@link GamePiece#getType} method
   */
  protected GamePiece createBasic(String type) {
    GamePiece p = null;
    String prefix = type.substring(0,type.indexOf(';')+1);
    if (prefix.length() == 0) {
      prefix = type;
    }
    BasicPieceFactory f = basicFactories.get(prefix);
    if (f != null) {
      p = f.createBasicPiece(type);
    }
    return p;
  }

  /**
   * Creates a GamePiece instance from the given type information. Determines
   * from the type whether the represented piece is a {@link Decorator} or not
   * and forwards to {@link #createDecorator} or {@link #createBasic}. This
   * method should generally not need to be overridden. Instead, override
   * createDecorator or createBasic
   */
  public GamePiece createPiece(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, '\t');
    type = st.nextToken();
    String innerType = st.hasMoreTokens() ? st.nextToken() : null;

    if (innerType != null) {
      GamePiece inner = createPiece(innerType);
      if (inner == null) {
        GameModule.getGameModule().getChatter().send("Invalid piece type - see Error Log for details"); //$NON-NLS-1$
        logger.warn("Could not create piece with type " + innerType);
        inner = new BasicPiece();
      }
      Decorator d = createDecorator(type, inner);
      return d != null ? d : inner;
    }
    else {
      return createBasic(type);
    }
  }

  public void build(org.w3c.dom.Element e) {
    Builder.build(e, this);
  }

  public void addTo(Buildable parent) {
    ((GameModule) parent).addCommandEncoder(this);
  }

  public void add(Buildable b) {
  }

  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  private static final char PARAM_SEPARATOR = '/';
  public static final String ADD = "+" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String REMOVE = "-" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String CHANGE = "D" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String MOVE = "M" + PARAM_SEPARATOR; //$NON-NLS-1$

  public Command decode(String command) {
    if (command.length() == 0) {
      return new NullCommand();
    }
    SequenceEncoder.Decoder st;
    if (command.startsWith(ADD)) {
      command = command.substring(ADD.length());
      st = new SequenceEncoder.Decoder(command, PARAM_SEPARATOR);
      String id = unwrapNull(st.nextToken());
      String type = st.nextToken();
      String state = st.nextToken();
      GamePiece p = createPiece(type);
      if (p == null) {
        return null;
      }
      else {
        p.setId(id);
        return new AddPiece(p, state);
      }
    }
    else if (command.startsWith(REMOVE)) {
      String id = command.substring(REMOVE.length());
      GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(id);
      if (target == null) {
        return new RemovePiece(id);
      }
      else {
        return new RemovePiece(target);
      }
    }
    else if (command.startsWith(CHANGE)) {
      command = command.substring(CHANGE.length());
      st = new SequenceEncoder.Decoder(command, PARAM_SEPARATOR);
      String id = st.nextToken();
      String newState = st.nextToken();
      String oldState = st.hasMoreTokens() ? st.nextToken() : null;
      return new ChangePiece(id, oldState, newState);
    }
    else if (command.startsWith(MOVE)) {
      command = command.substring(MOVE.length());
      st = new SequenceEncoder.Decoder(command, PARAM_SEPARATOR);
      String id = unwrapNull(st.nextToken());
      String newMapId = unwrapNull(st.nextToken());
      int newX = Integer.parseInt(st.nextToken());
      int newY = Integer.parseInt(st.nextToken());
      String newUnderId = unwrapNull(st.nextToken());
      String oldMapId = unwrapNull(st.nextToken());
      int oldX = Integer.parseInt(st.nextToken());
      int oldY = Integer.parseInt(st.nextToken());
      String oldUnderId = unwrapNull(st.nextToken());
      String playerid = st.nextToken(GameModule.getUserId());
      return new MovePiece(id, newMapId, new Point(newX, newY), newUnderId, oldMapId, new Point(oldX, oldY), oldUnderId, playerid);
    }
    else {
      return PlayAudioClipCommand.decode(command);
    }
  }

  private String wrapNull(String s) {
    return s == null ? "null" : s; //$NON-NLS-1$
  }

  private String unwrapNull(String s) {
    return "null".equals(s) ? null : s; //$NON-NLS-1$
  }

  public String encode(Command c) {
    SequenceEncoder se = new SequenceEncoder(PARAM_SEPARATOR);
    if (c instanceof AddPiece) {
      AddPiece a = (AddPiece) c;
      return ADD + se.append(wrapNull(a.getTarget().getId())).append(a.getTarget().getType()).append(a.getState()).getValue();
    }
    else if (c instanceof RemovePiece) {
      return REMOVE + ((RemovePiece) c).getId();
    }
    else if (c instanceof ChangePiece) {
      ChangePiece cp = (ChangePiece) c;
      se.append(cp.getId()).append(cp.getNewState());
      if (cp.getOldState() != null) {
        se.append(cp.getOldState());
      }
      return CHANGE + se.getValue();
    }
    else if (c instanceof MovePiece) {
      MovePiece mp = (MovePiece) c;
      se.append(mp.getId()).append(wrapNull(mp.getNewMapId())).append(mp.getNewPosition().x + "").append(mp.getNewPosition().y + "").append( //$NON-NLS-1$ //$NON-NLS-2$
          wrapNull(mp.getNewUnderneathId())).append(wrapNull(mp.getOldMapId())).append(mp.getOldPosition().x + "").append(mp.getOldPosition().y + "").append( //$NON-NLS-1$ //$NON-NLS-2$
          wrapNull(mp.getOldUnderneathId())).append(mp.getPlayerId());
      return MOVE + se.getValue();
    }
    else if (c instanceof NullCommand) {
      return ""; //$NON-NLS-1$
    }
    else if (c instanceof PlayAudioClipCommand) {
      return ((PlayAudioClipCommand)c).encode();
    }
    else {
      return null;
    }
  }

  public static interface DecoratorFactory {
    Decorator createDecorator(String type, GamePiece inner);
  }

  public static interface BasicPieceFactory {
    GamePiece createBasicPiece(String type);
  }
}
