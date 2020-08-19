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
package VASSAL.build.module;

import java.awt.Point;
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
import VASSAL.counters.MenuSeparator;
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

  public interface DecoratorFactory {
    Decorator createDecorator(String type, GamePiece inner);
  }

  public interface BasicPieceFactory {
    GamePiece createBasicPiece(String type);
  }

  private final Map<String, BasicPieceFactory> basicFactories = Map.ofEntries(
    Map.entry(Stack.TYPE, type -> new Stack()),
    Map.entry(BasicPiece.ID, BasicPiece::new),
    Map.entry(Deck.ID, type -> new Deck(GameModule.getGameModule(), type))
  );

  private final BasicPieceFactory defaultBasicPieceFactory = type -> null;

  private final Map<String, DecoratorFactory> decoratorFactories = Map.ofEntries(
    Map.entry(Immobilized.ID, Immobilized::new),
    Map.entry(Embellishment.ID, (type, inner) -> {
      final Embellishment e = new Embellishment(type, inner);
      if (e.getVersion() == Embellishment.BASE_VERSION) {
        return new Embellishment0(type, inner);
      }
      return e;
    }),
    Map.entry(Embellishment.OLD_ID, Embellishment::new),
    Map.entry(Hideable.ID, Hideable::new),
    Map.entry(Obscurable.ID, Obscurable::new),
    Map.entry(Labeler.ID, Labeler::new),
    Map.entry(TableInfo.ID, TableInfo::new),
    Map.entry(PropertySheet.ID, PropertySheet::new),
    Map.entry(FreeRotator.ID, FreeRotator::new),
    Map.entry(Pivot.ID, Pivot::new),
    Map.entry(NonRectangular.ID, NonRectangular::new),
    Map.entry(Marker.ID, Marker::new),
    Map.entry(Restricted.ID, Restricted::new),
    Map.entry(PlaceMarker.ID, PlaceMarker::new),
    Map.entry(Replace.ID, Replace::new),
    Map.entry(ReportState.ID, ReportState::new),
    Map.entry(MovementMarkable.ID, MovementMarkable::new),
    Map.entry(Footprint.ID, Footprint::new),
    Map.entry(ReturnToDeck.ID, ReturnToDeck::new),
    Map.entry(SendToLocation.ID, SendToLocation::new),
    Map.entry(UsePrototype.ID, UsePrototype::new),
    Map.entry(Clone.ID, Clone::new),
    Map.entry(Delete.ID, Delete::new),
    Map.entry(SubMenu.ID, SubMenu::new),
    Map.entry(MenuSeparator.ID, MenuSeparator::new),
    Map.entry(Translate.ID, Translate::new),
    Map.entry(AreaOfEffect.ID, AreaOfEffect::new),
    Map.entry(CounterGlobalKeyCommand.ID, CounterGlobalKeyCommand::new),
    Map.entry(TriggerAction.ID, TriggerAction::new),
    Map.entry(DynamicProperty.ID, DynamicProperty::new),
    Map.entry(CalculatedProperty.ID, CalculatedProperty::new),
    Map.entry(SetGlobalProperty.ID, SetGlobalProperty::new),
    Map.entry(RestrictCommands.ID, RestrictCommands::new),
    Map.entry(PlaySound.ID, PlaySound::new),
    Map.entry(ActionButton.ID, ActionButton::new),
    Map.entry(GlobalHotKey.ID, GlobalHotKey::new)
  );

  private final DecoratorFactory defaultDecoratorFactory = (type, inner) -> {
    System.err.println("Unknown type " + type);
    return new Marker(Marker.ID, inner);
  };

  public BasicCommandEncoder() {
  }

  private String typePrefix(String type) {
    final String prefix = type.substring(0, type.indexOf(';') + 1);
    return prefix.isEmpty() ? type : prefix;
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
    return decoratorFactories.getOrDefault(
      typePrefix(type), defaultDecoratorFactory
    ).createDecorator(type, inner);
  }

  /**
   * Create a GamePiece instance that is not a Decorator
   *
   * @param type
   *          the type of the GamePiece. The created piece should return this
   *          value from its {@link GamePiece#getType} method
   */
  protected GamePiece createBasic(String type) {
    return basicFactories.getOrDefault(
      typePrefix(type), defaultBasicPieceFactory
    ).createBasicPiece(type);
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

  @Override
  public void build(org.w3c.dom.Element e) {
    Builder.build(e, this);
  }

  @Override
  public void addTo(Buildable parent) {
    ((GameModule) parent).addCommandEncoder(this);
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  private static final char PARAM_SEPARATOR = '/';
  public static final String ADD = "+" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String REMOVE = "-" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String CHANGE = "D" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String MOVE = "M" + PARAM_SEPARATOR; //$NON-NLS-1$

  @Override
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

  @Override
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
}
