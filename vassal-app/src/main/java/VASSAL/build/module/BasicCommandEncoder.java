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

import VASSAL.build.BadDataReport;
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
import VASSAL.counters.Deselect;
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
import VASSAL.counters.TranslatableMessage;
import VASSAL.counters.MenuSeparator;
import VASSAL.counters.MovementMarkable;
import VASSAL.counters.NonRectangular;
import VASSAL.counters.Obscurable;
import VASSAL.counters.PieceDefiner;
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
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.SequenceEncoder;

/**
 * Although it is the {@link CommandEncoder} which handles the basic commands: {@link AddPiece},
 * {@link RemovePiece}, {@link ChangePiece}, {@link MovePiece}, this class is most commonly needed by
 * module designers who want to make custom "Traits" for game pieces because it contains {@link #createDecorator},
 * the {@link DecoratorFactory} for Traits, which are usually internally referred to as Decorators because they
 * are implemented using the <a href="https://en.wikipedia.org/wiki/Decorator_pattern">Decorator Pattern</a>.
 * If a module is to add its own custom game pieces, it will need to override the {@link #createDecorator} method, and
 * use this pattern to create any custom Traits:
 * <pre>
 *   package MyCustomClasses; // Your package name here
 *   public class MyCustomCommandEncoder extends BasicCommandEncoder {
 *     public Decorator createDecorator(String type, GamePiece inner) {
 *       if (type.startsWith(MyCustomClass.ID)) {
 *         return new MyCustomClass(type, inner);
 *       }
 *       //... more custom traits, possibly
 *
 *       // Now allow BasicCommandEncoder to run so that "normal" Traits process
 *       return super.createDecorator(type, inner);
 *     }
 *   }
 * </pre>
 * Then in the buildFile (XML) for the module, the VASSAL.build.module.BasicCommandEncoder entry is replaced
 * with an entry for (in the example above): <code>MyCustomClasses.MyCustomCommandEncoder</code>
 * The class files are placed in a <code>MyCustomClasses</code> (package name) folder in the Zip structure of the VMOD file, and
 * At that point the new trait can be imported into a piece as <code>MyCustomClasses.MyCustomClass</code> (package name followed by
 * class name).
 *
 * Less often, the {@link #createBasic} or {@link #createPiece} methods could be overridden to allow instantiation of
 * custom {@link GamePiece} classes.
 */
public class BasicCommandEncoder implements CommandEncoder, Buildable {
  /**
   * Factory interface for Decorators
   *
   * See: <a href="https://en.wikipedia.org/wiki/Decorator_pattern">Decorator Pattern</a>, <a href="https://en.wikipedia.org/wiki/Factory_method_pattern">Factory Pattern</a>
   */
  @FunctionalInterface
  public interface DecoratorFactory {
    Decorator createDecorator(String type, GamePiece inner);
  }

  /**
   * Factory interface for BasicPieces.
   * See: <a href="https://en.wikipedia.org/wiki/Factory_method_pattern">Factory Pattern</a>
   */
  @FunctionalInterface
  public interface BasicPieceFactory {
    GamePiece createBasicPiece(String type);
  }

  /**
   * Factories for the three Basic Piece types that can be created:
   * Stack, BasicPiece, and Deck.
   */
  private final Map<String, BasicPieceFactory> basicFactories = Map.ofEntries(
    Map.entry(Stack.TYPE, type -> new Stack()),
    Map.entry(BasicPiece.ID, BasicPiece::new),
    Map.entry(Deck.ID, type -> new Deck(GameModule.getGameModule(), type))
  );

  private final BasicPieceFactory defaultBasicPieceFactory = type -> null;

  /**
   * Factories for all of the standard Trait (aka "Decorator") types. If a new standard Trait
   * is to be added to *VASSAL* it must be added here (as opposed to adding a Custom Class trait,
   * which is added per the documentation in {@link BasicCommandEncoder}'s main Javadoc entry).
   */
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
    Map.entry(TranslatableMessage.ID, TranslatableMessage::new),
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
    Map.entry(GlobalHotKey.ID, GlobalHotKey::new),
    Map.entry(Deselect.ID, Deselect::new)
  );

  /**
   * Fallthrough factory to catch unknown types.
   */
  private final DecoratorFactory defaultDecoratorFactory = (type, inner) -> {
    ErrorDialog.dataWarning(new BadDataReport("Unknown type " + type + " not found in BasicCommandEncoder's list of traits and basic pieces.", "")); //NON-NLS
    return new Marker(Marker.ID, inner);
  };

  /**
   * Parses out the command prefix for a type definition
   * @param type a type definition string
   * @return the command prefix for the type definition
   */
  private String typePrefix(String type) {
    final String prefix = type.substring(0, type.indexOf(';') + 1);
    return prefix.isEmpty() ? type : prefix;
  }

  /**
   * Creates a {@link Decorator} instance - a {@link GamePiece} "Trait". Modules which wish to provide their own custom
   * classes should subclass BasicCommandEncoder and override this class. The override should check for and parse any
   * definitions that match desired custom Traits(Decorators), and then use super to call this method for any unmatched
   * definitions. Further documentation on creating custom traits appears in BasicCommandEncoder's own javadoc entry, or
   * alternatively at the top of BasicCommandEncoder.java.
   *
   * @param type the type of the Decorator ("Trait") to be created. Once created, the Decorator should
   *             return this value from its {@link Decorator#myGetType} method.
   * @param inner the inner trait/piece of the Decorator (the "innermost" member of a game piece will be a {@link BasicPiece}; each
   *              successive Trait in the trait list presented in a piece's {@link PieceDefiner} dialog represents a sep "outward").
   * @see Decorator, <a href="https://en.wikipedia.org/wiki/Decorator_pattern">Decorator Pattern</a>
   */
  public Decorator createDecorator(String type, GamePiece inner) {
    return decoratorFactories.getOrDefault(
      typePrefix(type), defaultDecoratorFactory
    ).createDecorator(type, inner);
  }

  /**
   * Create a {@link GamePiece} instance that is not a Decorator ("Trait"). In other words a {@link BasicPiece}, a {@link Stack}, or a {@link Deck}.
   *
   * @param type the type of the GamePiece. The created piece should return this value from its {@link GamePiece#getType} method.
   */
  protected GamePiece createBasic(String type) {
    return basicFactories.getOrDefault(
      typePrefix(type), defaultBasicPieceFactory
    ).createBasicPiece(type);
  }

  /**
   * Creates a GamePiece instance from the given type information. Determines
   * from the type whether the represented piece is a {@link Decorator} ("Trait") or not
   * and forwards to {@link #createDecorator} or {@link #createBasic}. This
   * method should generally not need to be overridden. Instead, override
   * {@link #createDecorator} or {@link #createBasic}
   *
   * @param type definition string of the piece or trait to be created.
   */
  public GamePiece createPiece(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, '\t');
    type = st.nextToken();
    final String innerType = st.hasMoreTokens() ? st.nextToken() : null;

    if (innerType != null) {
      GamePiece inner = createPiece(innerType);
      if (inner == null) {
        ErrorDialog.dataWarning(new BadDataReport("Could not create piece with type " + innerType, type)); //NON-NLS
        inner = new BasicPiece();
      }
      final Decorator d = createDecorator(type, inner);
      return d != null ? d : inner;
    }
    else {
      return createBasic(type);
    }
  }

  /**
   * Build a BasicCommandEncoder from the XML buildFile. See {@link Builder}
   * @param e the XML element containing the object data
   */
  @Override
  public void build(org.w3c.dom.Element e) {
    Builder.build(e, this);
  }

  /**
   * Adds this BasicCommandEncoder to its parent, which should be the {@link GameModule}.
   * @param parent the {@link GameModule}
   */
  @Override
  public void addTo(Buildable parent) {
    ((GameModule) parent).addCommandEncoder(this);
  }

  /**
   * Adds a buildable subcomponent. BasicCommandEncoder doesn't normally have subcomponents, so this is empty.
   * @param b the buildable subcomponent
   */
  @Override
  public void add(Buildable b) {
  }

  /**
   * @return an XML element from which this component can be built
   * @param doc An XML document
   */
  @Override
  public org.w3c.dom.Element getBuildElement(org.w3c.dom.Document doc) {
    return doc.createElement(getClass().getName());
  }

  private static final char PARAM_SEPARATOR = '/';
  public static final String ADD = "+" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String REMOVE = "-" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String CHANGE = "D" + PARAM_SEPARATOR; //$NON-NLS-1$
  public static final String MOVE = "M" + PARAM_SEPARATOR; //$NON-NLS-1$

  /**
   * Deserializes a string into a Basic Piece command (Add, Remove, Change, Move, and... Play Audio Clip!), readying it for execution.
   * @param command string form of the command
   * @return Command object for command.
   * @see CommandEncoder
   */
  @Override
  public Command decode(String command) {
    if (command.length() == 0) {
      return new NullCommand();
    }
    final SequenceEncoder.Decoder st;
    if (command.startsWith(ADD)) {
      command = command.substring(ADD.length());
      st = new SequenceEncoder.Decoder(command, PARAM_SEPARATOR);
      final String id = unwrapNull(st.nextToken());
      final String type = st.nextToken();
      final String state = st.nextToken();
      final GamePiece p = createPiece(type);
      if (p == null) {
        return null;
      }
      else {
        p.setId(id);
        return new AddPiece(p, state);
      }
    }
    else if (command.startsWith(REMOVE)) {
      final String id = command.substring(REMOVE.length());
      final GamePiece target = GameModule.getGameModule().getGameState().getPieceForId(id);
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
      final String id = st.nextToken();
      final String newState = st.nextToken();
      final String oldState = st.hasMoreTokens() ? st.nextToken() : null;
      return new ChangePiece(id, oldState, newState);
    }
    else if (command.startsWith(MOVE)) {
      command = command.substring(MOVE.length());
      st = new SequenceEncoder.Decoder(command, PARAM_SEPARATOR);
      final String id = unwrapNull(st.nextToken());
      final String newMapId = unwrapNull(st.nextToken());
      final int newX = Integer.parseInt(st.nextToken());
      final int newY = Integer.parseInt(st.nextToken());
      final String newUnderId = unwrapNull(st.nextToken());
      final String oldMapId = unwrapNull(st.nextToken());
      final int oldX = Integer.parseInt(st.nextToken());
      final int oldY = Integer.parseInt(st.nextToken());
      final String oldUnderId = unwrapNull(st.nextToken());
      final String playerid = st.nextToken(GameModule.getUserId());
      return new MovePiece(id, newMapId, new Point(newX, newY), newUnderId, oldMapId, new Point(oldX, oldY), oldUnderId, playerid);
    }
    else {
      return PlayAudioClipCommand.decode(command);
    }
  }

  /**
   * Safely wraps a string-which-might-be-null
   * @param s String value, or null
   * @return The string passed, or the string "null" if value was null
   */
  private String wrapNull(String s) {
    return s == null ? "null" : s; //$NON-NLS-1$
  }

  /**
   * Unwraps a String where the string "null" represents a null value
   * @param s String to be unwrapped
   * @return null if string was "null", otherwise the string passed.
   */
  private String unwrapNull(String s) {
    return "null".equals(s) ? null : s; //$NON-NLS-1$
  }

  /**
   * Serializes a Basic Piece command (Add, Remove, Change, Move, and ... Play Audio Clip!) into a String,
   * readying it for transmission to other clients.
   * @param c Command to be serialized
   * @return String form of the command
   * @see CommandEncoder
   */
  @Override
  public String encode(Command c) {
    final SequenceEncoder se = new SequenceEncoder(PARAM_SEPARATOR);
    if (c instanceof AddPiece) {
      final AddPiece a = (AddPiece) c;
      return ADD +
        se
          .append(wrapNull(a.getTarget().getId()))
          .append(a.getTarget().getType())
          .append(a.getState())
          .getValue();
    }
    else if (c instanceof RemovePiece) {
      return REMOVE + ((RemovePiece) c).getId();
    }
    else if (c instanceof ChangePiece) {
      final ChangePiece cp = (ChangePiece) c;
      se.append(cp.getId()).append(cp.getNewState());
      if (cp.getOldState() != null) {
        se.append(cp.getOldState());
      }
      return CHANGE + se.getValue();
    }
    else if (c instanceof MovePiece) {
      final MovePiece mp = (MovePiece) c;
      se.append(mp.getId())
        .append(wrapNull(mp.getNewMapId()))
        .append(mp.getNewPosition().x)
        .append(mp.getNewPosition().y)
        .append(wrapNull(mp.getNewUnderneathId()))
        .append(wrapNull(mp.getOldMapId()))
        .append(mp.getOldPosition().x)
        .append(mp.getOldPosition().y)
        .append(wrapNull(mp.getOldUnderneathId()))
        .append(mp.getPlayerId());
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
