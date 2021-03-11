package VASSAL.counters;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.util.Arrays;
import java.util.List;

import java.util.Objects;
import javax.swing.KeyStroke;

/**
 * Adds a menu entry that fires a specified key event to the module window.
 * Effectively allows a Game Piece to activate a button in the toolbar
 * @author rkinney
 *
 */
public class GlobalHotKey extends Decorator implements TranslatablePiece {
  public static final String ID = "globalhotkey;"; // NON-NLS

  protected NamedKeyStroke commandKey;
  protected NamedKeyStroke globalHotKey;
  protected String commandName;
  protected KeyCommand[] commands;
  protected KeyCommand command;
  protected String description = "";

  public GlobalHotKey() {
    this(ID, null);
  }

  public GlobalHotKey(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      command = new KeyCommand(commandName, commandKey, Decorator.getOutermost(this), this);
      command.setEnabled(getMap() != null);
      if (commandName != null && commandName.length() > 0 && commandKey != null && ! commandKey.isNull()) {
        commands = new KeyCommand[]{command};
      }
      else {
        commands = KeyCommand.NONE;
      }
    }
    return commands;
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName).append(commandKey).append(globalHotKey).append(description);
    return ID + se.getValue();
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (command.matches(stroke)) {
      final GameModule gm = GameModule.getGameModule();
      final boolean loggingPausedByMe = gm.pauseLogging();
      GameModule.getGameModule().fireKeyStroke(globalHotKey);
      if (loggingPausedByMe) {
        return gm.resumeLogging();
      }
    }
    return null;
  }

  @Override
  public void mySetState(String newState) {
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.GlobalHotKey.trait_description", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalHotKey.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(type.substring(ID.length()), ';');
    commandName = sd.nextToken();
    commandKey = sd.nextNamedKeyStroke('H');
    globalHotKey = sd.nextNamedKeyStroke(null);
    description = sd.nextToken("");
    commands = null;
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(commandName, Resources.getString("Editor.GlobalHoyKey.global_hotkey_command")); // NON-NLS
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof GlobalHotKey)) return false;
    final GlobalHotKey c = (GlobalHotKey) o;
    if (! Objects.equals(commandName, c.commandName)) return false;
    if (! Objects.equals(commandKey, c.commandKey)) return false;
    if (! Objects.equals(globalHotKey, c.globalHotKey)) return false;
    return Objects.equals(description, c.description);
  }

  public static class Ed implements PieceEditor {

    private final StringConfigurer commandConfig;
    private final NamedHotKeyConfigurer commandKeyConfig;
    private final NamedHotKeyConfigurer hotKeyConfig;
    protected StringConfigurer descConfig;

    private final TraitConfigPanel controls;

    public Ed(GlobalHotKey k) {
      controls = new TraitConfigPanel();

      descConfig = new StringConfigurer(k.description);
      descConfig.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descConfig);

      commandConfig = new StringConfigurer(k.commandName);
      commandConfig.setHintKey("Editor.menu_command_hint");
      controls.add("Editor.menu_command", commandConfig);

      commandKeyConfig = new NamedHotKeyConfigurer(k.commandKey);
      controls.add("Editor.keyboard_command", commandKeyConfig);

      hotKeyConfig = new NamedHotKeyConfigurer(k.globalHotKey);
      controls.add("Editor.GlobalHotKey.global_hotkey", hotKeyConfig);
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
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(commandConfig.getValueString()).append(commandKeyConfig.getValueString()).append(hotKeyConfig.getValueString()).append(descConfig.getValueString());
      return ID + se.getValue();
    }

  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(commandKey, globalHotKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(commandName);
  }
}
