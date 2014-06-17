package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;

import javax.swing.Box;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * Adds a menu entry that fires a specified key event to the module window.
 * Effectively allows a Game Piece to activate a button in the toolbar
 * @author rkinney
 *
 */
public class GlobalHotKey extends Decorator implements TranslatablePiece {
  public static final String ID="globalhotkey;";

  protected NamedKeyStroke commandKey;
  protected NamedKeyStroke globalHotKey;
  protected String commandName="Hotkey";
  protected KeyCommand[] commands;
  protected KeyCommand command;
  protected String description = "";

  public GlobalHotKey() {
    this(ID,null);
  }

  public GlobalHotKey(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (commands == null) {
      command = new KeyCommand(commandName,commandKey,Decorator.getOutermost(this), this);
      command.setEnabled(getMap() != null);
      if (commandName != null && commandName.length() > 0 && commandKey != null && ! commandKey.isNull()) {
        commands = new KeyCommand[]{command};
      }
      else {
        commands = new KeyCommand[0];
      }
    }
    return commands;
  }

  public String myGetState() {
    return "";
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(commandName).append(commandKey).append(globalHotKey).append(description);
    return ID+se.getValue();
  }

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

  public void mySetState(String newState) {
  }

  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g,x,y,obs,zoom);
  }

  public String getName() {
    return piece.getName();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public String getDescription() {
    return (description == null || description.length() == 0) ? "Global Hotkey" : "Global Hotkey:  "+description;
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GlobalHotKey.htm");
  }

  public void mySetType(String type) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(type.substring(ID.length()),';');
    commandName = sd.nextToken();
    commandKey = sd.nextNamedKeyStroke('H');
    globalHotKey = sd.nextNamedKeyStroke(null);
    description = sd.nextToken("");
    commands = null;
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public PieceI18nData getI18nData() {
    return getI18nData(commandName, getDescription() + " command");
  }

  public static class Ed implements PieceEditor {

    private StringConfigurer commandConfig;
    private NamedHotKeyConfigurer commandKeyConfig;
    private NamedHotKeyConfigurer hotKeyConfig;
    protected StringConfigurer descConfig;

    private Box controls;

    public Ed(GlobalHotKey k) {
      controls = Box.createVerticalBox();

      descConfig = new StringConfigurer(null, "Description:  ", k.description);
      controls.add(descConfig.getControls());

      commandConfig = new StringConfigurer(null,"Menu text:  ",k.commandName);
      controls.add(commandConfig.getControls());

      commandKeyConfig = new NamedHotKeyConfigurer(null,"Keyboard Command:  ",k.commandKey);
      controls.add(commandKeyConfig.getControls());

      hotKeyConfig = new NamedHotKeyConfigurer(null,"Global Hotkey:  ",k.globalHotKey);
      controls.add(hotKeyConfig.getControls());
    }

    public Component getControls() {
      return controls;
    }

    public String getState() {
      return "";
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(commandConfig.getValueString()).append(commandKeyConfig.getValueString()).append(hotKeyConfig.getValueString()).append(descConfig.getValueString());
      return ID+se.getValue();
    }

  }


}
