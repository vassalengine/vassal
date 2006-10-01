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
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.SequenceEncoder;

/**
 * Adds a menu entry that fires a specified key event to the module window.
 * Effectively allows a Game Piece to activate a button in the toolbar  
 * @author rkinney
 * 
 */
public class GlobalHotKey extends Decorator implements EditablePiece {
	public static final String ID="globalkey;";
	
	protected KeyStroke commandKey;
	
	protected KeyStroke globalHotKey;
	
	protected String commandName="Hotkey";
	
	protected KeyCommand[] commands;
	
	protected KeyCommand command;
	
	public GlobalHotKey() {
		this(ID,null);
	}

	public GlobalHotKey(String type, GamePiece inner) {
		mySetType(type);
		setInner(inner);
	}

	protected KeyCommand[] myGetKeyCommands() {
		if (commands == null) {
			command = new KeyCommand(commandName,commandKey,Decorator.getOutermost(this));
			if (commandName != null && commandName.length() > 0) {
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
		se.append(commandName).append(commandKey).append(globalHotKey);
		return ID+se.getValue();
	}

	public Command myKeyEvent(KeyStroke stroke) {
		myGetKeyCommands();
		if (command.matches(stroke)) {
			GameModule.getGameModule().fireKeyStroke(globalHotKey);
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
		return globalHotKey == null ? "Global Hotkey" : "Global Hotkey:  "+HotKeyConfigurer.getString(commandKey)+" -> "+HotKeyConfigurer.getString(globalHotKey);
	}

	public HelpFile getHelpFile() {
		return null;
	}

	public void mySetType(String type) {
		SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(type.substring(ID.length()),';');
		commandName = sd.nextToken();
		commandKey = sd.nextKeyStroke('H');
		globalHotKey = sd.nextKeyStroke('H');
		commands = null;
	}

	public PieceEditor getEditor() {
		return new Ed(this);
	}
	
	public static class Ed implements PieceEditor {
		
		private StringConfigurer commandConfig;
		private HotKeyConfigurer commandKeyConfig;
		private HotKeyConfigurer hotKeyConfig;
		
		private Box controls;

		public Ed(GlobalHotKey k) {
			controls = Box.createVerticalBox();
			commandConfig = new StringConfigurer(null,"Menu text: ",k.commandName);
			controls.add(commandConfig.getControls());

			commandKeyConfig = new HotKeyConfigurer(null,"Keyboard Command: ",k.commandKey);
			controls.add(commandKeyConfig.getControls());

			hotKeyConfig = new HotKeyConfigurer(null,"Global Hotkey: ",k.globalHotKey);
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
			se.append(commandConfig.getValueString()).append(commandKeyConfig.getValueString()).append(hotKeyConfig.getValueString());
			return ID+se.getValue();
		}
		
	}
	
	
}
