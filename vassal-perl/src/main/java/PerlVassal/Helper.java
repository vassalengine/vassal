
package PerlVassal;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.command.Command;
import VASSAL.launch.BasicModule;
import VASSAL.tools.DataArchive;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.ObfuscatingOutputStream;
import VASSAL.tools.io.ZipArchive;

import javax.swing.*;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;

public final class Helper {
    private final String pieceName;
    private final Command[] myCommand = new Command[1];

    public Helper(String name) {pieceName = name;}

    public String getName() {return pieceName;}

    public Command getCommand() {return myCommand[0];}

    public void resaveSavedGame(String moduleFilename, String saveGameFilename, Command c) {
	// GameModule.init(new BasicModule(new DataArchive(moduleFilename)));
	String newSave = GameModule.getGameModule().encode(c);
	try (FileArchive archive = new ZipArchive(new File(saveGameFilename))) {
	    try (final OutputStream zout = archive.getOutputStream(GameState.SAVEFILE_ZIP_ENTRY);
		 final BufferedOutputStream bout = new BufferedOutputStream(zout);
		 final OutputStream out = new ObfuscatingOutputStream(bout)) {
		out.write(newSave.getBytes(StandardCharsets.UTF_8));
	    } catch (IOException e) {
		e.printStackTrace();
	    }
	    (new SaveMetaData()).save(archive);
	} catch (IOException e) {
	    e.printStackTrace();
	}
    }

    public void readSavedGame(String moduleFilename, String saveGameFilename) {
	// we have to run code in the GUI thread, so pretend we have a GUI
	System.setProperty("java.awt.headless", "false");

	try {
	    // run doReadSavedGame() in the GUI thread
	    SwingUtilities.invokeAndWait(() -> doReadSavedGame(moduleFilename, saveGameFilename));
	} catch (InterruptedException | InvocationTargetException e) {
	    e.printStackTrace();
	}
    }

    private void doReadSavedGame(String moduleFilename, String saveGameFilename) {
	try {
	    // we need to do a bit of reflection to ensure the needed classes exist
	    init();

	    // now we can actually decode the savedGame
	    GameModule.init(new BasicModule(new DataArchive(moduleFilename)));
	    myCommand[0] = GameModule.getGameModule().getGameState().decodeSavedGame(new File(saveGameFilename));
	} catch (IOException e) {
	    e.printStackTrace();
	}
    }

    // We need to make an instance of PlayerMenuManager available in order to decode the savedGame
    private static void init() {
	try {
	    Class<?> clazz = Class.forName("VASSAL.launch.Player$PlayerMenuManager");
	    final Constructor<?> declaredConstructor = clazz.getDeclaredConstructor();
	    declaredConstructor.setAccessible(true);
	    declaredConstructor.newInstance();
	} catch (InstantiationException | InvocationTargetException | NoSuchMethodException | IllegalAccessException | ClassNotFoundException e) {
	    e.printStackTrace();
	}
    }
}
