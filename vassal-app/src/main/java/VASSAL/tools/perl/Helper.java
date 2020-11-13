package VASSAL.tools.perl;

import VASSAL.build.GameModule;
import VASSAL.build.module.GameState;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.command.Command;
import VASSAL.counters.BasicPiece;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.io.FileArchive;
import VASSAL.tools.io.ObfuscatingOutputStream;
import VASSAL.tools.io.ZipArchive;

import javax.swing.*;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;

public final class Helper {
    private final String perlVersion;
    private final Command[] myCommand = new Command[1];
    private static boolean inited = false;

    public Helper(String version) {
        perlVersion = version;
    }

    public String getPerlVersion() {
        return perlVersion;
    }

    public String getJavaVersion() {
        String javaVersion = "0.01";
        return javaVersion;
    }

    public Command getCommand() {
        return myCommand[0];
    }

    public String getPieceImageName(BasicPiece piece) {
        String imageName = "NONE";
        try {
            Field imageNameField = BasicPiece.class.getDeclaredField("imageName");
            imageNameField.setAccessible(true);
            imageName = (String) imageNameField.get(piece);
        }
        catch (IllegalAccessException | NoSuchFieldException e) {
            e.printStackTrace();
        }
        return imageName;
    }

    public void saveGameModule(GameModule module) {
        try {
            // run save() in the GUI thread
            SwingUtilities.invokeAndWait(() -> doSaveGameModule(module));
        }
        catch (InterruptedException | InvocationTargetException e) {
            e.printStackTrace();
        }
    }

    private void doSaveGameModule(GameModule module) {
        try {
            module.save();
        }
        catch (NullPointerException npe) {
            // ignore until we fix GameModule.save()
        }
    }

    /*
    public void writeBuildFile(ArrayList<Buildable> buildables) {
      org.w3c.dom.Document doc = Builder.createNewDocument();

      Element el = doc.createElement(getClass().getName());
      GameModule module = GameModule.getGameModule();
      String[] names = module.getAttributeNames();
      for (String name : names) {
        String val = module.getAttributeValueString(name);
        if (val != null) {
          el.setAttribute(name, val);
        }
      }

      for (Buildable b : buildables) {
        el.appendChild(b.getBuildElement(doc));
      }
      final String buildString = Builder.toString(doc);
    }
  */
    public GameModule initGameModule(String moduleFilename) {
        try {
            // run doReadSavedGame() in the GUI thread
            SwingUtilities.invokeAndWait(() -> doInitGameModule(moduleFilename));
        }
        catch (InterruptedException | InvocationTargetException e) {
            e.printStackTrace();
        }
        return GameModule.getGameModule();
    }

    public void resaveSavedGame(String moduleFilename, String saveGameFilename, Command c) {
        // we've already initialized the GameModule, so we just use it to encode
        String newSave = GameModule.getGameModule().encode(c);
        try (FileArchive archive = new ZipArchive(new File(saveGameFilename))) {
            try (final OutputStream zout = archive.getOutputStream(GameState.SAVEFILE_ZIP_ENTRY);
                 final BufferedOutputStream bout = new BufferedOutputStream(zout);
                 final OutputStream out = new ObfuscatingOutputStream(bout)) {
                out.write(newSave.getBytes(StandardCharsets.UTF_8));
            }
            catch (IOException e) {
                e.printStackTrace();
            }
            (new SaveMetaData()).save(archive);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public Command readSavedGame(String moduleFilename, String saveGameFilename) {
        // we have to run code in the GUI thread, so pretend we have a GUI
        System.setProperty("java.awt.headless", "false");

        try {
            // run doReadSavedGame() in the GUI thread
            SwingUtilities.invokeAndWait(() -> doReadSavedGame(moduleFilename, saveGameFilename));
        }
        catch (InterruptedException | InvocationTargetException e) {
            e.printStackTrace();
        }
        return myCommand[0];
    }

    private void doReadSavedGame(String moduleFilename, String saveGameFilename) {
        try {
            doInitGameModule(moduleFilename);
            myCommand[0] = GameModule.getGameModule().getGameState().decodeSavedGame(new File(saveGameFilename));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static void doInitGameModule(String moduleFilename) {
        try {
            // we need to do a bit of reflection to ensure the needed classes exist
            init();

            // now we can actually decode the savedGame
            GameModule.init(new GameModule(new ArchiveWriter(moduleFilename)));
        }
        catch (IOException e) {
            e.printStackTrace();
        }
    }

    // We need to make an instance of PlayerMenuManager available in order to decode the savedGame
    private static void init() {
        if (!inited) {
            try {
                Class<?> clazz = Class.forName("VASSAL.launch.Player$PlayerMenuManager");
                final Constructor<?> declaredConstructor = clazz.getDeclaredConstructor();
                declaredConstructor.setAccessible(true);
                declaredConstructor.newInstance();
                inited = true;
            } catch (InstantiationException | InvocationTargetException | NoSuchMethodException | IllegalAccessException | ClassNotFoundException e) {
                e.printStackTrace();
            }
        }
    }
}
