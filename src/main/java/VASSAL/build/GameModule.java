/*
 *
 * Copyright (c) 2000-2012 by Rodney Kinney, Brent Easton
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
package VASSAL.build;

import java.awt.FileDialog;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.ArrayUtils;

import org.slf4j.LoggerFactory;

import VASSAL.Info;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.ChartWindow;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.DoActionButton;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.GameState;
import VASSAL.build.module.GlobalKeyCommand;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Inventory;
import VASSAL.build.module.Map;
import VASSAL.build.module.ModuleExtension;
import VASSAL.build.module.MultiActionButton;
import VASSAL.build.module.NotesWindow;
import VASSAL.build.module.PieceWindow;
import VASSAL.build.module.PlayerHand;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.Plugin;
import VASSAL.build.module.PredefinedSetup;
import VASSAL.build.module.PrivateMap;
import VASSAL.build.module.PrototypesContainer;
import VASSAL.build.module.RandomTextButton;
import VASSAL.build.module.ServerConnection;
import VASSAL.build.module.SpecialDiceButton;
import VASSAL.build.module.StartupGlobalKeyCommand;
import VASSAL.build.module.ToolbarMenu;
import VASSAL.build.module.WizardSupport;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.metadata.ModuleMetaData;
import VASSAL.build.module.properties.ChangePropertyCommandEncoder;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.module.turn.TurnTracker;
import VASSAL.build.widget.PieceSlot;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.Logger;
import VASSAL.command.NullCommand;
import VASSAL.configure.CompoundValidityChecker;
import VASSAL.configure.MandatoryComponent;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.launch.PlayerWindow;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.CRCUtils;
import VASSAL.tools.DataArchive;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ToolBarComponent;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.image.ImageTileSource;
import VASSAL.tools.image.tilecache.ImageTileDiskCache;

/**
 * The GameModule class is the base class for a VASSAL module.  It is
 * the root of the {@link Buildable} containment hierarchy.
 * Components which are added directly to the GameModule are contained
 * in the <code>VASSAL.build.module</code> package.
 *
 * <p>It is a singleton, and contains access points for many other classes,
 * such as {@link DataArchive}, {@link ServerConnection}, {@link Logger},
 * and {@link Prefs}.</p>
 */
public abstract class GameModule extends AbstractConfigurable implements CommandEncoder, ToolBarComponent, PropertySource, MutablePropertiesContainer, GpIdSupport {
  private static final org.slf4j.Logger log =
    LoggerFactory.getLogger(GameModule.class);

  protected static final String DEFAULT_NAME = "Unnamed module";  //$NON-NLS-1$
  public static final String MODULE_NAME = "name";  //$NON-NLS-1$
  public static final String MODULE_VERSION = "version";  //$NON-NLS-1$
  public static final String DESCRIPTION = "description";
  public static final String VASSAL_VERSION_CREATED = "VassalVersion";  //$NON-NLS-1$
  /** The System property of this name will return a version identifier for the version of VASSAL being run */
  public static final String VASSAL_VERSION_RUNNING = "runningVassalVersion";  //$NON-NLS-1$
  public static final String NEXT_PIECESLOT_ID = "nextPieceSlotId";
  public static final String BUILDFILE = "buildFile";

  private static GameModule theModule;

  protected String moduleVersion = "0.0";  //$NON-NLS-1$
  protected String vassalVersionCreated = "0.0";  //$NON-NLS-1$
  protected String gameName = DEFAULT_NAME;
  protected String localizedGameName = null;
  protected String description = "";
  protected String lastSavedConfiguration;
  protected FileChooser fileChooser;
  protected FileDialog fileDialog;
  protected MutablePropertiesContainer propsContainer = new Impl();
  protected PropertyChangeListener repaintOnPropertyChange =
      new PropertyChangeListener() {
    @Override
    public void propertyChange(PropertyChangeEvent evt) {
      for (Map map : Map.getMapList()) {
        map.repaint();
      }
    }
  };

  protected PlayerWindow frame = new PlayerWindow();
  protected JPanel controlPanel = frame.getControlPanel();

  protected GameState theState;
  protected DataArchive archive;
  protected Prefs preferences;
  protected Logger logger;
  protected Chatter chat;
  protected Random RNG = new SecureRandom();
  protected ServerConnection server;

  protected ImageTileSource tcache;

  protected WizardSupport wizardSupport;
  protected PropertyChangeSupport idChangeSupport;

  protected List<KeyStrokeSource> keyStrokeSources = new ArrayList<>();
  protected List<KeyStrokeListener> keyStrokeListeners = new ArrayList<>();
  protected CommandEncoder[] commandEncoders = new CommandEncoder[0];
  protected List<String> deferredChat = new ArrayList<>();

  protected int nextGpId = 0;

  protected boolean loggingPaused = false;
  protected Object loggingLock = new Object();
  protected Command pausedCommands;

  /*
   * Store the currently building GpId source. Only meaningful while
   * the GameModule or an Extension is actually in the process of being built
   * during module/extension load.
   */
  protected GpIdSupport gpidSupport = null;
  protected Long crc = null;
  
  private static String oldDragThreshold; //

  /**
   * @return the top-level frame of the controls window
   */
  public JFrame getFrame() {
    return frame;
  }

  public void initFrameTitle() {
    frame.setTitle(getLocalizedGameName());
  }

  public WizardSupport getWizardSupport() {
    if (wizardSupport == null) {
      wizardSupport = new WizardSupport();
    }
    return wizardSupport;
  }

  protected GameModule(DataArchive archive) {
    this.archive = archive;

    frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    frame.addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        quit();
      }
    });

    addKeyStrokeSource(
      new KeyStrokeSource(frame.getRootPane(), JComponent.WHEN_IN_FOCUSED_WINDOW));

    validator = new CompoundValidityChecker(
      new MandatoryComponent(this, Documentation.class),
      new MandatoryComponent(this, GlobalOptions.class));

    addCommandEncoder(new ChangePropertyCommandEncoder(propsContainer));
  }

  /**
   * Initialize the module
   */
  protected abstract void build() throws IOException;

  @Override
  public void setAttribute(String name, Object value) {
    if (MODULE_NAME.equals(name)) {
      if (Localization.getInstance().isTranslationInProgress()) {
        localizedGameName = (String) value;
      }
      else {
        gameName = (String) value;
      }
      setConfigureName((String) value);
    }
    else if (MODULE_VERSION.equals(name)) {
      moduleVersion = (String) value;
    }
    else if (VASSAL_VERSION_CREATED.equals(name)) {
      vassalVersionCreated = (String) value;
      String runningVersion = Info.getVersion();
      if (Info.compareVersions(vassalVersionCreated, runningVersion) > 0) {
        WarningDialog.show("GameModule.version_warning",
                           vassalVersionCreated, runningVersion);
      }
    }
    else if (NEXT_PIECESLOT_ID.equals(name)) {
      try {
        nextGpId = Integer.parseInt((String) value);
      }
      catch (NumberFormatException e) {
        throw new IllegalBuildException(e);
      }
    }
    else if (DESCRIPTION.equals(name)) {
      description = (String) value;
    }
  }

  @Override
  public String getAttributeValueString(String name) {
    if (MODULE_NAME.equals(name)) {
      return gameName;
    }
    else if (MODULE_VERSION.equals(name)) {
      return moduleVersion;
    }
    else if (VASSAL_VERSION_CREATED.equals(name)) {
      return vassalVersionCreated;
    }
    else if (VASSAL_VERSION_RUNNING.equals(name)) {
      return Info.getVersion();
    }
    else if (NEXT_PIECESLOT_ID.equals(name)) {
      return String.valueOf(nextGpId);
    }
    else if (DESCRIPTION.equals(name)) {
      return description;
    }
    return null;
  }

  /**
   *
   * A valid verson format is "w.x.y[bz]", where
   * 'w','x','y', and 'z' are integers.
   * @return a negative number if <code>v2</code> is a later version
   * the <code>v1</code>, a positive number if an earlier version,
   * or zero if the versions are the same.
   *
   * @deprecated use {@link Info#compareVersions}
   */
  @Deprecated
  public static int compareVersions(String v1, String v2) {
    return Info.compareVersions(v1, v2);
  }

  @Override
  public void addTo(Buildable b) {
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GameModule.component_type");  //$NON-NLS-1$
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm");  //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      MODULE_NAME,
      MODULE_VERSION,
      DESCRIPTION,
      VASSAL_VERSION_CREATED,
      NEXT_PIECESLOT_ID
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.GameModule.name_label"),    //$NON-NLS-1$
      Resources.getString("Editor.GameModule.version_label"), //$NON-NLS-1$
      Resources.getString("Editor.GameModule.description")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      String.class
    };
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{
      Map.class,
      PieceWindow.class,
      PrototypesContainer.class,
      ToolbarMenu.class,
      MultiActionButton.class,
      DoActionButton.class,
      DiceButton.class,
      GlobalKeyCommand.class,
      StartupGlobalKeyCommand.class,
      Inventory.class,
//                  InternetDiceButton.class,   // Disable internet dice button until Bones server can prevent email spamming
      RandomTextButton.class,
      SpecialDiceButton.class,
      PredefinedSetup.class,
      ChartWindow.class,
      PrivateMap.class,
      PlayerHand.class,
      NotesWindow.class,
      TurnTracker.class
    };
  }

  /**
   * The GameModule acts as the mediator for hotkey events.
   *
   * Components that wish to fire hotkey events when they have the
   * focus should register themselves using this method.  These events will be
   * forwarded to all listeners that have registered themselves with {@link #addKeyStrokeListener}
   */
  public void addKeyStrokeSource(KeyStrokeSource src) {
    keyStrokeSources.add(src);
    for (KeyStrokeListener l : keyStrokeListeners) {
      l.addKeyStrokeSource(src);
    }
  }

  /**
   * The GameModule acts as the mediator for hotkey events.
   *
   * Objects that react to hotkey events should register themselves
   * using this method.  Any component that has been registered with {@link #addKeyStrokeSource}
   * will forward hotkey events to listeners registered with this method.
   */
  public void addKeyStrokeListener(KeyStrokeListener l) {
    keyStrokeListeners.add(l);
    for (KeyStrokeSource s : keyStrokeSources) {
      l.addKeyStrokeSource(s);
    }
  }

  @Deprecated public void fireKeyStroke(KeyStroke stroke) {
    if (stroke != null) {
      for (KeyStrokeListener l : keyStrokeListeners) {
        l.keyPressed(stroke);
      }
    }
  }

  public void fireKeyStroke(NamedKeyStroke stroke) {
    if (stroke != null && !stroke.isNull()) {
      fireKeyStroke(stroke.getKeyStroke());
    }
  }

  /**
   * @return the name of the game for this module
   */
  public String getGameName() {
    return gameName;
  }

  public String getLocalizedGameName() {
    return localizedGameName == null ? gameName : localizedGameName;
  }

  public String getGameVersion() {
    return moduleVersion;
  }

  /** The {@link Prefs} key for the user's real name */
  public static final String REAL_NAME = "RealName"; //$NON-NLS-1$
  /** The {@link Prefs} key for the user's secret name */
  public static final String SECRET_NAME = "SecretName"; //$NON-NLS-1$
  /** The {@link Prefs} key for the user's personal info */
  public static final String PERSONAL_INFO = "Profile"; //$NON-NLS-1$

  public void addIdChangeListener(PropertyChangeListener l) {
    idChangeSupport.addPropertyChangeListener(l);
  }

  public void removeIdChangeListener(PropertyChangeListener l) {
    idChangeSupport.removePropertyChangeListener(l);
  }

  /**
   * @return the preferences for this module
   */
  public Prefs getPrefs() {
    if (preferences == null) {
      setPrefs(new Prefs(Prefs.getGlobalPrefs().getEditor(), gameName));
    }
    return preferences;
  }

  /**
   * A set of preferences that applies to all modules
   * @return
   */
  @Deprecated
  public Prefs getGlobalPrefs() {
    return Prefs.getGlobalPrefs();
  }

  /**
   * This method adds a {@link CommandEncoder} to the list of objects
   * that will attempt to decode/encode a command
   *
   * @see #decode
   * @see #encode
   */
  public void addCommandEncoder(CommandEncoder ce) {
    commandEncoders = ArrayUtils.add(commandEncoders, ce);
  }

  /**
   * This method removes a {@link CommandEncoder} from the list of objects
   * that will attempt to decode/encode a command
   *
   *
   * @see #decode
   * @see #encode
   */
  public void removeCommandEncoder(CommandEncoder ce) {
    commandEncoders = ArrayUtils.removeElement(commandEncoders, ce);
  }

  /**
   * Central location to create any type of GamePiece from within VASSAL
   *
   * @param type
   * @return
   */
  public GamePiece createPiece(String type) {
    for (CommandEncoder commandEncoder : commandEncoders) {
      if (commandEncoder instanceof BasicCommandEncoder) {
        GamePiece p = ((BasicCommandEncoder) commandEncoder).createPiece(type);
        if (p != null) {
          return p;
        }
      }
    }
    return null;
  }

  public GamePiece createPiece(String type, GamePiece inner) {
    for (CommandEncoder commandEncoder : commandEncoders) {
      if (commandEncoder instanceof BasicCommandEncoder) {
        GamePiece p = ((BasicCommandEncoder) commandEncoder).createDecorator(type, inner);
        if (p != null) {
          return p;
        }
      }
    }
    return null;
  }

  /**
   * Display the given text in the control window's status line.
   * Save the messages for later if the Chatter has not been initialised yet
   */
  public void warn(String s) {
    if (chat == null) {
      deferredChat.add(s);
    }
    else {
      chat.show(" - " + s); //$NON-NLS-1$
    }
  }

  /**
   * @return a single Random number generator that all objects may share
   */
  public Random getRNG() {
    return RNG;
  }

  /**
   * @return the object responsible for logging commands to a logfile
   */
  public Logger getLogger() {
    return logger;
  }

  /**
   * Set the object that displays chat text. Display any warning
   * messages deferred during earlier initialisation
   */
  public void setChatter(Chatter c) {
    chat = c;
    if (deferredChat.size() > 0) {
      for (String msg : deferredChat) {
        warn(msg);
      }
      deferredChat.clear();
    }
  }

  public JComponent getControlPanel() {
    return controlPanel;
  }

  /**
   * @return the object that displays chat text
   */
  public Chatter getChatter() {
    return chat;
  }

  public void setPrefs(Prefs p) {
    preferences = p;
    preferences.getEditor().initDialog(getFrame());
  }

  @Deprecated
  public void setGlobalPrefs(Prefs p) {
  }

  /**
   * Uses the registered  {@link CommandEncoder}s
   * to decode a String into a {@link Command}.
   */
  @Override
  public Command decode(String command) {
    if (command == null) {
      return null;
    }
    else {
      Command c = null;
      for (int i = 0; i < commandEncoders.length && c == null; ++i) {
        c = commandEncoders[i].decode(command);
      }
      if (c == null) {
        System.err.println("Failed to decode " + command); //$NON-NLS-1$
      }
      return c;
    }
  }

  /**
   * Uses the registered {@link CommandEncoder}s to encode a {@link Command} into a String object
   */
  @Override
  public String encode(Command c) {
    if (c == null) {
      return null;
    }
    String s = null;
    for (int i = 0; i < commandEncoders.length && s == null; ++i) {
      s = commandEncoders[i].encode(c);
    }
    if (s == null) {
      System.err.println("Failed to encode " + c); //$NON-NLS-1$
    }
    return s;
  }

  /**
   * @return a common FileChooser so that recent file locations
   * can be remembered
   */
  public FileChooser getFileChooser() {
    if (fileChooser == null) {
      fileChooser = FileChooser.createFileChooser(getFrame(),
        getGameState().getSavedGameDirectoryPreference());
    }
    else {
      fileChooser.resetChoosableFileFilters();
      fileChooser.rescanCurrentDirectory();
    }

    return fileChooser;
  }

  /**
   * @deprecated Use {@link #getFileChooser} instead.
   */
  @Deprecated
  public FileDialog getFileDialog() {
    if (fileDialog == null) {
      fileDialog = new FileDialog(getFrame());
      File f = getGameState().getSavedGameDirectoryPreference().getFileValue();
      if (f != null) {
        fileDialog.setDirectory(f.getPath());
      }
      fileDialog.setModal(true);
    }
    else {
      fileDialog.setDirectory(fileDialog.getDirectory());
    }
    return fileDialog;
  }

  /**
   * @return the JToolBar of the command window
   */
  @Override
  public JToolBar getToolBar() {
    return frame.getToolBar();
  }

  /**
   * Append the string to the title of the controls window and all Map windows
   * @param s If null, set the title to the default.
   */
  public void appendToTitle(String s) {
    if (s == null) {
      frame.setTitle(Resources.getString("GameModule.frame_title", getLocalizedGameName()));  //$NON-NLS-1$
    }
    else {
      frame.setTitle(frame.getTitle() + s);
    }
    for (Map m : getComponentsOf(Map.class)) {
      m.appendToTitle(s);
    }
  }

  /**
   * Exit the application, prompting user to save if necessary
   */
  public void quit() {
    if (shutDown()) {
      System.exit(0);
    }
  }

  /**
   * Prompt user to save open game and modules/extensions being edited
   * @return true if shutDown should proceed, i.e. user did not cancel
   */
  public boolean shutDown() {
    boolean cancelled;
    getGameState().setup(false);
    cancelled = getGameState().isGameStarted();

    if (!cancelled) {
      if (getDataArchive() instanceof ArchiveWriter
          && !buildString().equals(lastSavedConfiguration)) {
        switch (JOptionPane.showConfirmDialog(frame,
          Resources.getString("GameModule.save_module"),  //$NON-NLS-1$
             "", JOptionPane.YES_NO_CANCEL_OPTION)) {  //$NON-NLS-1$
        case JOptionPane.YES_OPTION:
          save();
          break;
        case JOptionPane.CANCEL_OPTION:
        case JOptionPane.CLOSED_OPTION:
          cancelled = true;
        }
      }
      for (ModuleExtension ext : getComponentsOf(ModuleExtension.class)) {
        cancelled = !ext.confirmExit();
      }
    }

    if (!cancelled) {
      Prefs p = null;

      // write and close module prefs
      try {
        p = getPrefs();
        p.write();
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, p.getFile());
      }
      finally {
        if (p != null) {
          try {
            p.close();
          }
          catch (IOException e) {
            log.error("Error while closing module preferences", e);
          }
        }
      }

      // TODO remove this code if it is not needed anymore
      // write and close global prefs
      // Bug 10179 - Global prefs are now written out each time a preference is changed
      // try {
      //   p = getGlobalPrefs();
      //   p.write();
      //   p.close();
      // }
      // catch (IOException e) {
      //   WriteErrorDialog.error(e, p.getFile());
      // }
      // finally {
      //  IOUtils.closeQuietly(p);
      // }

      // close the module
      try {
        archive.close();
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, archive.getName());
      }

      log.info("Exiting");
    }

    return !cancelled;
  }

  /**
   * Encode the {@link Command}, send it to the server and write it
   * to a logfile (if any is open)
   *
   * @see #encode
   */
  public void sendAndLog(Command c) {
    if (c != null && !c.isNull()) {
      synchronized(loggingLock) {
        if (loggingPaused) {
          if (pausedCommands == null) {
            pausedCommands = c;
          }
          else {
            pausedCommands.append(c);
          }
        }
        else {
          getServer().sendToOthers(c);
          getLogger().log(c);
        }
      }
    }
  }

  /**
   * Pause logging and return true if successful.
   * Return false if logging already paused
   *
   * While Paused, commands are accumulated into pausedCommands so that they
   * can all be logged at the same time, and generate a single UNDO command.
   *
   * @return
   */
  public boolean pauseLogging() {
    synchronized(loggingLock) {
      if (loggingPaused) {
        return false;
      }
      loggingPaused = true;
      pausedCommands = null;
      return true;
    }
  }

  /**
   * Restart logging and return any outstanding commands
   */
  public Command resumeLogging() {
    Command c = null;
    synchronized(loggingLock) {
      c = pausedCommands == null ? new NullCommand() : pausedCommands;
      pausedCommands = null;
      loggingPaused = false;
    }
    return c;
  }

  /**
   * Clear outstanding Commands
   * Use where the calling level handles the sending of outstanding commands
   */
  public void clearPausedCommands() {
    pausedCommands = null;
  }


  private static String userId = null;

  /**
   * @return a String that uniquely identifies the user
   */
  public static String getUserId() {
    return userId;
  }

  /**
   * Set the identifier for the user
   */
  public static void setUserId(String newId) {
    userId = newId;
  }

  /**
   * @return the object reponsible for sending messages to the server
   */
  public ServerConnection getServer() {
    return server;
  }

  /**
   * Set the singleton GameModule and invoke {@link #build} on it.
   */
  public static void init(GameModule module) throws IOException {
    if (theModule != null) {
      throw new UnsupportedOperationException(
        Resources.getString("GameModule.open_error",
          theModule.getDataArchive().getName()));
    }
    else {
      theModule = module;
      theModule.setGpIdSupport(theModule);
      try {
        theModule.build();
      }
      catch (IOException e) {
        theModule = null;
        throw e;
      }
    }
    
    /*
     *  If we are editing, check for duplicate, illegal or missing GamePiece Id's
     *  and update if necessary.
     */
    if (theModule.getDataArchive() instanceof ArchiveWriter) {
      theModule.checkGpIds();
    }
    
    //Save our old drag threshold
    oldDragThreshold = System.getProperty("awt.dnd.drag.threshold");
    System.setProperty("awt.dnd.drag.threshold", Integer.toString(GlobalOptions.getInstance().getDragThreshold()));

    /*
     * Tell any Plugin components that the build is complete so that they
     * can finish initialization.
     */
    for (Plugin plugin : theModule.getComponentsOf(Plugin.class)) {
      plugin.init();
    }
  }

  /**
   * Unload the module
   */
  public static void unload() {
    
    // Put our old drag threshold back, or if it wasn't set then return it to an unset state.
    if (oldDragThreshold != null) {
      System.setProperty("awt.dnd.drag.threshold", oldDragThreshold);      
    }
    else {
      System.clearProperty("awt.dnd.drag.threshold");            
    }
    
    if (theModule != null) {
      if (theModule.shutDown()) {
        theModule = null;
      }
    }
  }

  // Saved the current buildString for comparison when we try and quit.
  public void updateLastSave() {
    lastSavedConfiguration = buildString();
  }

  @Override
  public String generateGpId() {
    return String.valueOf(nextGpId++);
  }

  @Override
  public int getNextGpId() {
    return nextGpId;
  }

  @Override
  public void setNextGpId(int id) {
    nextGpId = id;
  }

  public void setGpIdSupport(GpIdSupport s) {
    gpidSupport = s;
  }

  public GpIdSupport getGpIdSupport() {
    return gpidSupport;
  }

  /**
   * Check every PieceSlot and PlaceMarker trait for duplicate,
   * illegal or Missing GamePiece id's and update them if necessary
   */
  protected void checkGpIds() {
    final GpIdChecker checker = new GpIdChecker(this);
    for (PieceSlot pieceSlot : theModule.getAllDescendantComponentsOf(PieceSlot.class)) {
      checker.add(pieceSlot);
    }
    checker.fixErrors();
  }

  /**
   * @return the object which stores data for the module
   */
  public DataArchive getDataArchive() {
    return archive;
  }

  /**
   * If the module is being edited, return the writeable archive for the module
   */
  public ArchiveWriter getArchiveWriter() {
    return archive.getWriter();
  }

  public ImageTileSource getImageTileSource() {
    if (tcache == null) {
      // FIXME: There's no guarantee that getGameName() and getGameVersion()
      // are properly set at this point.

      final String hstr =
        DigestUtils.sha1Hex(getGameName() + "_" + getGameVersion());

      final File tc = new File(Info.getConfDir(), "tiles/" + hstr);
      tcache = new ImageTileDiskCache(tc.getAbsolutePath());
    }

    return tcache;
  }

  /**
   * Is the module being translated into the user's Locale?  Localization is disabled when editing a module
   *
   * @return true if the module/extension has been localized
   */
  public boolean isLocalizationEnabled() {
    return getArchiveWriter() == null;
  }

  /**
   * @return the singleton instance of GameModule
   */
  public static GameModule getGameModule() {
    return theModule;
  }

  /**
   * Return the object responsible for tracking the state of a game.
   * Only one game in progress is allowed;
   */
  public GameState getGameState() {
    return theState;
  }

  public void saveAs() {
    save(true);
  }

  /**
   * If the module is being edited, write the module data
   */
  public void save() {
    save(false);
  }

  protected void save(boolean saveAs) {
    vassalVersionCreated = Info.getVersion();

    final ArchiveWriter writer = getArchiveWriter();

    try {
      (new ModuleMetaData(this)).save(writer);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, writer.getName());
    }

    try {
      final String save = buildString();
      writer.addFile(BUILDFILE,
        new ByteArrayInputStream(save.getBytes(StandardCharsets.UTF_8)));

      if (saveAs) writer.saveAs(true);
      else writer.save(true);

      lastSavedConfiguration = save;
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, writer.getName());
    }
  }

  protected String buildString() {
    org.w3c.dom.Document doc = Builder.createNewDocument();
    doc.appendChild(getBuildElement(doc));
    return Builder.toString(doc);
  }

  /**
   * Return values of Global properties
   */
  @Override
  public Object getProperty(Object key) {
    if (GlobalOptions.PLAYER_SIDE.equals(key) || GlobalOptions.PLAYER_SIDE_ALT.equals(key)) {
      String mySide = PlayerRoster.getMySide();
      return mySide == null ? "" : mySide;  //$NON-NLS-1$
    }
    else if (GlobalOptions.PLAYER_NAME.equals(key) || GlobalOptions.PLAYER_NAME_ALT.equals(key)) {
      return getPrefs().getValue(GameModule.REAL_NAME);
    }
    else if (GlobalOptions.PLAYER_ID.equals(key) || GlobalOptions.PLAYER_ID_ALT.equals(key)) {
      return GlobalOptions.getInstance().getPlayerId();
    }
    MutableProperty p = propsContainer.getMutableProperty(String.valueOf(key));
    return p == null ? null : p.getPropertyValue();
  }

  @Override
  public MutableProperty getMutableProperty(String name) {
    return propsContainer.getMutableProperty(name);
  }

  @Override
  public void addMutableProperty(String key, MutableProperty p) {
    propsContainer.addMutableProperty(key, p);
    p.addMutablePropertyChangeListener(repaintOnPropertyChange);
  }

  @Override
  public MutableProperty removeMutableProperty(String key) {
    MutableProperty p = propsContainer.removeMutableProperty(key);
    if (p != null) {
      p.removeMutablePropertyChangeListener(repaintOnPropertyChange);
    }
    return p;
  }

  @Override
  public String getMutablePropertiesContainerId() {
    return "Module";
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (GlobalOptions.PLAYER_SIDE.equals(key) || GlobalOptions.PLAYER_SIDE_ALT.equals(key)) {
      String mySide = PlayerRoster.getMyLocalizedSide();
      return mySide == null ? "" : mySide;  //$NON-NLS-1$
    }
    else {
      return getProperty(key);
    }
  }

  public long getCrc() {
    if (crc == null) {
      crc = buildCrc();
    }
    return crc;
  }

  protected Long buildCrc() {
    final List<File> files = new ArrayList<>();
    if (getDataArchive().getArchive() != null) {
      files.add(new File(getDataArchive().getName()));
    }

    for (ModuleExtension ext : getComponentsOf(ModuleExtension.class)) {
      if (ext.getDataArchive().getArchive() != null) {
        files.add(new File(ext.getDataArchive().getName()));
      }
    }

    try {
      return CRCUtils.getCRC(files);
    }
    catch (IOException e) {
      log.error("Error generating CRC", e);
      return 0L;
    }
  }

  @Override
  public ComponentI18nData getI18nData() {
    ComponentI18nData myI18nData = super.getI18nData();
    myI18nData.setAttributeTranslatable(MODULE_VERSION, false);
    return myI18nData;
  }

  /**
   * @return the {@link PlayerRoster} instance, or <code>null</code> if no {@link PlayerRoster} exists
   * within this {@link GameModule}
   */
  public PlayerRoster getPlayerRoster() {
    return getComponentsOf(PlayerRoster.class).stream()
                                              .findFirst()
                                              .orElse(null);
  }

  public void addSideChangeListenerToPlayerRoster(PlayerRoster.SideChangeListener l) {
    PlayerRoster r = getPlayerRoster();
    if (r != null) {
      r.addSideChangeListenerToInstance(l);
    }
  }
}
