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

import java.awt.Container;
import java.awt.FileDialog;
import java.awt.Rectangle;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.NoSuchFileException;
import java.security.SecureRandom;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.Deque;

import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;

import VASSAL.build.module.properties.GlobalTranslatableMessages;
import VASSAL.build.module.properties.TranslatableString;
import VASSAL.build.module.properties.TranslatableStringContainer;
import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;

import org.slf4j.LoggerFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.Info;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.BasicLogger;
import VASSAL.build.module.ChartWindow;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.ChessClockControl;
import VASSAL.build.module.Console;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.DoActionButton;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.GameRefresher;
import VASSAL.build.module.GameState;
import VASSAL.build.module.GlobalKeyCommand;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Inventory;
import VASSAL.build.module.KeyNamer;
import VASSAL.build.module.Map;
import VASSAL.build.module.ModuleExtension;
import VASSAL.build.module.MultiActionButton;
import VASSAL.build.module.NotesWindow;
import VASSAL.build.module.PieceWindow;
import VASSAL.build.module.PlayerHand;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.Plugin;
import VASSAL.build.module.PluginsLoader;
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
import VASSAL.build.module.gamepieceimage.GamePieceImageDefinitions;
import VASSAL.build.module.metadata.AbstractMetaData;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.ModuleMetaData;
import VASSAL.build.module.properties.ChangePropertyCommandEncoder;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.build.module.turn.TurnTracker;
import VASSAL.build.widget.PieceSlot;
import VASSAL.chat.AddressBookServerConfigurer;
import VASSAL.chat.ChatServerFactory;
import VASSAL.chat.DynamicClient;
import VASSAL.chat.HybridClient;
import VASSAL.chat.node.NodeClientFactory;
import VASSAL.chat.node.OfficialNodeClientFactory;
import VASSAL.chat.node.PrivateNodeClientFactory;
import VASSAL.chat.peer2peer.P2PClientFactory;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.Logger;
import VASSAL.command.NullCommand;
import VASSAL.configure.AutoConfigurer;
import VASSAL.configure.CompoundValidityChecker;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.MandatoryComponent;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.configure.password.ToggleablePasswordConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.ComponentI18nData;
import VASSAL.i18n.Language;
import VASSAL.i18n.Localization;
import VASSAL.i18n.Resources;
import VASSAL.i18n.I18nResourcePathFinder;
import VASSAL.launch.PlayerWindow;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.Prefs;
import VASSAL.script.expression.Expression;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.CRCUtils;
import VASSAL.tools.DataArchive;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.QuickColors;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.ReflectionUtils;
import VASSAL.tools.ResourcePathFinder;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.ToolBarComponent;
import VASSAL.tools.WarningDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.image.ImageTileSource;
import VASSAL.tools.image.tilecache.ImageTileDiskCache;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SwingUtils;
import VASSAL.tools.version.VersionUtils;

import static VASSAL.preferences.Prefs.MAIN_WINDOW_HEIGHT;
import static VASSAL.preferences.Prefs.MAIN_WINDOW_WIDTH;
import static VASSAL.preferences.Prefs.MAIN_WINDOW_REMEMBER;

/**
 * The GameModule class is the base class for a VASSAL module.  It is
 * the root of the {@link Buildable} containment hierarchy, and thus also
 * the root of the Editor's {@link ConfigureTree}. GameModule's implementation
 * of {@link CommandEncoder} is slightly different, serving as a central dispatch
 * point for all other CommandEncoders, which register themselves with GameModule.
 *
 * Components which are intended to be added directly to the GameModule are contained
 * in the <code>VASSAL.build.module</code> package.
 *
 * <p>GameModule is a <a href="https://en.wikipedia.org/wiki/Singleton_pattern">singleton</a>, and contains access points for many other classes,
 * such as {@link DataArchive}, {@link ServerConnection}, {@link Logger}, {@link Chatter}, and {@link Prefs}.</p>
 */
public class GameModule extends AbstractConfigurable
  implements CommandEncoder, ToolBarComponent, PropertySource, MutablePropertiesContainer, TranslatableStringContainer, GpIdSupport {

  private static final org.slf4j.Logger log = LoggerFactory.getLogger(GameModule.class);

  private static final String DEFAULT_NAME = "Unnamed module";  //$NON-NLS-1$

  public static final String MODULE_NAME = "name";  //$NON-NLS-1$
  public static final String MODULE_VERSION = "version";  //$NON-NLS-1$
  public static final String DESCRIPTION = "description"; //NON-NLS
  public static final String VASSAL_VERSION_CREATED = "VassalVersion";  //$NON-NLS-1$
  /**
   * The System property of this name will return a version identifier for the version of VASSAL being run
   */
  public static final String VASSAL_VERSION_RUNNING = "runningVassalVersion";  //$NON-NLS-1$
  public static final String NEXT_PIECESLOT_ID = "nextPieceSlotId"; //NON-NLS
  public static final String BUILDFILE = "buildFile.xml"; //NON-NLS
  public static final String BUILDFILE_OLD = "buildFile"; //NON-NLS

  /** The {@link Prefs} key for the user's real name */
  public static final String REAL_NAME = "RealName"; //$NON-NLS-1$
  /** The {@link Prefs} key for the user's secret name */
  public static final String SECRET_NAME = "SecretName"; //$NON-NLS-1$
  /** The {@link Prefs} key for the user's personal info */
  public static final String PERSONAL_INFO = "Profile"; //$NON-NLS-1$

  public static final String MODULE_NAME_PROPERTY = "ModuleName"; //NON-NLS
  public static final String MODULE_VERSION_PROPERTY = "ModuleVersion"; //NON-NLS
  public static final String MODULE_DESCRIPTION_PROPERTY = "ModuleDescription"; //NON-NLS
  public static final String MODULE_OTHER1_PROPERTY = "ModuleOther1"; //NON-NLS
  public static final String MODULE_OTHER2_PROPERTY = "ModuleOther2"; //NON-NLS
  public static final String MODULE_VASSAL_VERSION_CREATED_PROPERTY = "VassalVersionCreated"; //NON-NLS
  public static final String MODULE_VASSAL_VERSION_RUNNING_PROPERTY = "VassalVersionRunning"; //NON-NLS

  public static final String MODULE_CURRENT_LOCALE = "CurrentLanguage"; //NON-NLS
  public static final String MODULE_CURRENT_LOCALE_NAME = "CurrentLanguageName"; //NON-NLS

  private static final char COMMAND_SEPARATOR = KeyEvent.VK_ESCAPE;

  /**
   * Last type of game save/load for our current game
   */
  public enum GameFileMode {
    SAVED_GAME("saved"), //NON-NLS
    LOADED_GAME("loaded"), //NON-NLS
    REPLAYED_GAME("replayed"), //NON-NLS
    REPLAYING_GAME("replaying"), //NON-NLS
    LOGGING_GAME("logging"), //NON-NLS
    LOGGED_GAME("logged"), //NON-NLS
    NEW_GAME("new"); //NON-NLS

    private final String prettyName;

    GameFileMode(String prettyName) {
      this.prettyName = prettyName;
    }

    @Override
    public String toString() {
      return prettyName;
    }
  }

  private static String userId = null;

  private static GameModule theModule;

  private static String DEFAULT_MODULE_VERSION = "0.0"; //$NON-NLS-1$

  private String moduleVersion = DEFAULT_MODULE_VERSION;
  private String vassalVersionCreated = "0.0";  //$NON-NLS-1$
  private String moduleOther1 = "";
  private String moduleOther2 = "";
  private String gameName = DEFAULT_NAME;
  private String localizedGameName = null;
  private String description = "";
  private String lastSavedConfiguration;
  private FileChooser fileChooser;
  private FileDialog fileDialog;
  private final MutablePropertiesContainer propsContainer = new MutablePropertiesContainer.Impl();
  private final TranslatableStringContainer transContainer = new TranslatableStringContainer.Impl();

  private final PropertyChangeListener repaintOnPropertyChange =
    evt -> {
      for (final Map map : Map.getMapList()) {
        map.repaint();
      }
    };

  private final PlayerWindow frame = new PlayerWindow();

  /**
   * Will hold the main module toolbar
   *
   * @deprecated use {@link #getPlayerWindow()} and {@link PlayerWindow#getControlPanel()} instead.
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  private final JPanel controlPanel = frame.getControlPanel();

  /**
   * Reads/writes full game state; starts/stops gameplay.
   */
  private GameState theState;

  /**
   * Our "zip" archive with a .vmod file extension
   */
  private final DataArchive archive;

  /**
   * Our finder of the resources, for translation of images.
   */
  private final ResourcePathFinder resourceFinder;
  
  /**
   * The user preferences
   */
  private Prefs preferences;

  /**
   * For creating .vlog log files (playbacks) of PBEM games
   */
  private Logger logger;

  /**
   * The Chat Log window
   */
  private Chatter chat;

  /**
   * The Chat Log Console
   */
  private final Console console = new Console();

  /**
   * Docked PieceWindow (we need to know which one to get our splitters all splatting in the right order)
   */
  private PieceWindow pieceWindow = null;

  /**
   * Random number generator
   */
  private final Random RNG = new SecureRandom();

  /**
   * Server object for online games
   */
  private ServerConnection server;

  /**
   * Chat server controls
   */
  private ChatServerControls serverControls;

  /**
   * Manages the tiling of large map images
   */
  private ImageTileSource tcache;

  /**
   * For the startup Wizard
   */
  private WizardSupport wizardSupport;

  /**
   * This manages the changing of player names in online connections
   */
  private PropertyChangeSupport idChangeSupport;

  private final List<KeyStrokeSource> keyStrokeSources = new ArrayList<>();
  private final List<KeyStrokeListener> keyStrokeListeners = new ArrayList<>();

  private CommandEncoder[] commandEncoders = new CommandEncoder[0];
  private final List<String> deferredChat = new ArrayList<>();

  private boolean loggingPaused = false;
  private final Object loggingLock = new Object();
  private final Deque<Command> pausedCommands = new ArrayDeque<>();

  private String gameFile = ""; //NON-NLS
  private GameFileMode gameFileMode = GameFileMode.NEW_GAME;

  private boolean iFeelDirty = false; // Touched the module in ways not detectable by buildString compare

  /**
   * Store the currently building GpId source. Only meaningful while
   * the GameModule or an Extension is actually in the process of being built
   * during module/extension load.
   */
  private GpIdSupport gpidSupport = null;

  /**
   * Next GamePiece ID available.
   */
  private int nextGpId = 0;
  private Long crc = null;

  /**
   * Error Logging to {@link Chatter}?
   */
  private static boolean errorLogToChat = false;

  private boolean loadOverSemaphore = false; // if we're currently loading overtop of another game (so don't disturb UI if possible, it will get a setup(true) soon enough)
  /**
   * @param state - true if we're loading-over-top of an existing game (so don't disturb UI elements if possible, will receive a setup(true) soon enough)
   */
  public void setLoadOverSemaphore(boolean state) {
    loadOverSemaphore = state;
  }

  /**
   * @return true if we're loading-over-top of an existing game (so don't disturb UI elements if possible, will receive a setup(true) soon enough)
   */
  public boolean isLoadOverSemaphore() {
    return loadOverSemaphore;
  }


  private boolean refreshingSemaphore = false; // if we're currently loading a game just to refresh its (not to play it)
  /**
   * @param state - true if refreshing (suppresses GameState.setup method)
   */
  public void setRefreshingSemaphore(boolean state) {
    refreshingSemaphore = state;
  }

  /**
   * @return true if refreshing (suppresses GameState.setup method)
   */
  public boolean isRefreshingSemaphore() {
    return refreshingSemaphore;
  }


  private boolean loadingContinuationSemaphore = false; // if we're currently loading a game as continuation
  /**
   * @param state - true if loading game as continuation (don't thrash listeners)
   */
  public void setLoadingContinuationSemaphore(boolean state) {
    loadingContinuationSemaphore = state;
  }

  /**
   * @return true if loading game as continuation (don't thrash listeners)
   */
  public boolean isLoadingContinuationSemaphore() {
    return loadingContinuationSemaphore;
  }



  /**
   * @return the top-level frame of the controls window
   * @deprecated use {@link #getPlayerWindow()}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public JFrame getFrame() {
    ProblemDialog.showDeprecated("2020-08-06");  //NON-NLS
    return frame;
  }

  /**
   * @return String identifier for module
   */
  @Override
  public String toString() {
    return "BasicModule{" +                                       //NON-NLS
      "name='" + name + '\'' +                                    //NON-NLS
      ", moduleVersion='" + moduleVersion + '\'' +                //NON-NLS
      '}';
  }

  /**
   * "Player" in this context meaning the VASSAL Player, i.e. the main module window, as opposed to any
   * individual player meaning participant-in-a-boardgame.
   *
   * @return The main window for the module
   */
  public PlayerWindow getPlayerWindow() {
    return frame;
  }

  /**
   * Sets the proper name for module window's title bar
   */
  public void initFrameTitle() {
    frame.setTitle(getTitleString());
  }

  /**
   * @return Our wizard
   */
  public WizardSupport getWizardSupport() {
    if (wizardSupport == null) {
      wizardSupport = new WizardSupport();
    }
    return wizardSupport;
  }

  /**
   * @return our ChatServerControls.
   */
  public ChatServerControls getServerControls() {
    return serverControls;
  }

  /**
   * @return our Console
   */
  public Console getConsole() {
    return console;
  }

  /**
   * @return True if errorLog should also be sent to chat log.
   */
  public static boolean isErrorLogToChat() {
    return errorLogToChat;
  }

  /**
   * @param errorLogToChat true if errorLog should also be sent to chat log
   */
  public static void setErrorLogToChat(boolean errorLogToChat) {
    GameModule.errorLogToChat = errorLogToChat;
  }

  /*
   * The module I18n key prefix is null for the top level.
   */
  @Override
  public String getI18nPrefix() {
    return ""; //NON-NLS
  }


  public void setDirty(boolean touchThis) {
    iFeelDirty = touchThis;
  }

  /**
   * Constructor for a GameModule.
   * @param archive The .vmod (or .tmp) archive to associate
   */
  public GameModule(DataArchive archive) {
    this.archive = archive;
    final boolean isEditing = (archive instanceof ArchiveWriter);
    resourceFinder = new I18nResourcePathFinder(archive, isEditing ? "en" : Resources.getLocale().getLanguage());

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
   * Builds the module's component hierarchy from the XML buildFile, or if the module file does not
   * yet exist, builds the default "new module" hierarchy.
   *
   * Method is package-private for testing purposes.
   *
   * @throws IOException IOException
   */
  void build() throws IOException {
    final DataArchive darch = getDataArchive();

    final File f = new File(darch.getName());
    if (!f.exists() || f.length() == 0) {
      // new module, no buildFile
      build(null);
    }
    else {
      final AbstractMetaData data = MetaDataFactory.buildMetaData(f);

      if (!(data instanceof ModuleMetaData)) {
        throw new IOException(
          Resources.getString("BasicModule.not_a_module"), //$NON-NLS-1$
          null);
      }

      final String properBuildFileName = (VersionUtils.compareVersions(VersionUtils.truncateToMinorVersion(data.getVassalVersion()), "3.5") < 0) ? BUILDFILE_OLD : BUILDFILE;

      // existing module
      try (InputStream inner = darch.getInputStream(properBuildFileName);
           BufferedInputStream in = new BufferedInputStream(inner)) {
        final Document doc = Builder.createDocument(in);
        build(doc.getDocumentElement());
      }
      catch (FileNotFoundException | NoSuchFileException e) {
        throw new IOException(
          Resources.getString("BasicModule.no_buildfile"), //$NON-NLS-1$
          e);
      }
      catch (IOException e) {
        throw new IOException(
          Resources.getString("BasicModule.io_error_reading_archive"), //$NON-NLS-1$
          e);
      }
    }

    // If a dockable PieceWindow got registered, dock it now (since we're
    // sure the Chatter will already be safely docked by this point)
    final PieceWindow pw = getPieceWindow();
    if (pw != null) {
      pw.dockMe();
    }

    MenuManager.getInstance().addAction(
      "Prefs.edit_preferences", //NON-NLS
      getPrefs().getEditor().getEditAction()
    );

    // Our counter refresher
    final GameRefresher gameRefresher = new GameRefresher(this);
    gameRefresher.addTo(this);
    MenuManager.getInstance().addAction(
      "GameRefresher.refresh_counters", //NON-NLS
      gameRefresher.getRefreshAction()
    );
  }

  /**
   * Builds the module's component hierarchy from a given XML element, or a null one is given initializes
   * a brand new default "new module" hierarchy.
   * @param e XML element to build from, or null to build the default hierarchy
   */
  @Override
  public void build(Element e) {
    /*
     * We determine the name of the module at the very beginning, so we
     * know which preferences to read.
     */
    if (e != null) {
      gameName = e.getAttribute(MODULE_NAME);
      if (e.getAttribute(VASSAL_VERSION_CREATED).length() > 0) {
        vassalVersionCreated = e.getAttribute(VASSAL_VERSION_CREATED);
      }
    }

    initIdentityPreferences();
    Prefs.initSharedGlobalPrefs();
    initGameState();
    initLogger();
    initServer();
    new PluginsLoader().addTo(this);
    if (e != null) {
      super.build(e);
      ensureComponent(GamePieceImageDefinitions.class);
      ensureComponent(GlobalProperties.class);
      ensureComponent(GlobalTranslatableMessages.class);
      ensureComponent(Language.class);
      ensureComponent(BasicCommandEncoder.class);
      ensureComponent(Documentation.class);
      ensureComponent(PlayerRoster.class);
      ensureComponent(GlobalOptions.class);
      ensureComponent(PrototypesContainer.class);
      ensureComponent(Chatter.class);
      ensureComponent(KeyNamer.class);
    }
    else {
      buildDefaultComponents();
    }
    initFrame();
  }

  /**
   * Associates our user identity with the module's preferences.
   */
  private void initIdentityPreferences() {
    idChangeSupport = new PropertyChangeSupport(this);
    final StringConfigurer fullName = new StringConfigurer(GameModule.REAL_NAME, Resources.getString("Prefs.name_label"), Resources.getString("Prefs.newbie"));   //$NON-NLS-1$ //$NON-NLS-2$
    fullName.addPropertyChangeListener(evt -> idChangeSupport.firePropertyChange(evt));
    final TextConfigurer profile = new TextConfigurer(GameModule.PERSONAL_INFO, Resources.getString("Prefs.personal_info"), "");   //$NON-NLS-1$ //$NON-NLS-2$
    profile.addPropertyChangeListener(evt -> idChangeSupport.firePropertyChange(evt));
    final ToggleablePasswordConfigurer user = new ToggleablePasswordConfigurer(GameModule.SECRET_NAME, Resources.getString("Prefs.password_label"), Resources.getString("Prefs.password_prompt", System.getProperty("user.name"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    user.addPropertyChangeListener(evt -> GameModule.setUserId((String) evt.getNewValue()));
    getPrefs().addOption(Resources.getString("Prefs.personal_tab"), fullName);   //$NON-NLS-1$ //$NON-NLS-2$
    getPrefs().addOption(Resources.getString("Prefs.personal_tab"), user);   //$NON-NLS-1$ //$NON-NLS-2$
    getPrefs().addOption(Resources.getString("Prefs.personal_tab"), profile);  //$NON-NLS-1$
    GameModule.setUserId(user.getValueString());
  }

  /**
   * Initialize and register our multiplayer server controls
   */
  private void initServer() {
    final OfficialNodeClientFactory oncf = new OfficialNodeClientFactory();

    ChatServerFactory.register(OfficialNodeClientFactory.OFFICIAL_TYPE, oncf);
    ChatServerFactory.register(PrivateNodeClientFactory.PRIVATE_TYPE, new PrivateNodeClientFactory());
    ChatServerFactory.register(P2PClientFactory.P2P_TYPE, new P2PClientFactory());

    // legacy server used to be stored as node type
    ChatServerFactory.register(NodeClientFactory.NODE_TYPE, oncf);
    // redirect removed jabber type to official server
    ChatServerFactory.register("jabber", oncf); //NON-NLS

    server = new DynamicClient();
    final AddressBookServerConfigurer config = new AddressBookServerConfigurer("ServerSelected", "", (HybridClient) server); //NON-NLS
    Prefs.getGlobalPrefs().addOption(Resources.getString("Chat.server"), config); //$NON-NLS-1$
    serverControls = new ChatServerControls();
    serverControls.addTo(this);
  }

  /**
   * Initialize and register our "logger", which allows player commands to be recorded into a .vlog file for PBEM games.
   */
  private void initLogger() {
    logger = new BasicLogger();
    ((BasicLogger) logger).build(null);
    ((BasicLogger) logger).addTo(this);
  }

  /**
   * Initialize and register a record of our GameState, which provides methods for saving/loading our entire enumerated game state,
   * along with controls for "starting" and "ending" gameplay.
   */
  private void initGameState() {
    theState = new GameState();
    theState.addTo(this);
    addCommandEncoder(theState);
  }

  /**
   * Adds the standard default components for a brand new module.
   */
  private void buildDefaultComponents() {
    addComponent(BasicCommandEncoder.class);
    addComponent(Documentation.class);
    addComponent(PlayerRoster.class);
    addComponent(GlobalOptions.class);
    addComponent(Map.class);
    addComponent(GamePieceImageDefinitions.class);
    addComponent(GlobalProperties.class);
    addComponent(GlobalTranslatableMessages.class);
    addComponent(PrototypesContainer.class);
    addComponent(PieceWindow.class);
    addComponent(Chatter.class);
    addComponent(KeyNamer.class);
    addComponent(Language.class);
  }

  /**
   * Initializes our actual window frame -- size, title bar. Send a message with our module name and version number
   * to the chat log, to be displayed there once a Chatter is registered.
   */
  private void initFrame() {
    final Rectangle screen = SwingUtils.getScreenBounds(frame);

    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      frame.setLocation(screen.getLocation());

      final Prefs p = Prefs.getGlobalPrefs();

      // If not "remembering" window size, nuke the pref
      if (Boolean.FALSE.equals(p.getOption(MAIN_WINDOW_REMEMBER).getValue())) {
        p.getOption(MAIN_WINDOW_WIDTH).setValue(-1);
        p.getOption(MAIN_WINDOW_HEIGHT).setValue(-1);
      }

      // Read window size prefs
      final int ph = (Integer) p.getOption(MAIN_WINDOW_HEIGHT).getValue();
      final int pw = (Integer) p.getOption(MAIN_WINDOW_WIDTH).getValue();

      // Use pref if valid, otherwise screen dimensions
      final int h = (ph > 0) ? ph : screen.height;
      final int w = (pw > 0) ? pw : screen.width;

      // Before we have a map, we use 1/3 of height
      frame.setSize(w, h / 3);
    }
    else {
      final String key = "BoundsOfGameModule"; //$NON-NLS-1$
      final Rectangle r = new Rectangle(0, 0, screen.width, screen.height / 4);
      getPrefs().addOption(new PositionOption(key, frame, r));
    }

    final String mess = Resources.getString(
      "BasicModule.version_message", getLocalizedGameName(), moduleVersion); //$NON-NLS-1$
    warn(mess);
    log.info(mess);
    initFrameTitle();
  }

  /**
   * Ensures that the module contains at least one component of the specified class.
   * This is used to protect modules from accidental deletion of necessary (or potentially necessary)
   * objects for which there is no easy interface provided to restore them (e.g. the PlayerRoster)
   * @param componentClass a subcomponent class. If the module has no subcomponents of that class, a nice fresh new one is added.
   */
  private void ensureComponent(Class<? extends Buildable> componentClass) {
    if (getComponentsOf(componentClass).isEmpty()) {
      addComponent(componentClass);
    }
  }

  /**
   * Adds a subcomponent of the specified class to the module hierarchy. "Build" the
   * child in its default form, and register it with the module.
   * @param componentClass a subcomponent class, to be added.
   */
  private void addComponent(Class<? extends Buildable> componentClass) {
    Buildable child = null;
    try {
      child = componentClass.getConstructor().newInstance();
    }
    catch (Throwable t) {
      ReflectionUtils.handleNewInstanceFailure(t, componentClass);
    }

    if (child != null) {
      child.build(null);
      child.addTo(this);
      add(child);
    }
  }

  /**
   * Sets a buildFile (XML) attribute value for this component.
   * @param name the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @param value If the <code>value</code> parameter is a String, it will be the value returned by {@link #getAttributeValueString} for the same
   *              <code>key</code>. If the implementing class extends {@link AbstractConfigurable}, then <code>value</code> can also be an instance of
   */
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
      final String runningVersion = Info.getVersion();
      if (VersionUtils.compareVersions(vassalVersionCreated, runningVersion) > 0) {
        WarningDialog.show("GameModule.version_warning", //NON-NLS
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
    else if (MODULE_OTHER1_PROPERTY.equals(name)) {
      moduleOther1 = (String) value;
    }
    else if (MODULE_OTHER2_PROPERTY.equals(name)) {
      moduleOther2 = (String) value;
    }
  }

  /**
   * @param name the name of the attribute. Will be one of those listed in {@link #getAttributeNames}
   * @return a String representation of the attribute with the given name. When initializing a module, this String value will be passed to {@link #setAttribute}
   */
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
    else if (MODULE_OTHER1_PROPERTY.equals(name)) {
      return moduleOther1;
    }
    else if (MODULE_OTHER2_PROPERTY.equals(name)) {
      return moduleOther2;
    }
    return null;
  }

  /**
   * A valid version format is "w.x.y[bz]", where
   * 'w','x','y', and 'z' are integers.
   * @return a negative number if <code>v2</code> is a later version
   * the <code>v1</code>, a positive number if an earlier version,
   * or zero if the versions are the same.
   *
   * @deprecated use {@link VersionUtils#compareVersions(String, String)}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public static int compareVersions(String v1, String v2) {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    return VersionUtils.compareVersions(v1, v2);
  }

  /**
   * Game Module is normally at the root of the hierarchy, so it doesn't expect to get added to anything.
   * @param b Notional "parent" of this GameModule
   */
  @Override
  public void addTo(Buildable b) {
  }

  /**
   * Gets the generic name for this type of class across all instances of it. Appears
   * in the Editor window in [..] as e.g. [Map], [Prototype], etc.
   * @return The generic name for this kind of component, i.e. the part appearing [In Brackets] in the Editor's {@link ConfigureTree}.
   */
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GameModule.component_type");  //$NON-NLS-1$
  }

  /**
   * Since we aren't expecting to be {@link #addTo}'ed to a parent, we likewise don't need to do much about being "removeFrom'ed" one.
   * @param parent Notional "parent" of this GameModule
   */
  @Override
  public void removeFrom(Buildable parent) {
  }

  /**
   * @return a HelpFile describing how to use and configure this component
   */
  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.html");  //$NON-NLS-1$
  }

  /**
   * Lists all the buildFile (XML) attribute names for this component.
   * If this component is ALSO an {@link AbstractConfigurable}, then this list of attributes determines the appropriate
   * attribute order for {@link AbstractConfigurable#getAttributeDescriptions()} and {@link AbstractConfigurable#getAttributeTypes()}.
   * @return a list of all buildFile (XML) attribute names for this component
   * @see AbstractBuildable
   */
  @Override
  public String[] getAttributeNames() {
    return new String[]{
      MODULE_NAME,
      MODULE_VERSION,
      DESCRIPTION,
      MODULE_OTHER1_PROPERTY,
      MODULE_OTHER2_PROPERTY,
      VASSAL_VERSION_CREATED,
      NEXT_PIECESLOT_ID,
    };
  }

  /**
   * @return an array of Strings describing the buildFile (XML) attributes of this component. These strings are used as prompts in the
   * Properties window for this object, when the component is configured in the Editor. The order of descriptions should
   * be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   * @see AbstractConfigurable
   */
  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString("Editor.GameModule.name_label"),    //$NON-NLS-1$
      Resources.getString("Editor.GameModule.version_label"), //$NON-NLS-1$
      Resources.getString("Editor.GameModule.description_label"),    //NON-NLS
      Resources.getString("Editor.GameModule.module_other_1_label"), //NON-NLS
      Resources.getString("Editor.GameModule.module_other_2_label") //NON-NLS
    };
  }

  /**
   * @return the Class for the buildFile (XML) attributes of this component. Valid classes include: String, Integer, Double, Boolean, Image,
   * Color, and KeyStroke, along with any class for which a Configurer exists in VASSAL.configure. The class determines, among other things,
   * which type of {@link AutoConfigurer} will be used to configure the attribute when the object is configured in the Editor.
   *
   * The order of classes should be the same as the order of names in {@link AbstractBuildable#getAttributeNames}
   * @see AbstractConfigurable
   */
  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
      String.class,
      String.class,
      String.class
    };
  }

  /**
   * List of subcomponents which can be added to a GameModule.
   *
   * @return a list of valid sub-component Classes.  If a Class
   * appears in this list, then instances of that class may be added
   * to this component from the Editor's {@link ConfigureTree} window by
   * right-clicking on the component and selecting the appropriate "Add"
   * option.
   * @see Configurable
   */
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
      TurnTracker.class,
      ChessClockControl.class
    };
  }

  /**
   * The GameModule acts as the mediator for hotkey events.
   *
   * Components that wish to fire hotkey events when they have the
   * focus should register themselves using this method.  These events will be
   * forwarded to all listeners that have registered themselves with {@link #addKeyStrokeListener}
   * @param src KeyStrokeSource Component that wants to register as a source for hotkey events
   */
  public void addKeyStrokeSource(KeyStrokeSource src) {
    if (!isLoadingContinuationSemaphore()) {
      if (!keyStrokeSources.contains(src)) {
        keyStrokeSources.add(src);
        for (final KeyStrokeListener l : keyStrokeListeners) {
          l.addKeyStrokeSource(src);
        }
      }
    }
  }


  public void removeKeyStrokeListener(KeyStrokeListener l) {
    for (final KeyStrokeSource s : keyStrokeSources) {
      l.removeKeyStrokeSource(s);
    }

    keyStrokeListeners.remove(l);
  }

  /**
   * The GameModule acts as the mediator for hotkey events.
   *
   * Objects that react to hotkey events should register themselves
   * using this method.  Any component that has been registered with {@link #addKeyStrokeSource}
   * will forward hotkey events to listeners registered with this method.
   *
   * @param l KeystrokeListener to add
   */
  public void addKeyStrokeListener(KeyStrokeListener l) {
    if (!isLoadingContinuationSemaphore()) {
      if (!keyStrokeListeners.contains(l)) {
        keyStrokeListeners.add(l);
        for (final KeyStrokeSource s : keyStrokeSources) {
          l.addKeyStrokeSource(s);
        }
      }
    }
  }


  public void resetSourcesAndListeners() {
    Expression.resetCachedExpressions();
  }

  /**
   * If our keyboard mapping paradigm changes (example: Mac Legacy preference checked/unchecked), we need to reregister all of our KeyStrokeListeners
   */
  public void refreshKeyStrokeListeners() {
    keyStrokeListeners.forEach(l -> l.setKeyStroke(l.getKeyStroke()));
  }

  /**
   * @deprecated use {@link #fireKeyStroke(NamedKeyStroke)}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void fireKeyStroke(KeyStroke stroke) {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
    if (stroke != null) {
      for (final KeyStrokeListener l : keyStrokeListeners) {
        l.keyPressed(stroke);
      }
    }
  }

  /**
   * Invokes a {@link NamedKeyStroke} to all of our listeners.
   * @param stroke NamedKeyStroke to invoke
   */
  public void fireKeyStroke(NamedKeyStroke stroke) {
    if (stroke == null || stroke.isNull()) {
      return;
    }

    final KeyStroke innerStroke = stroke.getKeyStroke();
    if (innerStroke == null) {
      return;
    }

    keyStrokeListeners.forEach(l -> l.keyPressed(innerStroke));
  }

  /**
   * @return the name of the game for this module
   */
  public String getGameName() {
    return gameName;
  }

  /**
   * @return the localized/translated game name for this module
   */
  public String getLocalizedGameName() {
    return localizedGameName == null ? gameName : localizedGameName;
  }

  /**
   * @return the version number for this module
   */
  public String getGameVersion() {
    return moduleVersion;
  }

  /**
   * Currently used to listen for changes to player names
   * @param l propertyChangeListener to add
   */
  public void addIdChangeListener(PropertyChangeListener l) {
    idChangeSupport.addPropertyChangeListener(l);
  }

  /**
   * Currently used to listen for changes to player names
   * @param l propertyChangeListener to remove
   */
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
   * @deprecated use {@link Prefs#getGlobalPrefs()}
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public Prefs getGlobalPrefs() {
    ProblemDialog.showDeprecated("2020-08-06");
    return Prefs.getGlobalPrefs();
  }


  public boolean isTranslatableSupport() {
    return true;
  }


  /**
   * GameModule holds the master list of CommandEncoders, and invokes them as appropriate when commands are sent and
   * received. This method adds a {@link CommandEncoder} to the list of objects that will attempt to decode/encode a command.
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
   * @see #addCommandEncoder
   * @see #decode
   * @see #encode
   */
  public void removeCommandEncoder(CommandEncoder ce) {
    commandEncoders = ArrayUtils.removeElement(commandEncoders, ce);
  }

  /**
   * Central location to create any type of GamePiece from within VASSAL
   *
   * @param type String definition of the piece
   * @return Created Piece
   */
  public GamePiece createPiece(String type) {
    for (final CommandEncoder commandEncoder : commandEncoders) {
      if (commandEncoder instanceof BasicCommandEncoder) {
        final GamePiece p = ((BasicCommandEncoder) commandEncoder).createPiece(type);
        if (p != null) {
          return p;
        }
      }
    }
    return null;
  }

  /**
   * Central location to create any type of GamePiece Trait (Decorator) from within VASSAL
   *
   * @param type String definition of the Trait (Decorator)
   * @return Created Piece
   */
  public GamePiece createPiece(String type, GamePiece inner) {
    for (final CommandEncoder commandEncoder : commandEncoders) {
      if (commandEncoder instanceof BasicCommandEncoder) {
        final GamePiece p = ((BasicCommandEncoder) commandEncoder).createDecorator(type, inner);
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
   * @param s message to display in Chat Log
   */
  public void warn(String s) {
    String s2 = s;
    if (s2.isEmpty() || (QuickColors.getQuickColor(s) == -1)) { // Quick Colors "opt in" HTML
      s2 = s2.replaceAll("<", "&lt;")  // So < symbols in warning messages don't get misinterpreted as HTML //$NON-NLS
             .replaceAll(">", "&gt;"); //$NON-NLS
    }
    if (chat == null) {
      deferredChat.add(s2);
    }
    else {
      chat.show(" - " + s2); //$NON-NLS-1$
    }
  }

  /**
   * @return a single Random number generator that all objects may share
   */
  public Random getRNG() {
    return RNG;
  }

  /**
   * @return the object responsible for logging commands to a .vlog logfile to support PBEM play
   */
  public Logger getLogger() {
    return logger;
  }

  /**
   * Allows the Chat Log window to register itself to the Module. When a Chat Log is first
   * registered, display any warning messages deferred during earlier initialisation
   *
   * @param c The Chat Log window we want to use
   */
  public void setChatter(Chatter c) {
    chat = c;
    if (!deferredChat.isEmpty()) {
      for (final String msg : deferredChat) {
        warn(msg);
      }
      deferredChat.clear();
    }
  }

  public JComponent getControlPanel() {
    return controlPanel;
  }

  /**
   * @return the registered Chat Log for the Module Window (window that displays chat text)
   */
  public Chatter getChatter() {
    return chat;
  }

  /**
   * @return the registered docked PieceWindow (game piece palette) for the module
   */
  public PieceWindow getPieceWindow() {
    return pieceWindow;
  }

  /**
   * Allows a PieceWindow that wants to be our docked PieceWindow to register itself.
   *
   * @param pieceWindow PieceWindow to be our docked one
   */
  public void setPieceWindow(PieceWindow pieceWindow) {
    this.pieceWindow = pieceWindow;
  }

  public void setPrefs(Prefs p) {
    preferences = p;
    preferences.getEditor().initDialog(getPlayerWindow());
  }

  /**
   *
   * @deprecated no replacement
   */
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public void setGlobalPrefs(@SuppressWarnings("unused") Prefs p) {
    ProblemDialog.showDeprecated("2020-08-06"); //NON-NLS
  }

  /**
   * Decodes our inbound {@link Command} traffic -- incoming player commands ready to be executed.
   * Unlike everybody else's decode method, the GameModule's method first pulls apart strings of multiple
   * commands into singles. It then invokes each of its registered {@link CommandEncoder}s in turn
   * until one of them is able to deserialize the command from a String into a {@link Command} object ready to be executed.
   */
  @Override
  public Command decode(String command) {
    if (command == null) {
      return null;
    }
    Command c;
    final SequenceEncoder.Decoder st =
      new SequenceEncoder.Decoder(command, COMMAND_SEPARATOR);
    final String first = st.nextToken();
    if (command.equals(first)) {
      c = decodeSubCommand(first);
    }
    else {
      Command next;
      c = decode(first);
      while (st.hasMoreTokens()) {
        next = decode(st.nextToken());
        c = c == null ? next : c.append(next);
      }
    }
    return c;
  }

  /**
   * Deserializes a single anonymous subcommand String into a {@link Command}, by invoking #decode from each of our registered
   * command encoders in turn until one of them is able to successfully recognize and deserialize the command.
   * @param subCommand A single command, to be deserialized
   * @return a {@link Command} object for this command, ready to be executed.
   */
  private Command decodeSubCommand(String subCommand) {
    Command c = null;
    for (int i = 0; i < commandEncoders.length && c == null; ++i) {
      c = commandEncoders[i].decode(subCommand);
    }
    return c;
  }

  /**
   * Encodes our outbound {@link Command} traffic -- outgoing player commands to be sent to other players' clients via
   * either online (server) or logfile (PBEM). Unlike everybody else's encode method, the GameModule's method accepts Command
   * trees containing multiple appended commands. It goes through them in order and for each command invokes each of its registered
   * {@link CommandEncoder}s in turn until one of them is able to serialize the {@link Command} object into an ascii-compatible
   * String ready to be sent to other players' clients. It does this for each of the subcommands in the list, and the results for
   * all are returned as a single String.
   */
  @Override
  public String encode(Command c) {
    if (c == null) {
      return null;
    }
    String s = encodeSubCommand(c);
    String s2;
    final Command[] sub = c.getSubCommands();
    if (sub.length > 0) {
      final SequenceEncoder se = new SequenceEncoder(s, COMMAND_SEPARATOR);
      for (final Command command : sub) {
        s2 = encode(command);
        if (s2 != null) {
          se.append(s2);
        }
      }
      s = se.getValue();
    }
    return s;
  }

  /**
   * Serializes a single anonymous {@link Command} object into an ascii-compatible string, by invoking #encode on
   * from each of our registered {@link CommandEncoder}s in turn until one of them is successfully able to recognize
   * and serialize the Command.
   * @param c A Command object containing a single Command of any type.
   * @return ascii-friendly String form of the command, ready to be sent to other players' clients.
   */
  private String encodeSubCommand(Command c) {
    String s = null;
    for (int i = 0; i < commandEncoders.length && s == null; ++i) {
      s = commandEncoders[i].encode(c);
    }
    return s;
  }

  /**
   * @return a common FileChooser so that recent file locations
   * can be remembered
   */
  public FileChooser getFileChooser() {
    if (fileChooser == null) {
      fileChooser = FileChooser.createFileChooser(getPlayerWindow(),
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
  @Deprecated(since = "2020-08-06", forRemoval = true)
  public FileDialog getFileDialog() {
    ProblemDialog.showDeprecated("2020-08-06");
    if (fileDialog == null) {
      fileDialog = new FileDialog(getPlayerWindow());
      final File f = getGameState().getSavedGameDirectoryPreference().getFileValue();
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
   * Provides access to the Game Module's toolbar.
   * @return the JToolBar of the command window
   */
  @Override
  public JToolBar getToolBar() {
    return frame.getToolBar();
  }

  /**
   * Returns an appropriate Title Bar string for a window, based on the module name,
   * the last read/written game file, and the manner of interaction with it.
   * @return appropriate title bar string for a window.
   * @param key Localization key to be used to generate string
   * @param name Name of the object whose title bar is to be generated
   * @param moduleVersion if true, include the module version number
   */
  public String getWindowTitleString(String key, String name, boolean moduleVersion) {
    final String version = getGameVersion();
    final String nameString;

    if (moduleVersion && !DEFAULT_MODULE_VERSION.equals(version)) {
      nameString = name + " " + version;
    }
    else {
      nameString = name;
    }

    if (StringUtils.isEmpty(gameFile) || GameFileMode.NEW_GAME.equals(gameFileMode)) {
      return Resources.getString(key + "_title", nameString, "", Info.getVersion());  //NON-NLS-1$
    }
    else {
      return Resources.getString(key + "_title_" + gameFileMode, nameString, moduleVersion ? gameFile : FilenameUtils.removeExtension(gameFile), Info.getVersion()); //NON-NLS-1$
    }
  }

  public String getWindowTitleString(String key, String name) {
    return getWindowTitleString(key, name, true);
  }


  /**
   * Returns an appropriate Title Bar string for the main module window, based on the module name,
   * the last read/written game file, and the manner of interaction with it.
   * @return appropriate title bar string for main module window.
   */
  public String getTitleString() {
    return getWindowTitleString("GameModule.frame", getLocalizedGameName()); //NON-NLS-1$
  }

  /**
   * Updates the title bar of the main module window, and all map windows
   */
  public void updateTitleBar() {
    frame.setTitle(getTitleString());

    for (final Map m : getComponentsOf(Map.class)) {
      m.updateTitleBar();
    }
  }

  /**
   * @deprecated use {@link #updateTitleBar()}
   * @param s String to append to title
   */
  @Deprecated(since = "2020-09-16", forRemoval = true)
  public void appendToTitle(String s) {
    // replaced by updateTitleBar()
  }

  /**
   * Sets the most recent .VSAV / .VLOG file saved, loaded, or logged to, along with
   * the type of action taken with that file.
   * @param gameFile Most recent VSAV/VLOG if any
   * @param mode mode of access
   */
  public void setGameFile(String gameFile, GameFileMode mode) {
    this.gameFile = gameFile;
    setGameFileMode(mode);
    updateTitleBar();
  }

  /**
   * @return Most recent .VSAV/.VLOG that we've read or written.
   */
  public String getGameFile() {
    return gameFile;
  }

  /**
   * Sets the type of interaction we most recently had with saving/loading/replaying/logging, for managing title bars
   * of windows.
   * @param mode mode of access
   */
  public void setGameFileMode(GameFileMode mode) {
    gameFileMode = Objects.requireNonNull(mode);
    updateTitleBar();
  }

  /**
   * @return Returns the most recent type of interaction we've had for saving/loading/replaying/logging the game, for managing
   * title bars of windows.
   */
  public GameFileMode getGameFileMode() {
    return gameFileMode;
  }

  /**
   * @return true if we are currently logging or replaying a game.
   */
  public boolean isReplayingOrLogging() {
    return (gameFileMode == GameFileMode.LOGGING_GAME) || (gameFileMode == GameFileMode.REPLAYING_GAME);
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
          && (!buildString().equals(lastSavedConfiguration) || iFeelDirty)) {
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
      for (final ModuleExtension ext : getComponentsOf(ModuleExtension.class)) {
        cancelled = !ext.confirmExit();
      }
    }

    if (!cancelled) {
      Prefs p = null;

      // write and close module prefs
      try {
        p = getPrefs();
        p.save();
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
            log.error("Error while closing module preferences", e); //NON-NLS
          }
        }
      }

      // close the module
      try {
        archive.close();
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, archive.getName());
      }

      log.info("Exiting"); //NON-NLS
    }

    return !cancelled;
  }

/*
  private void dumpCommand(Command c, int indent) {
    System.out.println(" ".repeat(indent) + c);
    for (final Command s : c.getSubCommands()) {
      dumpCommand(s, indent + 1);
    }
  }
*/

  /**
   * When the local player has taken any action that would change the game state (or otherwise needs to be sent to
   * any other players' clients), the action should be encapsulated into a {@link Command} and sent here. This method
   * encodes the {@link Command}, sends it to the server (if we're online) and write it to a .vlog PBEM logfile (if
   * any is open). It thus drives both formats of multiplayer game through a common interface.
   * @param c The {@link Command} to be sent and logged.
   *
   * @see #encode
   */
  public void sendAndLog(Command c) {
    if (c != null && !c.isNull()) {
      synchronized (loggingLock) {
        if (loggingPaused) {
          pausedCommands.getFirst().append(c);
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
   * @return Current logging pause status, false if logging currently paused
   */
  public boolean pauseLogging() {
    synchronized (loggingLock) {
      pausedCommands.push(new NullCommand());
      loggingPaused = true;
      return true;
    }
  }

  /**
   * Restart logging and return any outstanding commands
   * @return any outstanding {@link Command} (can contain multiple chained commands)
   */
  public Command resumeLogging() {
    final Command c;
    synchronized (loggingLock) {
      c = pausedCommands.pop();
      if (pausedCommands.isEmpty()) {
        loggingPaused = false;
      }
    }
    return c;
  }

  /**
   * Clear outstanding Commands
   * Use where the calling level handles the sending of outstanding commands
   */
  public void clearPausedCommands() {
    pausedCommands.clear();
  }

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
   * @return the object responsible for sending messages to the server
   */
  public ServerConnection getServer() {
    return server;
  }

  /**
   * @return true if the game is online or has ever had more than one player.
   */
  public boolean isMultiPlayer() {
    final ServerConnection sv = getServer();
    if ((sv != null) && sv.isConnected()) return true;

    final PlayerRoster pr = getPlayerRoster();
    return ((pr != null) && pr.isMultiPlayer());
  }

  /**
   * Loads a module object into the player window.
   *
   * Registers a <a href="https://en.wikipedia.org/wiki/Singleton_pattern">singleton</a> GameModule
   * and invokes {@link Buildable#build} on it to build it from the XML buildFile. This will have the effect of
   * invoking {@link Buildable#build} on all of the module's subcomponents as well, effectively building our whole
   * component hierarchy from the XML.
   */
  public static void init(GameModule module) throws IOException {
    if (theModule != null) {
      throw new UnsupportedOperationException(
        Resources.getString("GameModule.open_error",
          theModule.getDataArchive().getName()));
    }

    theModule = module;
    theModule.setGpIdSupport(theModule);
    try {
      theModule.build();
    }
    catch (IOException e) {
      theModule = null;
      throw e;
    }

    /*
     *  If we are editing, check for duplicate, illegal or missing GamePiece Id's
     *  and update if necessary.
     */
    if (theModule.getDataArchive() instanceof ArchiveWriter) {
      theModule.checkGpIds();
    }

    /*
     * Tell any Plugin components that the build is complete so that they
     * can finish initialization.
     */
    for (final Plugin plugin : theModule.getComponentsOf(Plugin.class)) {
      plugin.init();
    }
  }

  /**
   * Save the current buildString for comparison when we try and quit.
   */
  public void updateLastSave() {
    lastSavedConfiguration = buildString();
  }

  /**
   * Allocates the next available GamePiece ID and returns it.
   * @return an available GpID
   * @see GpIdSupport
   */
  @Override
  public String generateGpId() {
    return String.valueOf(nextGpId++);
  }

  /**
   * @return the next available GpId (GamePiece ID) (but doesn't allocate it -- for that see {@link #generateGpId})
   * @see GpIdSupport
   */
  @Override
  public int getNextGpId() {
    return nextGpId;
  }

  /**
   * Sets the next available GamePiece ID
   * @param id new value for nextGpId.
   * @see GpIdSupport
   */
  @Override
  public void setNextGpId(int id) {
    nextGpId = id;
  }

  /**
   * Registers a GamePiece ID support object for us to manage a namespace for unique GamePiece objects.
   * @param s GamePiece ID object
   */
  public void setGpIdSupport(GpIdSupport s) {
    gpidSupport = s;
  }

  /**
   * @return Our registered Game Piece ID namespace service provider.
   */
  public GpIdSupport getGpIdSupport() {
    return gpidSupport;
  }

  /**
   * Check every PieceSlot and PlaceMarker trait for duplicate,
   * illegal or Missing GamePiece id's and update them if necessary
   * @see GpIdSupport
   */
  private void checkGpIds() {
    final GpIdChecker checker = new GpIdChecker(this);
    for (final PieceSlot pieceSlot : getAllDescendantComponentsOf(PieceSlot.class)) {
      checker.add(pieceSlot);
    }

    // Add any PieceSlots in Prototype Definitions
    for (final PrototypesContainer pc : getComponentsOf(PrototypesContainer.class)) {
      pc.getDefinitions().forEach(checker::add);
    }

    checker.fixErrors();
  }

  /**
   * VASSAL modules are stored in ".vmod" files, which are actually simple ".zip" files with
   * a unique extension.
   * @return the object which stores data for the module -- our .vmod Zip file.
   */
  public DataArchive getDataArchive() {
    return archive;
  }

  /*
   * Returns the i18n resource finder
   */
  public ResourcePathFinder getResourcePathFinder() {
    return resourceFinder;
  }

  /**
   * VASSAL modules are stored in ".vmod" files, which are actually simple ".zip" files with
   * a unique extension.
   *
   * If the module is being edited, return the writeable archive for the module
   * @return the writer for our .vmod Zip file
   */
  public ArchiveWriter getArchiveWriter() {
    return archive.getWriter();
  }

  /**
   * @return the object that provides tiling and caching services for tiling large map images.
   */
  public ImageTileSource getImageTileSource() {
    if (tcache == null) {
      // FIXME: There's no guarantee that getGameName() and getGameVersion()
      // are properly set at this point.

      final String hstr =
        DigestUtils.sha1Hex(getGameName() + "_" + getGameVersion()); //NON-NLS

      final File tc = new File(Info.getConfDir(), "tiles/" + hstr); //NON-NLS
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
   * Is an editor window currently open
   * @return true if we're running with an editor window
   */
  public boolean isEditorOpen() {
    return !isLocalizationEnabled();
  }

  /**
   * @return the <a href="https://en.wikipedia.org/wiki/Singleton_pattern">singleton</a> instance of GameModule
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

  /**
   * If the module is being edited, write the module data after prompting for a new filename.
   */
  public void saveAs() {
    save(true);
  }

  /**
   * If the module is being edited, write the module data to the current filename
   */
  public void save() {
    save(false);
  }

  /**
   * If the module is being edited, write the module data
   * @param saveAs true to force display of a {@link FileChooser} to offer a new choice of filenames
   */
  private void save(boolean saveAs) {
    vassalVersionCreated = Info.getVersion();

    iFeelDirty = false; // Ahhhhhhhhhhh.

    final ArchiveWriter writer = getArchiveWriter();

    try {
      (new ModuleMetaData(this)).save(writer);
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, writer.getName());
    }

    try {
      final String save = buildString();
      writer.addFile(BUILDFILE, save.getBytes(StandardCharsets.UTF_8));

      writer.removeFile(BUILDFILE_OLD); // Don't leave old non-extension buildfile around if we successfully write the new one.

      final boolean actuallyDidStuff = saveAs ? writer.saveAs(true) : writer.save(true);
      if (actuallyDidStuff) {
        lastSavedConfiguration = save;
        warn(Resources.getString("Editor.GameModule.saved", writer.getArchive().getFile().getName()));
      }
    }
    catch (IOException e) {
      WriteErrorDialog.showError(
        getPlayerWindow(),
        e,
        writer.getArchive().getFile(),
        "Error.file_write_error" //NON-NLS
      );
    }
  }

  /**
   * @return an XML element that can be used to {@link Buildable#build} the module object.
   */
  private String buildString() {
    final Document doc = Builder.createNewDocument();
    doc.appendChild(getBuildElement(doc));
    return Builder.toString(doc);
  }

  /**
   * Gets the value of a module level global property -- this includes identification information for the
   * local player as well as the contents of any Global Property objects defined at module level in the Module.
   * @param key identifies the global property to be returned
   * @return value of designated global property
   */
  @Override
  public Object getProperty(Object key) {
    if (GlobalOptions.PLAYER_SIDE.equals(key) || GlobalOptions.PLAYER_SIDE_ALT.equals(key)) {
      final String mySide = PlayerRoster.getMySide();
      return mySide == null ? "" : mySide;  //$NON-NLS-1$
    }
    else if (GlobalOptions.PLAYER_NAME.equals(key) || GlobalOptions.PLAYER_NAME_ALT.equals(key)) {
      return getPrefs().getValue(GameModule.REAL_NAME);
    }
    else if (GlobalOptions.PLAYER_ID.equals(key) || GlobalOptions.PLAYER_ID_ALT.equals(key)) {
      return GlobalOptions.getInstance().getPlayerId();
    }
    else if (MODULE_NAME_PROPERTY.equals(key)) {
      return gameName;
    }
    else if (MODULE_VERSION_PROPERTY.equals(key)) {
      return moduleVersion;
    }
    else if (MODULE_DESCRIPTION_PROPERTY.equals(key)) {
      return description;
    }
    else if (MODULE_VASSAL_VERSION_CREATED_PROPERTY.equals(key)) {
      return vassalVersionCreated;
    }
    else if (MODULE_VASSAL_VERSION_RUNNING_PROPERTY.equals(key)) {
      return Info.getVersion();
    }
    else if (MODULE_OTHER1_PROPERTY.equals(key)) {
      return moduleOther1;
    }
    else if (MODULE_OTHER2_PROPERTY.equals(key)) {
      return moduleOther2;
    }
    else if (GameModule.MODULE_CURRENT_LOCALE.equals(key)) {
      return Resources.getLocale().getLanguage();
    }
    else if (GameModule.MODULE_CURRENT_LOCALE_NAME.equals(key)) {
      return Resources.getLocale().getDisplayName();
    }

    //BR// MapName_isVisible property for each map window
    for (final Map map : Map.getMapList()) {
      if ((map.getConfigureName() + "_isVisible").equals(key) || (map.getConfigureName().replaceAll(" ", "_") + "_isVisible").equals(key)) { //NON-NLS
        final Container tla = (map.getComponent() != null) ? ((JPanel)map.getComponent()).getTopLevelAncestor() : null;
        return String.valueOf(tla != null && tla.isShowing());
      }
    }

    final MutableProperty p = propsContainer.getMutableProperty(String.valueOf(key));
    if (p != null) {
      return p.getPropertyValue();
    }

    final TranslatableString s = transContainer.getTranslatableString(String.valueOf(key));
    return s == null ? null : s.getPropertyValue();
  }

  /**
   * Gets the value of a mutable (changeable) "Global Property". Module level Global Properties serve as the
   * "global variables" of a VASSAL Module, as they are accessible by any component at any time.
   * @param name identifies the Global Property whose value should be returned
   * @return value of designated global property
   */
  @Override
  public MutableProperty getMutableProperty(String name) {
    return propsContainer.getMutableProperty(name);
  }

  @Override
  public TranslatableString getTranslatableString(String name) {
    return transContainer.getTranslatableString(name);
  }

  /**
   * Adds a new mutable (changeable) "Global Property" to the Module. Module level Global Properties serve as the
   * "global variables" of a VASSAL Module, as they are accessible by any component at any time.
   * @param key Name for the new Global Property
   * @param p Starting property value
   */
  @Override
  public void addMutableProperty(String key, MutableProperty p) {
    propsContainer.addMutableProperty(key, p);
    p.addMutablePropertyChangeListener(repaintOnPropertyChange);
  }

  @Override
  public void addTranslatableString(String key, TranslatableString p) {
    transContainer.addTranslatableString(key, p);
  }

  /**
   * Removes a mutable property from the Global Properties list.
   * @param key Name of the Global Property
   * @return the removed property, for some reason.
   */
  @Override
  public MutableProperty removeMutableProperty(String key) {
    final MutableProperty p = propsContainer.removeMutableProperty(key);
    if (p != null) {
      p.removeMutablePropertyChangeListener(repaintOnPropertyChange);
    }
    return p;
  }

  @Override
  public TranslatableString removeTranslatableString(String key) {
    return transContainer.removeTranslatableString(key);
  }

  /**
   * @return Identifies the ID/level for mutable properties stored here in the Module.
   */
  @Override
  public String getMutablePropertiesContainerId() {
    return "Module"; //NON-NLS-$1
  }

  @Override
  public String getTranslatableStringContainerId() {
    return "Module"; //NON-NLS
  }

  /**
   * @param key Name of the property to get the value of
   * @return Localized/translated name of the named property, if one is available, otherwise returns the non-localized name
   */
  @Override
  public Object getLocalizedProperty(Object key) {
    if (GlobalOptions.PLAYER_SIDE.equals(key) || GlobalOptions.PLAYER_SIDE_ALT.equals(key)) {
      final String mySide = PlayerRoster.getMyLocalizedSide();
      return mySide == null ? "" : mySide;  //$NON-NLS-1$
    }
    else {
      return getProperty(key);
    }
  }

  /**
   * @return a cumulative CRC from all of our files
   */
  public long getCrc() {
    if (crc == null) {
      crc = buildCrc();
    }
    return crc;
  }

  /**
   * @return a cumulative CRC from all of our files
   */
  private Long buildCrc() {
    final List<File> files = new ArrayList<>();
    if (getDataArchive().getArchive() != null) {
      files.add(new File(getDataArchive().getName()));
    }

    for (final ModuleExtension ext : getComponentsOf(ModuleExtension.class)) {
      if (ext.getDataArchive().getArchive() != null) {
        files.add(new File(ext.getDataArchive().getName()));
      }
    }

    try {
      return CRCUtils.getCRC(files);
    }
    catch (IOException e) {
      log.error("Error generating CRC", e); //NON-NLS
      return 0L;
    }
  }

  /**
   * @return the object containing the internationalization/localization information for this component
   */
  @Override
  public ComponentI18nData getI18nData() {
    final ComponentI18nData myI18nData = super.getI18nData();
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

  /**
   * Adds listener for players changing sides
   * @param l new SideChangeListener
   */
  public void addSideChangeListenerToPlayerRoster(PlayerRoster.SideChangeListener l) {
    if (!isLoadingContinuationSemaphore()) {
      final PlayerRoster r = getPlayerRoster();
      if (r != null) {
        r.addSideChangeListenerToInstance(l);
      }
    }
  }

  /**
   * Removes listener for players changing sides
   * @param l old SideChangeListener
   */
  public void removeSideChangeListenerFromPlayerRoster(PlayerRoster.SideChangeListener l) {
    final PlayerRoster r = getPlayerRoster();
    if (r != null) {
      r.removeSideChangeListenerFromInstance(l);
    }
  }

  /**
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return List.of(gameName, moduleVersion, description);
  }
}
