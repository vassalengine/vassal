/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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

import java.awt.BorderLayout;
import java.awt.FileDialog;
import java.awt.FlowLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;
import VASSAL.Info;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.ChartWindow;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.DiceButton;
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
import VASSAL.build.module.PredefinedSetup;
import VASSAL.build.module.PrivateMap;
import VASSAL.build.module.PrototypesContainer;
import VASSAL.build.module.RandomTextButton;
import VASSAL.build.module.ServerConnection;
import VASSAL.build.module.SpecialDiceButton;
import VASSAL.build.module.ToolbarMenu;
import VASSAL.build.module.WizardSupport;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.command.Logger;
import VASSAL.configure.CompoundValidityChecker;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.MandatoryComponent;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;
import VASSAL.tools.FileChooser;
import VASSAL.tools.KeyStrokeListener;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.MTRandom;
import VASSAL.tools.ToolBarComponent;

/**
 * The GameModule class is the base class for a VASSAL module.  It is
 * the root of the {@link Buildable} containment hierarchy.
 * Components which are added directly to the GameModule are contained
 * in the <code>VASSAL.build.module</code> package
 *
 * It is a singleton, and contains access points for many other classes,
 * such as {@link DataArchive}, {@link ServerConnection}, {@link Logger},
 * and {@link Prefs} */
public abstract class GameModule extends AbstractConfigurable implements CommandEncoder, ToolBarComponent, PropertySource, MutablePropertiesContainer {
  protected static final String DEFAULT_NAME = "Unnamed module";  //$NON-NLS-1$
  public static final String MODULE_NAME = "name";  //$NON-NLS-1$
  public static final String MODULE_VERSION = "version";  //$NON-NLS-1$
  public static final String VASSAL_VERSION_CREATED = "VassalVersion";  //$NON-NLS-1$
  /** The System property of this name will return a version identifier for the version of VASSAL being run */
  public static final String VASSAL_VERSION_RUNNING = "runningVassalVersion";  //$NON-NLS-1$

  private static GameModule theModule;

  protected String moduleVersion = "0.0";  //$NON-NLS-1$
  protected String vassalVersionCreated = "0.0";  //$NON-NLS-1$
  protected String gameName = DEFAULT_NAME;
  protected String lastSavedConfiguration;
  protected FileChooser fileChooser;
  protected FileDialog fileDialog;
  protected MutablePropertiesContainer propsContainer = new Impl();
  protected PropertyChangeListener repaintOnPropertyChange = new PropertyChangeListener() {
    public void propertyChange(PropertyChangeEvent evt) {
        for (Iterator maps = Map.getAllMaps(); maps.hasNext(); ) {
          Map map = (Map) maps.next();
          map.repaint();            
        }
    }
  };

  protected JPanel controlPanel = new JPanel();

  protected JToolBar toolBar = new JToolBar();
  protected JMenu fileMenu = new JMenu(Resources.getString(Resources.FILE)); 

  protected GameState theState;
  protected DataArchive archive;
  protected Prefs preferences;
  protected Prefs globalPrefs;
  protected Logger logger;
  protected Chatter chat;
  protected Random RNG;
  protected ServerConnection server;

  protected JFrame frame = new JFrame();
  
  protected WizardSupport wizardSupport = new WizardSupport();

  protected List<KeyStrokeSource> keyStrokeSources =
    new ArrayList<KeyStrokeSource>();
  protected List<KeyStrokeListener> keyStrokeListeners =
    new ArrayList<KeyStrokeListener>();
  protected CommandEncoder[] commandEncoders = new CommandEncoder[0];

  /**
   * @return the top-level frame of the controls window
   */
  public JFrame getFrame() {
    return frame;
  }
  
  public WizardSupport getWizardSupport() {
    return wizardSupport;
  }

  protected GameModule(DataArchive archive) {
    frame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);

    this.archive = archive;
    frame.addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        quit();
      }
    });

    frame.getContentPane().setLayout(new BorderLayout());
    frame.setJMenuBar(new JMenuBar());

    fileMenu.setMnemonic('F');
    frame.getJMenuBar().add(fileMenu);

    toolBar.setLayout(new VASSAL.tools.WrapLayout(FlowLayout.LEFT, 0, 0));
    toolBar.setAlignmentX(0.0F);
    toolBar.setFloatable(false);
    frame.getContentPane().add(toolBar, BorderLayout.NORTH);
    controlPanel.setLayout(new BorderLayout());
    addKeyStrokeSource
        (new KeyStrokeSource
            (frame.getRootPane(),
             JComponent.WHEN_IN_FOCUSED_WINDOW));
    frame.getContentPane().add(controlPanel, BorderLayout.CENTER);

    validator = new CompoundValidityChecker
        (new MandatoryComponent(this, Documentation.class),
         new MandatoryComponent(this, GlobalOptions.class));
  }

  /**
   * Initialize the module
   */
  protected abstract void build() throws IOException;

  public void setAttribute(String name, Object value) {
    if (MODULE_NAME.equals(name)) {
      gameName = (String) value;
      setConfigureName(gameName);
    }
    else if (MODULE_VERSION.equals(name)) {
      moduleVersion = (String) value;
    }
    else if (VASSAL_VERSION_CREATED.equals(name)) {
      vassalVersionCreated = (String) value;
      String runningVersion = Info.getVersion();
      if (Info.compareVersions(vassalVersionCreated, runningVersion) > 0) {
        JOptionPane.showMessageDialog
            (null,
             Resources.getString("GameModule.version_error", runningVersion, vassalVersionCreated),  //$NON-NLS-1$
             Resources.getString("GameModule.version_error_short"),  //$NON-NLS-1$
             JOptionPane.ERROR_MESSAGE);
      }
    }
  }

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
    return null;
  }

  /**
   *
   * A valid verson format is "w.x.y[bz]", where
   * 'w','x','y', and 'z' are integers.
   * @deprecated use {@link Info#compareVersions}
   * @return a negative number if <code>v2</code> is a later version
   * the <code>v1</code>, a positive number if an earlier version,
   * or zero if the versions are the same.
   *
   */
  @Deprecated public static int compareVersions(String v1, String v2) {
    return Info.compareVersions(v1, v2);
  }

  public void addTo(Buildable b) {
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GameModule.component_type");  //$NON-NLS-1$
  }

  public void removeFrom(Buildable parent) {
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm");  //$NON-NLS-1$
  }

  public String[] getAttributeNames() {
    return new String[]{MODULE_NAME, MODULE_VERSION, VASSAL_VERSION_CREATED};
  }

  public String[] getAttributeDescriptions() {
    return new String[]{Resources.getString("Editor.GameModule.name_label"), Resources.getString("Editor.GameModule.version_label")}; //$NON-NLS-1$ //$NON-NLS-2$
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, String.class};
  }

  public Class[] getAllowableConfigureComponents() {
    Class[] c = { Map.class,
                  PieceWindow.class,
                  PrototypesContainer.class,
                  ToolbarMenu.class,
                  MultiActionButton.class,
                  DiceButton.class,
                  GlobalKeyCommand.class,
                  Inventory.class,
//                  InternetDiceButton.class,   // Disable internet dice button until Bones server can prevent email spamming 
                  RandomTextButton.class,
                  SpecialDiceButton.class,
                  PredefinedSetup.class,
                  ChartWindow.class,
                  PrivateMap.class,
                  PlayerHand.class,
                  NotesWindow.class
                };
    return c;
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
  
  public void fireKeyStroke(KeyStroke stroke) {
  	if (stroke != null) {
  		for (KeyStrokeListener l : keyStrokeListeners) {
				l.keyPressed(stroke);
			}
  	}
  }

  /**
   * @return the name of the game for this module
   */
  public String getGameName() {
    return gameName;
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

  /**
   * @return the preferences for this module
   */
  public Prefs getPrefs() {
    if (preferences == null) {
      setPrefs(new Prefs(globalPrefs.getEditor(), gameName));
    }
    return preferences;
  }

  /**
   * A set of preferences that applies to all modules
   * @return
   */
  public Prefs getGlobalPrefs() {
    return globalPrefs;
  }

  /**
   * This method adds a {@link CommandEncoder} to the list of objects
   * that will attempt to decode/encode a command
   *
   * @see #decode
   * @see #encode
   */
  public void addCommandEncoder(CommandEncoder ce) {
    CommandEncoder[] oldValue = commandEncoders;
    commandEncoders = new CommandEncoder[oldValue.length + 1];
    System.arraycopy(oldValue, 0, commandEncoders, 0, oldValue.length);
    commandEncoders[oldValue.length] = ce;
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
    for (int i = 0; i < commandEncoders.length; ++i) {
      if (ce.equals(commandEncoders[i])) {
        CommandEncoder[] oldValue = commandEncoders;
        commandEncoders = new CommandEncoder[oldValue.length - 1];
        System.arraycopy(oldValue, 0, commandEncoders, 0, i);
        if (i < commandEncoders.length) {
          System.arraycopy(oldValue, i + 1, commandEncoders, i, commandEncoders.length - i);
        }
        break;
      }
    }
  }

  /**
   * Central location to create any type of GamePiece from within VASSAL
   * 
   * @param type
   * @return
   */
  public GamePiece createPiece(String type) {
    for (int i = 0; i < commandEncoders.length; ++i) {
      if (commandEncoders[i] instanceof BasicCommandEncoder) {
        GamePiece p = ((BasicCommandEncoder) commandEncoders[i]).createPiece(type);
        if (p != null) {
          return p;
        }
      }
    }
    return null;
  }
  
  public GamePiece createPiece(String type, GamePiece inner) {
    for (int i = 0; i < commandEncoders.length; ++i) {
      if (commandEncoders[i] instanceof BasicCommandEncoder) {
        GamePiece p = ((BasicCommandEncoder) commandEncoders[i]).createDecorator(type, inner);
        if (p != null) {
          return p;
        }
      }
    }
    return null;
  }
  
  /**
   * Display the given text in the control window's status line
   */
  public void warn(String s) {
    chat.show(" - " + s); //$NON-NLS-1$
  }

  /**
   * @return a single Random number generator that all objects may share
   */
  public Random getRNG() {
    if (RNG == null) {
      RNG = new MTRandom();
    }
    return RNG;
  }

  /**
   * @return the object responsible for logging commands to a logfile
   */
  public Logger getLogger() {
    return logger;
  }

  /**
   * set the object that displays chat text
   */
  public void setChatter(Chatter c) {
    chat = c;
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

  public void setGlobalPrefs(Prefs p) {
    globalPrefs = p;
  }

  /**
   * Uses the registered  {@link CommandEncoder}s
   * to decode a String into a {@link Command}.
   */
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

  private static final String SAVE_DIR = "SaveDir"; //$NON-NLS-1$

  /**
   * @return a common FileChooser so that recent file locations
   * can be remembered
   */
  public FileChooser getFileChooser() {
    if (fileChooser == null) {
      DirectoryConfigurer directoryConfigurer = new DirectoryConfigurer(SAVE_DIR, null);
      getPrefs().addOption(null, directoryConfigurer);
      fileChooser = FileChooser.createFileChooser(getFrame(), directoryConfigurer);
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
  @Deprecated public FileDialog getFileDialog() {
    if (fileDialog == null) {
      getPrefs().addOption(null, new DirectoryConfigurer(SAVE_DIR, null));
      fileDialog = new FileDialog(getFrame());
      File f = (File) getPrefs().getValue(SAVE_DIR);
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
  public JToolBar getToolBar() {
    return toolBar;
  }

  /**
   * @return the File menu of the command window
   */
  public JMenu getFileMenu() {
    return fileMenu;
  }

  /**
   * Append the string to the title of the controls window and all Map windows
   * @param s If null, set the title to the default.
   */
  public void appendToTitle(String s) {
    if (s == null) {
      frame.setTitle(Resources.getString("GameModule.frame_title", gameName));  //$NON-NLS-1$
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
    boolean cancelled = false;
    try {
      getGameState().setup(false);
      cancelled = getGameState().isGameStarted();
      if (!cancelled) {
        if (fileChooser != null) {
          getPrefs().getOption(SAVE_DIR)
                    .setValue(fileChooser.getCurrentDirectory());
        }
        getPrefs().write();
        if (getDataArchive() instanceof ArchiveWriter
            && !buildString().equals(lastSavedConfiguration)) {
          switch (JOptionPane.showConfirmDialog
              (frame, Resources.getString("GameModule.save_module"),  //$NON-NLS-1$
               "", JOptionPane.YES_NO_CANCEL_OPTION)) {  //$NON-NLS-1$
            case JOptionPane.YES_OPTION:
              save();
              break;
            case JOptionPane.CANCEL_OPTION:
              cancelled = true;
          }
        }
        else if (getArchiveWriter() != null) {
          for (ModuleExtension ext : getComponentsOf(ModuleExtension.class)) {
            cancelled = !ext.confirmExit();
          }
        }
      }
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }
    finally {
      if (!cancelled) {
        System.exit(0);
      }
    }
  }

  /**
   * Encode the {@link Command}, send it to the server and write it
   * to a logfile (if any is open)
   *
   * @see #encode
   */
  public void sendAndLog(Command c) {
    if (c != null && !c.isNull()) {
      getServer().sendToOthers(c);
      getLogger().log(c);
    }
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
   * Set the singleton GameModule and invoke {@link #build} on it
   */
  public static void init(GameModule module) throws IOException {
    if (theModule != null) {
      throw new IOException(Resources.getString("GameModule.open_error", theModule.getDataArchive().getName()));  //$NON-NLS-1$
    }
    else {
      theModule = module;
      try {
        theModule.build();
      }
      catch (IOException e) {
        theModule = null;
        throw e;
      }
    }

    if (theModule.getDataArchive() instanceof ArchiveWriter) {
      theModule.lastSavedConfiguration = theModule.buildString();
    }
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
    try {
      String save = buildString();
      getArchiveWriter().addFile
          ("buildFile",  //$NON-NLS-1$
           new java.io.ByteArrayInputStream(save.getBytes("UTF-8")));  //$NON-NLS-1$
      if (saveAs) {
        getArchiveWriter().saveAs();
      }
      else {
        getArchiveWriter().write();
      }
      lastSavedConfiguration = save;
    }
    catch (IOException err) {
      err.printStackTrace();
      JOptionPane.showMessageDialog
          (frame,
           Resources.getString("GameModule.save_error", err.getMessage()), //$NON-NLS-1$
           Resources.getString("GameModule.save_error_short"),  //$NON-NLS-1$
           JOptionPane.ERROR_MESSAGE);
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
  public Object getProperty(Object key) {
    if (GlobalOptions.PLAYER_SIDE.equals(key)) {
      String mySide = PlayerRoster.getMySide();
      return mySide == null ? "" : mySide;  //$NON-NLS-1$
    }
    else if (GlobalOptions.PLAYER_NAME.equals(key)) {
      return getPrefs().getValue(GameModule.REAL_NAME);
    }
    else if (GlobalOptions.PLAYER_ID.equals(key)) {
      return GlobalOptions.getInstance().getPlayerId();
    }
    MutableProperty p = propsContainer.getMutableProperty(String.valueOf(key));
    return p == null ? null : p.getPropertyValue();
  }
  
  public MutableProperty getMutableProperty(String name) {
    return propsContainer.getMutableProperty(name);
  }

  public void addMutableProperty(String key, MutableProperty p) {
    propsContainer.addMutableProperty(key, p);
    p.addMutablePropertyChangeListener(repaintOnPropertyChange);
  }

  public MutableProperty removeMutableProperty(String key) {
    MutableProperty p = propsContainer.removeMutableProperty(key);
    if (p != null) {
      p.removeMutablePropertyChangeListener(repaintOnPropertyChange);
    }
    return p;
  }
  
  public Object getLocalizedProperty(Object key) {
    if (GlobalOptions.PLAYER_SIDE.equals(key)) {
      String mySide = PlayerRoster.getMyLocalizedSide();
      return mySide == null ? "" : mySide;  //$NON-NLS-1$
    }
    else {
      return getProperty(key);
    }
  }
  
}
