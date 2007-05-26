package VASSAL.launch;

import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import javax.swing.JMenuItem;
import org.w3c.dom.Document;
import VASSAL.build.Buildable;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.build.module.BasicCommandEncoder;
import VASSAL.build.module.BasicLogger;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.GameState;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.PieceWindow;
import VASSAL.build.module.PlayerRoster;
import VASSAL.build.module.PrototypesContainer;
import VASSAL.build.module.gamepieceimage.GamePieceImageDefinitions;
import VASSAL.build.module.properties.GlobalProperties;
import VASSAL.chat.ChatServerFactory;
import VASSAL.chat.DynamicClientFactory;
import VASSAL.chat.HybridClient;
import VASSAL.chat.ServerConfigurer;
import VASSAL.chat.jabber.JabberClientFactory;
import VASSAL.chat.node.NodeClientFactory;
import VASSAL.chat.peer2peer.P2PClientFactory;
import VASSAL.chat.ui.ChatServerControls;
import VASSAL.command.Command;
import VASSAL.configure.PasswordConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.i18n.Language;
import VASSAL.i18n.Resources;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.SequenceEncoder;

public class BasicModule extends GameModule {
  private static char COMMAND_SEPARATOR = (char) java.awt.event.KeyEvent.VK_ESCAPE;
  protected ChatServerControls serverControls;

  public BasicModule(DataArchive archive, Prefs globalPrefs) {
    super(archive);
    setGlobalPrefs(globalPrefs);
  }

  protected void build() throws IOException {
    String fileName = "buildFile"; //$NON-NLS-1$
    InputStream inStream = null;
    try {
      inStream = getDataArchive().getFileStream(fileName);
    }
    catch (IOException ex) {
      if (new File(getDataArchive().getName()).exists()) {
        throw new IOException(Resources.getString("BasicModule.not_a_module")); //$NON-NLS-1$
      }
    }
    try {
      if (inStream == null) {
        build(null);
      }
      else {
        Document doc = Builder.createDocument(inStream);
        build(doc.getDocumentElement());
      }
    }
    catch (IOException ex) {
      throw new IllegalArgumentException(ex.getMessage());
    }
    getFileMenu().add(getPrefs().getEditor().getEditAction());
    JMenuItem q = new JMenuItem(Resources.getString(Resources.QUIT));
    q.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        quit();
      }
    });
    q.setMnemonic('Q');
    getFileMenu().add(q);
  }

  public void build(org.w3c.dom.Element e) {
    /*
     * We determine the name of the module at the very beginning, so we know which preferences to read
     */
    if (e != null) {
      gameName = e.getAttribute(MODULE_NAME);
      if (e.getAttribute(VASSAL_VERSION_CREATED).length() > 0) {
        vassalVersionCreated = e.getAttribute(VASSAL_VERSION_CREATED);
      }
    }
    initIdentityPreferences();
    initGameState();
    initLogger();
    initServer();
    if (e != null) {
      super.build(e);
      ensureComponent(GamePieceImageDefinitions.class);
      ensureComponent(GlobalProperties.class);
      ensureComponent(Language.class);      
    }
    else {
      buildDefaultComponents();
    }
    initFrame();
  }

  protected void initIdentityPreferences() {
    StringConfigurer fullName = new StringConfigurer(GameModule.REAL_NAME, Resources.getString("Prefs.name_label"), Resources.getString("Prefs.newbie"));   //$NON-NLS-1$ //$NON-NLS-2$
    TextConfigurer profile = new TextConfigurer(GameModule.PERSONAL_INFO, Resources.getString("Prefs.personal_info"), "");   //$NON-NLS-1$ //$NON-NLS-2$
    StringConfigurer user = new PasswordConfigurer(GameModule.SECRET_NAME, Resources.getString("Prefs.password_label"), Resources.getString("Prefs.password_prompt", System.getProperty("user.name"))); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    user.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        GameModule.setUserId((String) evt.getNewValue());
      }
    });
    GameModule.getGameModule().getPrefs().addOption(Resources.getString("Prefs.personal_tab"), fullName);   //$NON-NLS-1$ //$NON-NLS-2$
    GameModule.getGameModule().getPrefs().addOption(Resources.getString("Prefs.personal_tab"), user);   //$NON-NLS-1$ //$NON-NLS-2$
    GameModule.getGameModule().getPrefs().addOption(Resources.getString("Prefs.personal_tab"), profile);  //$NON-NLS-1$
    GameModule.setUserId(user.getValueString());
  }

protected void initServer() {
    DynamicClientFactory dynamicClientFactory = new DynamicClientFactory();
    ChatServerFactory.register(ChatServerFactory.DEFAULT_TYPE, dynamicClientFactory);
    ChatServerFactory.register(NodeClientFactory.NODE_TYPE, NodeClientFactory.getInstance());
    ChatServerFactory.register(DynamicClientFactory.DYNAMIC_TYPE, dynamicClientFactory);
    ChatServerFactory.register(P2PClientFactory.P2P_TYPE, new P2PClientFactory());
    ChatServerFactory.register(JabberClientFactory.JABBER_SERVER_TYPE, new JabberClientFactory());
    server = new HybridClient();
    ServerConfigurer config = new ServerConfigurer("ServerImpl", "Server", (HybridClient) server); //$NON-NLS-1$ //$NON-NLS-2$
    GameModule.getGameModule().getGlobalPrefs().addOption(Resources.getString("Chat.server"), config); //$NON-NLS-1$
    serverControls = new ChatServerControls();
    serverControls.addTo(this);
  }

  protected void initLogger() {
    logger = new BasicLogger();
    ((BasicLogger) logger).build(null);
    ((BasicLogger) logger).addTo(this);
  }

  protected void initGameState() {
    theState = new GameState();
    theState.addTo(this);
    addCommandEncoder(theState);
  }

  public Command decode(String command) {
    if (command == null) {
      return null;
    }
    Command c = null;
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command, COMMAND_SEPARATOR);
    String first = st.nextToken();
    if (command.equals(first)) {
      c = decodeSubCommand(first);
    }
    else {
      Command next = null;
      c = decode(first);
      while (st.hasMoreTokens()) {
        next = decode(st.nextToken());
        c = c == null ? next : c.append(next);
      }
    }
    return c;
  }

  private Command decodeSubCommand(String subCommand) {
    Command c = null;
    for (int i = 0; i < commandEncoders.length && c == null; ++i) {
      c = commandEncoders[i].decode(subCommand);
    }
    return c;
  }

  public String encode(Command c) {
    if (c == null) {
      return null;
    }
    String s = encodeSubCommand(c);
    String s2;
    Command sub[] = c.getSubCommands();
    if (sub.length > 0) {
      SequenceEncoder se = new SequenceEncoder(s, COMMAND_SEPARATOR);
      for (int i = 0; i < sub.length; ++i) {
        s2 = encode(sub[i]);
        if (s2 != null) {
          se.append(s2);
        }
      }
      s = se.getValue();
    }
    return s;
  }

  private String encodeSubCommand(Command c) {
    String s = null;
    for (int i = 0; i < commandEncoders.length && s == null; ++i) {
      s = commandEncoders[i].encode(c);
    }
    return s;
  }

  protected void buildDefaultComponents() {
    addComponent(BasicCommandEncoder.class);
    addComponent(Documentation.class);
    addComponent(PlayerRoster.class);
    addComponent(GlobalOptions.class);
    addComponent(Map.class);
    addComponent(GamePieceImageDefinitions.class);
    addComponent(GlobalProperties.class);
    addComponent(PrototypesContainer.class);
    addComponent(PieceWindow.class);
    addComponent(Chatter.class);
    addComponent(Language.class);
  }

  protected void initFrame() {
    Rectangle screen = VASSAL.Info.getScreenBounds(frame);
    String key = "BoundsOfGameModule"; //$NON-NLS-1$
    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      frame.setLocation(screen.getLocation());
      frame.setSize(screen.width, screen.height / 3);
    }
    else {
      Rectangle r = new Rectangle(0, 0, screen.width, screen.height / 4);
      getPrefs().addOption(new PositionOption(key, frame, r));
    }
    String mess = Resources.getString("BasicModule.version_message", gameName, moduleVersion); //$NON-NLS-1$
    warn(mess);
    System.err.println("-- " + mess); //$NON-NLS-1$
    frame.setTitle(gameName);
    if (getArchiveWriter() == null) {
      getWizardSupport().showWelcomeWizard();
    }
    else {
      frame.setVisible(true);
    }
  }

  protected void ensureComponent(Class componentClass) {
    if (!getComponents(componentClass).hasMoreElements()) {
      addComponent(componentClass);
    }
  }

  protected void addComponent(Class componentClass) {
    try {
      Buildable child = (Buildable) componentClass.newInstance();
      child.build(null);
      child.addTo(this);
      add(child);
    }
    catch (InstantiationException e) {
      e.printStackTrace();
    }
    catch (IllegalAccessException e) {
      e.printStackTrace();
    }
  }
  
  public ChatServerControls getServerControls() {
    return serverControls;
  }
  
  /*
   * The module I18n key prefix is null for the top level.
   */
  public String getI18nPrefix() {
    return "";
  }

}
