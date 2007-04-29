package VASSAL.launch;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.zip.ZipFile;
import javax.swing.Action;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.WindowConstants;
import javax.swing.border.TitledBorder;
import VASSAL.build.GameModule;
import VASSAL.build.IllegalBuildException;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ExtensionsLoader;
import VASSAL.build.module.ModuleExtension;
import VASSAL.build.module.ServerConnection;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.FileChooser;
import VASSAL.tools.JarArchive;
import VASSAL.tools.SplashScreen;

public class Main {

  public static final String BUILT_IN = "auto";  //$NON-NLS-1$
  public static final String EXTENSION_LIST = "autoExtensions";  //$NON-NLS-1$
  public static final String LOCAL_INSTALL = "localInstall";  //$NON-NLS-1$
  protected Properties properties;
  protected PrefsEditor editor;
  protected boolean isFirstTime;
  protected boolean builtInModule;
  protected Prefs globalPrefs;
  protected File homeDir;
  public Main(final String[] args) {
    System.setProperty("swing.aatext", "true");   //$NON-NLS-1$ //$NON-NLS-2$
    System.setProperty("swing.boldMetal", "false");   //$NON-NLS-1$ //$NON-NLS-2$
    initHomeDir();
    initResources();
    initProperties();
    System.err.println("-- OS " + System.getProperty("os.name"));   //$NON-NLS-1$ //$NON-NLS-2$
    System.err.println("-- Java version " + System.getProperty("java.version"));   //$NON-NLS-1$ //$NON-NLS-2$
    String v = getVersion();
    System.err.println("-- VASSAL version " + v);  //$NON-NLS-1$
    final Thread t = new Thread(new ErrorLog.Group(), "Main Thread") {  //$NON-NLS-1$
      public void run() {
        Runnable runnable = new Runnable() {
          public void run() {
            Main.this.init(args);
          }
        };
        SwingUtilities.invokeLater(runnable);
      }
    };
    t.start();
  }

  protected String getVersion() {
    return VASSAL.Info.getVersion();
  }

  protected void init(final String[] args) {
    properties = parseArgs(args);
    File prefsFile = initPrefsFile();
    isFirstTime = !prefsFile.exists();
    if (isFirstTime) {
      showLicense();
    }
    builtInModule = properties.get(BUILT_IN) != null;
    final File installDir = properties.get(LOCAL_INSTALL) != null
      ? new File(System.getProperty("user.dir")) : null;  //$NON-NLS-1$
  
    editor = new PrefsEditor(new ArchiveWriter(prefsFile.getPath()));
    globalPrefs = new Prefs(editor, "VASSAL");  //$NON-NLS-1$
    final String[] extract = getExtractTargets(args);
    final PropertyChangeListener l = new PropertyChangeListener() {
      protected int next = 0;
  
      public void propertyChange(PropertyChangeEvent evt) {
        if (++next < extract.length) {
          extractResource(extract[next], this, globalPrefs, installDir);
        }
        else {
          launchVassal(globalPrefs, args);
        }
      }
    };
    if (extract.length > 0) {
      extractResource(extract[0], l, globalPrefs, installDir);
    }
    else {
      launchVassal(globalPrefs, args);
    }
  }
  
  protected Properties parseArgs(String[] args) {
    Properties props = new Properties();
    for (int i = 0;i < args.length;++i) {
      if (args[i].startsWith("-")) {  //$NON-NLS-1$
        if (i < args.length-1
          && !args[i+1].startsWith("-")) {  //$NON-NLS-1$
          props.put(args[i].substring(1),args[++i]);
        }
        else {
          props.put(args[i].substring(1),"");  //$NON-NLS-1$
        }
      }
    }
    return props;
  }

  protected File initPrefsFile() {
    File prefsFile = new File(homeDir, "Preferences");  //$NON-NLS-1$
    File oldFile = new File(System.getProperty("user.home")  //$NON-NLS-1$
                            + File.separator + ".VassalPreferences");  //$NON-NLS-1$
    if (!prefsFile.exists()
        && oldFile.exists()) {
      oldFile.renameTo(prefsFile);
    }
    return prefsFile;
  }

  protected void initHomeDir() {
    homeDir = new File(System.getProperty("user.home"), "VASSAL");   //$NON-NLS-1$ //$NON-NLS-2$
    if (!homeDir.exists()) {
      homeDir.mkdir();
    }
    else if (!homeDir.isDirectory()) {
      homeDir.delete();
      homeDir.mkdir();
    }
  }
  
  protected void initResources() {
    Resources.init(homeDir);
  }

  protected void newExtension() {
    ModuleExtension ext = new ModuleExtension(new ArchiveWriter((String) null));
    ext.build();
    new VASSAL.configure.ExtensionEditWindow(ext).setVisible(true);
  }

  protected void loadExtension(File f) throws IllegalBuildException, IOException {
    ModuleExtension ext = new ModuleExtension(new ArchiveWriter(new ZipFile(f.getPath())));
    ext.build();
    new VASSAL.configure.ExtensionEditWindow(ext).setVisible(true);
  }

  protected void newModule() throws IOException {
    GameModule.init(new BasicModule(new ArchiveWriter((String) null), globalPrefs));
    new VASSAL.configure.ModuleEditWindow().setVisible(true);
  }

  protected void edit(File moduleFile) throws IOException {
    ArchiveWriter archive = new ArchiveWriter(new ZipFile(moduleFile.getPath()));
    BasicModule mod = new BasicModule(archive, globalPrefs);
    mod.setGlobalPrefs(globalPrefs);
    GameModule.init(mod);
    new VASSAL.configure.ModuleEditWindow().setVisible(true);
  }

  /** Exit the Application */
  protected void quit() {
    if (GameModule.getGameModule() != null) {
      GameModule.getGameModule().quit();
    }
    else {
      System.exit(0);
    }
  }

  public static void main(final String[] args) {
    new Main(args);
  }

  protected void initProperties() {
    if (System.getProperty("stderr") == null) {  //$NON-NLS-1$
      System.setProperty("stderr", System.getProperty("user.home") + File.separator + "VASSAL" + File.separator + "errorLog"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    }
    if (!"null".equals(System.getProperty("stderr"))) {   //$NON-NLS-1$ //$NON-NLS-2$
      try {
        System.setErr(new PrintStream(new FileOutputStream(System.getProperty("stderr"))));  //$NON-NLS-1$
      }
      catch (IOException ex) {
        System.err.println("Unable to redirect stderr to " + System.getProperty("stderr"));   //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    System.getProperties().put("sun.awt.exception.handler", "VASSAL.tools.ErrorLog");   //$NON-NLS-1$ //$NON-NLS-2$
    if (System.getProperty("http.proxyHost") == null  //$NON-NLS-1$
        && System.getProperty("proxyHost") != null) {  //$NON-NLS-1$
      System.setProperty("http.proxyHost", System.getProperty("proxyHost"));   //$NON-NLS-1$ //$NON-NLS-2$
    }
    if (System.getProperty("http.proxyPort") == null  //$NON-NLS-1$
        && System.getProperty("proxyPort") != null) {  //$NON-NLS-1$
      System.setProperty("http.proxyPort", System.getProperty("proxyPort"));   //$NON-NLS-1$ //$NON-NLS-2$
    }
  }

  protected void showLicense() {
    try {
      final JDialog d = new JDialog((Frame) null, true);
      d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(Resources.getString("Main.license_agreement"));  //$NON-NLS-1$
      d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
      final JTextArea text = new JTextArea(20, 40);
      text.setEditable(false);
      JScrollPane scroll = new JScrollPane(text);
      d.getContentPane().add(scroll);
      Box b = Box.createHorizontalBox();
      JButton accept = new JButton(Resources.getString("Main.accept"));  //$NON-NLS-1$
      accept.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          d.dispose();
        }
      });
      JButton decline = new JButton(Resources.getString("Main.decline"));  //$NON-NLS-1$
      decline.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          System.exit(0);
        }
      });
      b.add(accept);
      b.add(decline);
      d.getContentPane().add(b);
      InputStream inStream = Main.class.getResourceAsStream("/LICENSE.txt");  //$NON-NLS-1$
      if (inStream != null) {
        BufferedReader in = new BufferedReader(new InputStreamReader(inStream));
        String line = null;
        while ((line = in.readLine()) != null) {
          text.append(line + '\n');
        }
        d.pack();
        Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
        d.setLocation(screen.width / 2 - d.getSize().width / 2, screen.height / 2 - d.getSize().height / 2);
        Runnable runnable = new Runnable() {
          public void run() {
            text.scrollRectToVisible(new Rectangle());
          }
        };
        SwingUtilities.invokeLater(runnable);
        d.setVisible(true);
      }
    }
    catch (IOException e) {
    }
  }

  public void extractResource(String infoResourceName, PropertyChangeListener l, Prefs prefs, File installDir) {
    Properties props = new Properties();
    InputStream resource = Main.class.getResourceAsStream(infoResourceName);
    if (resource != null) {
      try {
        props.load(resource);
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    new VASSAL.launch.ResourceExtracter(prefs, props, l, installDir).install();
  }

  protected String[] getExtractTargets(String[] args) {
    ArrayList l = new ArrayList();
    for (int i = 0; i < args.length; ++i) {
      if (args[i].startsWith("-ext")  //$NON-NLS-1$
          && i < args.length - 1) {
        l.add(args[i + 1]);
      }
    }
    return (String[]) l.toArray(new String[l.size()]);
  }

  protected void launchVassal(Prefs prefs, String[] args) {
    if (properties.getProperty("install") != null) {  //$NON-NLS-1$
      System.exit(0);
    }
    else {
      final File moduleFile = getModuleFile(properties, prefs, args);
      if (builtInModule
          || (moduleFile != null && moduleFile.exists())) {
        final String saveFile = properties.getProperty("load");  //$NON-NLS-1$
        try {
          if (properties.getProperty("edit") != null) {  //$NON-NLS-1$
            edit(moduleFile);
          }
          else {
            final DataArchive archive = initArchive(moduleFile);
            SplashScreen splash = null;
            final Observer o = new Observer() {
              public void update(Observable o, Object arg) {
                try {
                  loadModule(archive, true, properties);
                  if (saveFile != null) {
                    GameModule.getGameModule().getGameState().loadGameInBackground(new File(saveFile));
                  }
                }
                catch (IOException e) {
                  JOptionPane.showMessageDialog(null, Resources.getString("Main.open_module_error", moduleFile.getPath()));  //$NON-NLS-1$
                  new Frame().setVisible(true);
                }
                finally {
                  if (arg instanceof SplashScreen) {
                    ((SplashScreen) arg).dispose();
                  }
                }
              }
            };
            try {
              splash = new SplashScreen(archive.getCachedImage("Splash.gif")) {  //$NON-NLS-1$
                private static final long serialVersionUID = 1L;

                protected boolean triggered = false;
  
                public void paint(Graphics g) {
                  super.paint(g);
                  if (!triggered) {
                    triggered = true;
                    o.update(null, this);
                  }
                }
              };
            }
            catch (IOException ex) {
            }
            if (splash != null
              && splash.getWidth() * splash.getHeight() > 0) {
              splash.setVisible(true);
            }
            else {
              o.update(null, null);
            }
          }
        }
        catch (IOException ex) {
          JOptionPane.showMessageDialog(null, Resources.getString("Main.open_module_error", moduleFile.getPath()));  //$NON-NLS-1$
          new Frame().setVisible(true);
        }
      }
      else {
        if (moduleFile != null) {
          JOptionPane.showMessageDialog(null, Resources.getString("Main.not_found_error", moduleFile.getPath()));  //$NON-NLS-1$
        }
        if (isFirstTime) {
          String docsDir = prefs.getStoredValue(Documentation.DOCS_DIR);
          File base = docsDir != null ? new File(docsDir) : null;
          File tourMod = new File(base, "tour.mod");  //$NON-NLS-1$
          File tourLog = new File(base, "tour.log");  //$NON-NLS-1$
          if (tourMod.exists()
              && tourLog.exists()) {
            loadTour(tourMod, tourLog);
          }
          else {
            new Frame().setVisible(true);
  
          }
        }
        else {
          new Frame().setVisible(true);
        }
      }
    }
  }

  protected File getModuleFile(Properties p, Prefs prefs, String[] args) {
    String fileName = null;
    switch (args.length) {
      case 0:
        break;
      case 1:
        fileName = args[0].startsWith("-") ? null : args[0];  //$NON-NLS-1$
        break;
      default:
        fileName = p.getProperty("edit");  //$NON-NLS-1$
        if (fileName == null
            && !args[args.length - 1].startsWith("-")  //$NON-NLS-1$
            && !args[args.length - 2].startsWith("-")) {  //$NON-NLS-1$
          fileName = args[args.length - 1];
        }
    }
    File f = null;
    if (fileName != null) {
      String dir = p.getProperty("moduleDir");  //$NON-NLS-1$
      if (dir != null) {
        f = new File(prefs.getStoredValue(dir), fileName);
      }
      else {
        f = new File(fileName);
      }
    }
    return f;
  }

  protected void loadTour(final File tourMod, final File tourLog) {
    final JFrame f = new JFrame();
    f.getContentPane().setLayout(new BoxLayout(f.getContentPane(), BoxLayout.Y_AXIS));
    JLabel l = new JLabel();
    l.setFont(new Font("SansSerif", 1, 40));  //$NON-NLS-1$
    l.setText(Resources.getString("Main.welcome"));  //$NON-NLS-1$
    l.setForeground(Color.black);
    l.setAlignmentX(0.5F);
    f.getContentPane().add(l);
    Box b = Box.createHorizontalBox();
    JButton tour = new JButton(Resources.getString("Main.tour"));  //$NON-NLS-1$
    JButton jump = new JButton(Resources.getString("Main.jump_right_in"));  //$NON-NLS-1$
    b.add(tour);
    b.add(jump);
    JPanel p = new JPanel();
    p.add(b);
    f.getContentPane().add(p);
    tour.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        f.dispose();
        try {
          GameModule.init(new BasicModule(initArchive(tourMod), globalPrefs));
          GameModule.getGameModule().getGameState().loadGameInBackground(tourLog);
        }
        catch (Exception e) {
          e.printStackTrace();
          JOptionPane.showMessageDialog
              (null,
               e.getMessage(),
               Resources.getString("Main.open_error"),  //$NON-NLS-1$
               JOptionPane.ERROR_MESSAGE);
          new Frame().setVisible(true);
        }
      }
    });
    jump.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent evt) {
        f.dispose();
        new Frame().setVisible(true);
      }
    });
    f.pack();
    Dimension d = Toolkit.getDefaultToolkit().getScreenSize();
    f.setLocation(d.width / 2 - f.getSize().width / 2,
                  d.height / 2 - f.getSize().height);
    f.setVisible(true);
  }

  protected void loadModule(DataArchive archive, final boolean includeExtensions, Properties properties) throws IOException {
    GameModule.init(new BasicModule(archive, globalPrefs));
    if (includeExtensions) {
      String extensionList = properties.getProperty(EXTENSION_LIST);
      if (extensionList != null) {
        StringTokenizer st = new StringTokenizer(extensionList,",");  //$NON-NLS-1$
        while (st.hasMoreTokens()) {
          String name = st.nextToken();
          loadExtension(name);
        }
      }
      else {
          new ExtensionsLoader().addTo(GameModule.getGameModule());
      }
    }
  }

  protected void loadExtension(String name) {
    new ModuleExtension(new JarArchive(name)).build();
  }

  protected DataArchive initArchive(File f) throws IOException {
    DataArchive archive;
    if (builtInModule) {
      archive = new JarArchive();
    }
    else {
      archive = new DataArchive(f.getPath());
    }
    return archive;
  }
  
  protected ServerConnection initClient() {
    return null;
  }

  protected Action initShowServerStatusAction() {
    return null;
  }

  protected  class Frame extends JFrame {
    private static final long serialVersionUID = 1L;

    protected javax.swing.JButton openButton;
    protected javax.swing.JButton editButton;
    protected javax.swing.JButton newButton;
    protected javax.swing.JLabel splash;
    protected java.awt.Color fgColor = java.awt.Color.black;
    protected FileChooser fc;

    public Frame() {
      super(Resources.getString(Resources.VASSAL)); 
      initComponents();
      pack();
      java.awt.Dimension d = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
      setLocation(d.width / 2 - getSize().width / 2,
                  d.height / 2 - getSize().height / 2);
      String baseDir = globalPrefs.getStoredValue(Documentation.DOCS_DIR);
      if (baseDir == null) {
        baseDir = System.getProperty("user.home");  //$NON-NLS-1$
      }
      fc = FileChooser.createFileChooser(this);
      fc.setCurrentDirectory(new File(baseDir));
    }

    protected void initComponents() {
      setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);
      addWindowListener(new java.awt.event.WindowAdapter() {
        public void windowClosing(java.awt.event.WindowEvent evt) {
          quit();
        }
      }
      );

      JPanel controlPanel = new JPanel();
      controlPanel.setLayout(new javax.swing.BoxLayout(controlPanel, BoxLayout.Y_AXIS));

      splash = new javax.swing.JLabel();
      splash.setAlignmentX(0.5F);
      splash.setText(Resources.getString(Resources.VASSAL)); 
      splash.setFont(new java.awt.Font("SansSerif", 1, 48));  //$NON-NLS-1$
      splash.setName("splash");  //$NON-NLS-1$
      splash.setForeground(fgColor);
      controlPanel.add(splash);

      JLabel l2 = new JLabel(Resources.getString("Main.version", VASSAL.Info.getVersion()));  //$NON-NLS-1$
      l2.setAlignmentX(0.5F);
      l2.setFont(new java.awt.Font("SansSerif", 1, 12));  //$NON-NLS-1$
      l2.setForeground(fgColor);
      controlPanel.add(l2);

      Box box = Box.createHorizontalBox();
      openButton = new javax.swing.JButton();
      openButton.setText(Resources.getString("Main.play_module"));  //$NON-NLS-1$
      openButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          try {
            File f = chooseFile();
            if (f != null) {
              loadModule(initArchive(f), true, properties);
              if (GameModule.getGameModule() != null) {
                dispose();
              }
            }
          }
          catch (Exception e) {
            e.printStackTrace();
            JOptionPane.showMessageDialog
                (Frame.this,
                 e.getMessage(),
                 Resources.getString("Main.open_error"),  //$NON-NLS-1$
                 JOptionPane.ERROR_MESSAGE);
          }
        }
      }
      );
      box.add(openButton);
      Action a = initShowServerStatusAction();
      if (a != null) {
        JButton statusButton = new JButton((String) a.getValue(Action.NAME));
        statusButton.addActionListener(a);
        box.add(statusButton);
      }
      controlPanel.add(box);

      box = Box.createHorizontalBox();
      editButton = new javax.swing.JButton();
      editButton.setText(Resources.getString("Main.edit_module"));  //$NON-NLS-1$
      editButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          File f = chooseFile();
          if (f != null) {
            try {
              edit(f);
              dispose();
            }
            catch (IOException e) {
              JOptionPane.showMessageDialog(Frame.this, e.getMessage(), Resources.getString("Main.load_error"), JOptionPane.ERROR_MESSAGE);  //$NON-NLS-1$
            }
          }
        }
      }
      );
      box.add(editButton);

      newButton = new javax.swing.JButton();
      newButton.setText(Resources.getString("Main.new_module"));  //$NON-NLS-1$
      newButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          try {
            newModule();
            dispose();
          }
          catch (IOException e) {
            e.printStackTrace();
            JOptionPane.showMessageDialog
                (Frame.this,
                 e.getMessage(),
                 Resources.getString("Main.create_error"),  //$NON-NLS-1$
                 JOptionPane.ERROR_MESSAGE);
          }
        }
      }
      );
      box.add(newButton);
      box.setBorder(new TitledBorder(Resources.getString("Main.module_design")));  //$NON-NLS-1$
      controlPanel.add(box);

      JPanel p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      final JButton loadModule = new JButton(Resources.getString("Main.load_module"));  //$NON-NLS-1$
      final JButton loadExtension = new JButton(Resources.getString(Resources.EDIT)); 
      loadExtension.setEnabled(false);
      final JButton newExtension = new JButton(Resources.getString(Resources.NEW)); 
      newExtension.setEnabled(false);
      loadModule.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          try {
            File f = chooseFile();
            if (f != null) {
              loadModule(initArchive(f), false, properties);
              loadModule.setEnabled(false);
              loadExtension.setEnabled(true);
              newExtension.setEnabled(true);
            }
          }
          catch (IOException ex) {
            JOptionPane.showMessageDialog
                (Frame.this,
                 ex.getMessage(),
                 Resources.getString("Main.open_error"),  //$NON-NLS-1$
                 JOptionPane.ERROR_MESSAGE);
          }
        }
      });
      loadExtension.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          File f = chooseFile();
          if (f != null) {
            try {
              loadExtension(f);
              dispose();
            }
            catch (Exception ex) {
              ex.printStackTrace();
              JOptionPane.showMessageDialog(Frame.this, Resources.getString("Main.unable_to_load", f.getName(), ex.getMessage()), Resources.getString("Main.load_failed"), JOptionPane.ERROR_MESSAGE); //$NON-NLS-1$ //$NON-NLS-2$
            }
          }
        }
      });
      newExtension.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          newExtension();
          dispose();
        }
      });
      p.add(loadModule);
      p.add(loadExtension);
      p.add(newExtension);
      p.setBorder(new TitledBorder(Resources.getString("Main.extension_design")));  //$NON-NLS-1$
      controlPanel.add(p);

      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      getContentPane().add(controlPanel);
    }

    protected File chooseFile() {
      if (fc.showOpenDialog() != FileChooser.APPROVE_OPTION) return null;

      if (!fc.getSelectedFile().exists()) {
         JOptionPane.showMessageDialog(this,
         Resources.getString("Main.not_found_error", fc.getSelectedFile().toString()));  //$NON-NLS-1$
         return null;
      }
      else {
        return fc.getSelectedFile();
      }
    }
  }
}
