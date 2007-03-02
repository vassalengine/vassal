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
import VASSAL.preferences.Prefs;
import VASSAL.preferences.PrefsEditor;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.DataArchive;
import VASSAL.tools.ErrorLog;
import VASSAL.tools.FileChooser;
import VASSAL.tools.JarArchive;
import VASSAL.tools.SplashScreen;

public class Main {

  public static final String BUILT_IN = "auto";
  public static final String EXTENSION_LIST = "autoExtensions";
  public static final String LOCAL_INSTALL = "localInstall";
  protected Properties properties;
  protected PrefsEditor editor;
  protected boolean isFirstTime;
  protected boolean builtInModule;
  protected Prefs globalPrefs;
  protected File homeDir;
  public Main(final String[] args) {
    System.setProperty("swing.aatext", "true");
    System.setProperty("swing.boldMetal", "false");
    initHomeDir();
    initProperties();
    System.err.println("-- OS " + System.getProperty("os.name"));
    System.err.println("-- Java version " + System.getProperty("java.version"));
    String v = getVersion();
    System.err.println("-- VASSAL version " + v);
    final Thread t = new Thread(new ErrorLog.Group(), "Main Thread") {
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
      ? new File(System.getProperty("user.dir")) : null;
  
    editor = new PrefsEditor(new ArchiveWriter(prefsFile.getPath()));
    globalPrefs = new Prefs(editor, "VASSAL");
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
      if (args[i].startsWith("-")) {
        if (i < args.length-1
          && !args[i+1].startsWith("-")) {
          props.put(args[i].substring(1),args[++i]);
        }
        else {
          props.put(args[i].substring(1),"");
        }
      }
    }
    return props;
  }

  protected File initPrefsFile() {
    File prefsFile = new File(homeDir, "Preferences");
    File oldFile = new File(System.getProperty("user.home")
                            + File.separator + ".VassalPreferences");
    if (!prefsFile.exists()
        && oldFile.exists()) {
      oldFile.renameTo(prefsFile);
    }
    return prefsFile;
  }

  protected void initHomeDir() {
    homeDir = new File(System.getProperty("user.home"), "VASSAL");
    if (!homeDir.exists()) {
      homeDir.mkdir();
    }
    else if (!homeDir.isDirectory()) {
      homeDir.delete();
      homeDir.mkdir();
    }
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
    if (System.getProperty("stderr") == null) {
      System.setProperty("stderr", System.getProperty("user.home") + File.separator + "VASSAL" + File.separator + "errorLog");
    }
    if (!"null".equals(System.getProperty("stderr"))) {
      try {
        System.setErr(new PrintStream(new FileOutputStream(System.getProperty("stderr"))));
      }
      catch (IOException ex) {
        System.err.println("Unable to redirect stderr to " + System.getProperty("stderr"));
      }
    }
    System.getProperties().put("sun.awt.exception.handler", "VASSAL.tools.ErrorLog");
    if (System.getProperty("http.proxyHost") == null
        && System.getProperty("proxyHost") != null) {
      System.setProperty("http.proxyHost", System.getProperty("proxyHost"));
    }
    if (System.getProperty("http.proxyPort") == null
        && System.getProperty("proxyPort") != null) {
      System.setProperty("http.proxyPort", System.getProperty("proxyPort"));
    }
  }

  protected void showLicense() {
    try {
      final JDialog d = new JDialog((Frame) null, true);
      d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle("License Agreement");
      d.getContentPane().setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
      final JTextArea text = new JTextArea(20, 40);
      text.setEditable(false);
      JScrollPane scroll = new JScrollPane(text);
      d.getContentPane().add(scroll);
      Box b = Box.createHorizontalBox();
      JButton accept = new JButton("Accept");
      accept.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          d.dispose();
        }
      });
      JButton decline = new JButton("Decline");
      decline.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          System.exit(0);
        }
      });
      b.add(accept);
      b.add(decline);
      d.getContentPane().add(b);
      InputStream inStream = Main.class.getResourceAsStream("/LICENSE.txt");
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
      if (args[i].startsWith("-ext")
          && i < args.length - 1) {
        l.add(args[i + 1]);
      }
    }
    return (String[]) l.toArray(new String[l.size()]);
  }

  protected void launchVassal(Prefs prefs, String[] args) {
    if (properties.getProperty("install") != null) {
      System.exit(0);
    }
    else {
      final File moduleFile = getModuleFile(properties, prefs, args);
      if (builtInModule
          || (moduleFile != null && moduleFile.exists())) {
        final String saveFile = properties.getProperty("load");
        try {
          if (properties.getProperty("edit") != null) {
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
                    GameModule.getGameModule().getGameState().loadGame(new File(saveFile));
                  }
                }
                catch (IOException e) {
                  JOptionPane.showMessageDialog(null, "Unable to open module " + moduleFile.getPath());
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
              splash = new SplashScreen(archive.getCachedImage("Splash.gif")) {
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
          JOptionPane.showMessageDialog(null, "Unable to open module " + moduleFile.getPath());
          new Frame().setVisible(true);
        }
      }
      else {
        if (moduleFile != null) {
          JOptionPane.showMessageDialog(null, moduleFile.getPath() + " not found");
        }
        if (isFirstTime) {
          String docsDir = prefs.getStoredValue(Documentation.DOCS_DIR);
          File base = docsDir != null ? new File(docsDir) : null;
          File tourMod = new File(base, "tour.mod");
          File tourLog = new File(base, "tour.log");
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
        fileName = args[0].startsWith("-") ? null : args[0];
        break;
      default:
        fileName = p.getProperty("edit");
        if (fileName == null
            && !args[args.length - 1].startsWith("-")
            && !args[args.length - 2].startsWith("-")) {
          fileName = args[args.length - 1];
        }
    }
    File f = null;
    if (fileName != null) {
      String dir = p.getProperty("moduleDir");
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
    l.setFont(new Font("SansSerif", 1, 40));
    l.setText("Welcome to VASSAL");
    l.setForeground(Color.black);
    l.setAlignmentX(0.5F);
    f.getContentPane().add(l);
    Box b = Box.createHorizontalBox();
    JButton tour = new JButton("Take the tour");
    JButton jump = new JButton("Jump right in");
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
          GameModule.getGameModule().getGameState().loadGame(tourLog);
        }
        catch (Exception e) {
          e.printStackTrace();
          JOptionPane.showMessageDialog
              (null,
               e.getMessage(),
               "Error opening module",
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
        StringTokenizer st = new StringTokenizer(extensionList,",");
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
    protected javax.swing.JButton openButton;
    protected javax.swing.JButton editButton;
    protected javax.swing.JButton newButton;
    protected javax.swing.JLabel splash;
    protected java.awt.Color fgColor = java.awt.Color.black;
    protected FileChooser fc;

    public Frame() {
      super("VASSAL");
      initComponents();
      pack();
      java.awt.Dimension d = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
      setLocation(d.width / 2 - getSize().width / 2,
                  d.height / 2 - getSize().height / 2);
      String baseDir = globalPrefs.getStoredValue(Documentation.DOCS_DIR);
      if (baseDir == null) {
        baseDir = System.getProperty("user.home");
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
      splash.setText("VASSAL");
      splash.setFont(new java.awt.Font("SansSerif", 1, 48));
      splash.setName("splash");
      splash.setForeground(fgColor);
      controlPanel.add(splash);

      JLabel l2 = new JLabel("version " + VASSAL.Info.getVersion());
      l2.setAlignmentX(0.5F);
      l2.setFont(new java.awt.Font("SansSerif", 1, 12));
      l2.setForeground(fgColor);
      controlPanel.add(l2);

      Box box = Box.createHorizontalBox();
      openButton = new javax.swing.JButton();
      openButton.setText("Play Module");
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
            javax.swing.JOptionPane.showMessageDialog
                (Frame.this,
                 e.getMessage(),
                 "Error opening module",
                 javax.swing.JOptionPane.ERROR_MESSAGE);
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
      editButton.setText("Edit Module");
      editButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          File f = chooseFile();
          if (f != null) {
            try {
              edit(f);
              dispose();
            }
            catch (IOException e) {
              JOptionPane.showMessageDialog(Frame.this, e.getMessage(), "Load Error", JOptionPane.ERROR_MESSAGE);
            }
          }
        }
      }
      );
      box.add(editButton);

      newButton = new javax.swing.JButton();
      newButton.setText("New Module");
      newButton.addActionListener(new java.awt.event.ActionListener() {
        public void actionPerformed(java.awt.event.ActionEvent evt) {
          try {
            newModule();
            dispose();
          }
          catch (IOException e) {
            e.printStackTrace();
            javax.swing.JOptionPane.showMessageDialog
                (Frame.this,
                 e.getMessage(),
                 "Error creating module",
                 javax.swing.JOptionPane.ERROR_MESSAGE);
          }
        }
      }
      );
      box.add(newButton);
      box.setBorder(new TitledBorder("Module Design"));
      controlPanel.add(box);

      JPanel p = new JPanel();
      p.setLayout(new BoxLayout(p, BoxLayout.X_AXIS));
      final JButton loadModule = new JButton("Load Module");
      final JButton loadExtension = new JButton("Edit");
      loadExtension.setEnabled(false);
      final JButton newExtension = new JButton("New");
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
            javax.swing.JOptionPane.showMessageDialog
                (Frame.this,
                 ex.getMessage(),
                 "Error opening module",
                 javax.swing.JOptionPane.ERROR_MESSAGE);
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
              JOptionPane.showMessageDialog(Frame.this, "Unable to load " + f.getName() + "\n." + ex.getMessage(), "Load failed", JOptionPane.ERROR_MESSAGE);
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
      p.setBorder(new TitledBorder("Extension Design"));
      controlPanel.add(p);

      getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
      getContentPane().add(controlPanel);
    }

    protected File chooseFile() {
      if (fc.showOpenDialog() != FileChooser.APPROVE_OPTION) return null;

      if (!fc.getSelectedFile().exists()) {
         JOptionPane.showMessageDialog(this,
           fc.getSelectedFile() + " not found");
         return null;
      }
      else {
        return fc.getSelectedFile();
      }
    }
  }
}
