/*
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.library;

import org.jdesktop.swingx.JXTreeTable;
import org.jdesktop.swingx.treetable.AbstractTreeTableModel;

import org.apache.commons.lang3.SystemUtils;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
// For future upgrade
// import java.text.ParseException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Stream;
import javax.swing.AbstractAction;
import javax.swing.AbstractCellEditor;
import javax.swing.Action;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.SwingWorker;
import javax.swing.Timer;
import javax.swing.UIManager;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.TableColumnModelEvent;
import javax.swing.event.TableColumnModelListener;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeWillExpandListener;
import javax.swing.plaf.UIResource;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;
import javax.swing.tree.ExpandVetoException;
import javax.swing.tree.TreePath;
import java.text.DecimalFormat;

// For future upgrade
// import VASSAL.Info;
import VASSAL.build.module.Documentation;
import VASSAL.configure.ShowHelpAction;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.launch.AboutVASSALAction;
import VASSAL.preferences.Prefs;
import VASSAL.tools.BrowserSupport;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.lang.Pair;
import VASSAL.tools.menu.CheckBoxMenuItemProxy;
import VASSAL.tools.menu.MenuBarProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.menu.MenuProxy;
import VASSAL.tools.menu.RadioButtonMenuItemProxy;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.apache.commons.lang3.ArrayUtils;

/**
 * Displays queries from the GameLibrary in a table tree.  Allows user
 * to select files to download.  Marking a parent node will mark
 * (ultimate) file leafs for download.  Already downloaded files are
 * read from the V_Global preferences, and re-downloads of these are
 * disabled (could later on be configurable).  Downloaded files are
 * added to the V_Global preferences so that they are immediately
 * available to the user in the Module Manager (may need a fresh
 * start).
 */
public class GameLibraryWindow extends JFrame {
  private static final Logger logger =
    LoggerFactory.getLogger(GameLibraryWindow.class);

  private static int DETAIL_PROGRESS_DELAY = 400; // miliseconds 
  private static String LIBRARY_URL = "https://vassalengine.org/library/projects/"; //NON-NLS
  private static final long serialVersionUID = 1L;

  protected final GameLibrary           library;
  protected       JXTreeTable           treeTable;
  protected       LibraryTreeTableModel model;
  protected       JProgressBar          progressBar;
  protected       JButton               downloadButton;
  protected       Map<String, Project>  projectsMap;
  protected       GameLibrary.AbstractProjectsComparator comparator = GameLibrary.getTitleComparator(true);

  // For future upgrade
  // public static SemVer              vassalVersion;
  protected        StringArrayConfigurer recentModules;
  protected        StringArrayConfigurer registeredModules;
  protected static List<String>          moduleList = new ArrayList<>();

  private static final String[] COLUMN_NAMES = {
    "<html>&#10003;</html>",      //NON-NLS
    Resources.getString("LibraryBrowser.name"),  //NON-NLS
    Resources.getString("LibraryBrowser.type"),  //NON-NLS
    Resources.getString("LibraryBrowser.last_update"),  //NON-NLS
    Resources.getString("LibraryBrowser.size"),  //NON-NLS
    Resources.getString("LibraryBrowser.url") }; //NON-NLS

  public static final int CHECK_COLUMN = 0;
  public static final int NAME_COLUMN  = 1;
  public static final int TYPE_COLUMN  = 2;
  public static final int DATE_COLUMN  = 3;
  public static final int SIZE_COLUMN  = 4;
  public static final int URL_COLUMN   = 5;
  
  protected static String formatSize(long size) {
    if (size < 0) // When not loaded yet, we get negative value
      return "?"; //NON-NLS
    if (size == 0) // Uh?!
      return ""; //NON-NLS
    
    final String[] units  = { "B", "kB", "MB", "GB", "TB", "PB", "EB" }; //NON-NLS
    final int digitGroups = (int) (Math.log10(size)/Math.log10(1024));
    
    return new DecimalFormat("#,##0.#").format(size/Math.pow(1024, //NON-NLS
                                                             digitGroups))
      + " " + units[digitGroups]; //NON-NLS
  }
    
  public GameLibraryWindow() throws IOException {
    super("Game Library"); //NON-NLS
    
    // For future upgrade
    //
    // try {
    //   String       ver = Info.getVersion();
    //   final int    idx = ver.indexOf("-SNAPSHOT");
    //   if (idx > 0)
    //     ver = ver.substring(0, idx + 9);
    //   
    //   vassalVersion = SemVer.parse(ver);
    // }
    // catch (ParseException ignored) {
    //   vassalVersion = null;
    // }
    
    // --- Our interface to the GLS ---
    library = new GameLibrary();

    // --- When we get a shut-down request ---
    final AbstractAction shutDownAction = new AbstractAction() {
      private static final long serialVersionUID = 1L;
    
      @Override
      public void actionPerformed(ActionEvent e) {
        // saveColumnWidths();
        final Prefs gp = Prefs.getGlobalPrefs();
        try {
          gp.close();
          // shutdown();
        }
        catch (IOException ex) {
          WriteErrorDialog.error(ex, gp.getFile());
        }
        // Make sure logger isn't unused
        logger.info("Exiting"); //NON-NLS
        System.exit(0);
      }
    };
    shutDownAction.putValue(Action.NAME, Resources.getString("General.quit"));

    // --- Build the UI ---
    setupMainInterface(shutDownAction);
    setupMenu(shutDownAction);

    // --- Register global preferences that we need ---
    Prefs.getGlobalPrefs().getEditor().initDialog(this);
    Prefs.initSharedGlobalPrefs();
    
    recentModules = new StringArrayConfigurer("RecentModules", null); //NON-NLS
    Prefs.getGlobalPrefs().addOption(null, recentModules);

    registeredModules = new StringArrayConfigurer("Modules", null); //NON-NLS
    Prefs.getGlobalPrefs().addOption(null, registeredModules);

    // Read known modules from preferences file 
    updateKnownModules();

    // Fetch initial list of projects
    loadProjects();
  }

  /**
   * A Boolean (CheckBox) renderer that may show nothing (tri-state)
   */
  static class BooleanRenderer extends JCheckBox
    implements TableCellRenderer, UIResource {
    
    private static final long serialVersionUID = 1L;
    protected JLabel nullComponent;
    
    public BooleanRenderer() {
      super();
      nullComponent = new JLabel();
      nullComponent.setOpaque(true);
      setHorizontalAlignment(CENTER);
      setBorderPainted(true);
      setOpaque(true);
    }

    @Override
    public Component getTableCellRendererComponent(JTable table,
                                                   Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus,
                                                   int row,
                                                   int column) {
      // We may get called with something other than boolean. If so,
      // take it to mean no defined value (tri-state: true, false,
      // unknown).
      final Component ret = (!(value instanceof Boolean)) ?
        nullComponent : this;

      if (isSelected) {
        ret.setForeground(table.getSelectionForeground());
        ret.setBackground(table.getSelectionBackground());
      }
      else {
        ret.setForeground(table.getForeground());
        ret.setBackground(table.getBackground());
      }
      if (ret == this) 
        this.setSelected(value != null && ((Boolean)value).booleanValue());

      ret.repaint();
      
      return ret;
    }
    @Override
    public void updateUI() {
      super.updateUI();
      if (nullComponent != null)
        nullComponent.updateUI();
    }
  }
  /**
   * "Edit" a URL - open URL in web-browser 
   */
  private static class URLEditor extends AbstractCellEditor
    implements TableCellEditor {
    private static final long serialVersionUID = 1L;
    private String url;
    private JLabel comp;

    @Override
    public Component getTableCellEditorComponent(JTable  table,
                                                 Object  value,
                                                 boolean isSelected,
                                                 int     row,
                                                 int     column) {
      if (comp == null)
        comp = new JLabel();
      
      url = value.toString();
      comp.setText(url);
      
      BrowserSupport.openURL(url);
      stopCellEditing();

      return comp;
    }
    
    @Override
    public Object getCellEditorValue() {
      return url;
    }
  }

  /**
   * How to render dates - horizontally centred
   */
  static class DateRenderer extends DefaultTableCellRenderer.UIResource {
    private static final long serialVersionUID = 1L;
    private DateFormat formatter;
    public DateRenderer() {
      super();
      setHorizontalAlignment(CENTER);
    }

    @Override
    public void setValue(Object value) {
      if (formatter == null) {
        formatter = DateFormat.getDateInstance();
      }
      if (value instanceof Date)
        setText((value == null) ? "" : formatter.format(value));
    }
  }

  /**
   * Create the main user interface
   */
  protected void setupMainInterface(AbstractAction shutDownAction) {
    setTitle(Resources.getString("LibraryBrowser.window_title")); //NON-NLS

    // --- Handle close --- 
    setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      @Override
      public void windowClosing(WindowEvent e) {
        shutDownAction.actionPerformed(null);
      }
    }
    );
    
    // --- UI Setup ---
    setSize(1200, 600);
    setLocationRelativeTo(null);
    setLayout(new BorderLayout());

    // Setup TreeTable and Model
    final RootNode rootNode = new RootNode();
    model     = new LibraryTreeTableModel(rootNode);    
    treeTable = new JXTreeTable(model);
    treeTable.setRootVisible(false);
    treeTable.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
    treeTable.setShowsRootHandles(true);
    treeTable.setDefaultRenderer(Boolean.class, new BooleanRenderer());
    treeTable.setDefaultRenderer(Date.class, new DateRenderer());
    treeTable.setDefaultEditor(URL.class, new URLEditor());

    // Add tree expansion listener for lazy loading child items
    treeTable.addTreeWillExpandListener(new TreeWillExpandListener() {
        @Override
        public void treeWillExpand(TreeExpansionEvent event)
          throws ExpandVetoException {
          final TreePath path              = event.getPath();
          final Object   lastPathComponent = path.getLastPathComponent();

          if (lastPathComponent instanceof ProjectNode) {
            final ProjectNode pNode = (ProjectNode) lastPathComponent;
            if (!pNode.isLoaded() && !pNode.isLoading()) {
              loadProjectDetails(pNode);
            }
          }
        }

        @Override
        public void treeWillCollapse(TreeExpansionEvent event) {
          // No action needed on collapse
        }
      });

    final TableColumnModel colModel = treeTable.getColumnModel();
    // System.out.println(colModel.getColumn(CHECK_COLUMN).getWidth() + " " +
    //                    colModel.getColumn(NAME_COLUMN).getWidth() + " " +
    //                    colModel.getColumn(TYPE_COLUMN).getWidth() + " " +
    //                    colModel.getColumn(SIZE_COLUMN).getWidth() + " " +
    //                    colModel.getColumn(URL_COLUMN).getWidth());
    //
    // We set up a listener on the column model so that we can react
    // when the columns are changed.  That happens when the table is
    // resorted or loaded - i.e., any time the root node is changed.
    //
    // In this listener we change the column widths to preset values. 
    colModel.addColumnModelListener(new TableColumnModelListener() {
      private final int[]     preferred = { 25, 400, 75, 100, 100, 500}; 
      private final int[]     min       = { 25, 100, 75, 100, 75,  100};
      @Override
      public void columnAdded(TableColumnModelEvent e) {
        final int         i = e.getToIndex();
        final TableColumn c = colModel.getColumn(i);
        c.setPreferredWidth(preferred[i]);
        c.setWidth(preferred[i]);
        c.setMinWidth(min[i]);
        if (i == CHECK_COLUMN) c.setMaxWidth(min[i]);
      }
      @Override
      public void columnMarginChanged(ChangeEvent e) {
      }
      @Override
      public void columnMoved(TableColumnModelEvent e) {
      }
      @Override
      public void columnRemoved(TableColumnModelEvent e) {
      }
      @Override
      public void columnSelectionChanged(ListSelectionEvent e) {
      }
    });

    final JScrollPane scroll = new JScrollPane(treeTable);
    scroll.setPreferredSize(new Dimension(1200, 600));
    add(scroll, BorderLayout.CENTER);
    

    // --- Setup Bottom Control & Progress Bar Panel ---
    final JPanel bottomPanel = new JPanel(new BorderLayout(5, 5));
    bottomPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

    downloadButton = new JButton(Resources.getString("LibraryBrowser.download_selected")); //NON-NLS
    downloadButton.addActionListener(e -> downloadFiles());

    final JPanel buttonPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
    buttonPanel.add(downloadButton);

    progressBar = new JProgressBar();
    progressBar.setStringPainted(true);
    progressBar.setVisible(false);

    bottomPanel.add(progressBar, BorderLayout.NORTH);
    bottomPanel.add(buttonPanel, BorderLayout.SOUTH);
    add(bottomPanel, BorderLayout.SOUTH);
  }

  /**
   * Create the menus
   */
  protected void setupMenu(AbstractAction shutDownAction) {
    final MenuManager mm = MenuManager.getInstance();
    final MenuBarProxy mb = mm.getMenuBarProxyFor(this);

    // --- File menu, but not on Mac ---
    if (!SystemUtils.IS_OS_MAC) {
      final MenuProxy fileMenu =
        new MenuProxy(Resources.getString("General.file"));
      fileMenu.setMnemonic(Resources.getString("General.file.shortcut")
                           .charAt(0));

      fileMenu.add(mm.addKey("Prefs.edit_preferences"));
      fileMenu.addSeparator();
      fileMenu.add(mm.addKey("General.quit"));

      mb.add(fileMenu);
    }

    // --- View menu --- 
    final MenuProxy viewMenu =
      new MenuProxy(Resources.getString("General.view"));
    viewMenu.setMnemonic(Resources.getString("General.view.shortcut")
                         .charAt(0));

    final MenuProxy sortMenu =
      new MenuProxy(Resources.getString("LibraryBrowser.sort_by"));
    viewMenu.add(sortMenu);
      

    viewMenu.add(new CheckBoxMenuItemProxy(new AbstractAction(Resources.getString("LibraryBrowser.allow_update")) {
        private static final long serialVersionUID = 1L;
        
        @Override
        public void actionPerformed(ActionEvent e) {
          final JCheckBoxMenuItem item = (JCheckBoxMenuItem) e.getSource();
          AbstractNode.setAllowUpdate(item.isSelected());
        }
      }, AbstractNode.getAllowUpdate()));

    // --- Mutually exclusive group ---
    final ButtonGroup group = new ButtonGroup();
    final Map<Pair<String, Boolean>,
      GameLibrary.AbstractProjectsComparator> sorts =
      new LinkedHashMap<>();
    /* sorts.put(Pair.of("slug", true), comparator);     //NON-NLS
       sorts.put(Pair.of("slug", false),                //NON-NLS
                 GameLibrary.getSlugComparator(false)); */
    sorts.put(Pair.of("title", true),                //NON-NLS
              comparator /*GameLibrary.getTitleComparator(true) */); 
    sorts.put(Pair.of("title", false),               //NON-NLS
              GameLibrary.getTitleComparator(false));
    sorts.put(Pair.of("last_update", true),          //NON-NLS
              GameLibrary.getDateComparator(true));  
    sorts.put(Pair.of("last_update", false),         //NON-NLS
              GameLibrary.getDateComparator(false)); 

    // --- Add sort entrie ---
    for (final Entry<Pair<String, Boolean>,
           GameLibrary.AbstractProjectsComparator> sort
           : sorts.entrySet()) {
      final String title = Resources.getString("LibraryBrowser." + //NON-NLS
                                               sort.getKey().first) +
        " (" + //NON-NLS
        Resources.getString("LibraryBrowser." + //NON-NLS
                            (sort.getKey().second ?
                             "ascending" : //NON-NLS
                             "descending")) + ")"; //NON-NLS

      sortMenu.add(new RadioButtonMenuItemProxy(new AbstractAction(title) {
          private static final long serialVersionUID = 1L;

          @Override
          public void actionPerformed(ActionEvent e) {
            comparator = sort.getValue();
            updateProjects();
          }
        }, sort.getValue() == comparator && sort.getKey().second, group));
    }
    
    mb.add(viewMenu);

    // --- Help menu ----
    final MenuProxy helpMenu =
      new MenuProxy(Resources.getString("General.help"));

    helpMenu.setMnemonic(Resources.getString("General.help.shortcut")
                         .charAt(0));

    helpMenu.add(mm.addMarker("Documentation.VASSAL.start"));  //NON-NLS
    helpMenu.add(mm.addKey("Help.user_guide"));
    helpMenu.addSeparator();
    helpMenu.add(mm.addMarker("Documentation.VASSAL.end"));  //NON-NLS

    if (!SystemUtils.IS_OS_MAC) {
      helpMenu.add(mm.addKey("AboutScreen.about_vassal"));
    }
    
    mm.addAction("Prefs.edit_preferences",
      Prefs.getGlobalPrefs().getEditor().getEditAction());
    mm.addAction("General.quit", shutDownAction);
    
    try {
      final URL url = new File(Documentation.getDocumentationBaseDir(),
                               "userguide/userguide.pdf").toURI().toURL();
      mm.addAction("Help.user_guide",
                   new ShowHelpAction("Help.user_guide", url, null));
    }
    catch (MalformedURLException e) {
      ErrorDialog.bug(e);
    }

    mm.addAction("AboutScreen.about_vassal", new AboutVASSALAction(this));
    mb.add(helpMenu);
    
    
    setJMenuBar(mm.getMenuBarFor(this));    
  }

  /**
   * Read in modules already known to the module manager, so we can
   * flag them as already downloaded.
   */
  protected void updateKnownModules() {
    Stream.concat(
      Arrays.stream(recentModules.getStringArray()),
      Arrays.stream(registeredModules.getStringArray())
    ).sorted().distinct().forEach(s -> {
      // Remove any save directory or the like
      final int  semi = s.indexOf(';');
      if (semi > 0)
        s = s.substring(0, semi);
      
      final Path p = Paths.get(s);
      moduleList.add(p.getFileName().toString());
    });
  }
  
  /**
   * Update the display of the projects
   */
  protected void updateProjects() {
    // Sort by current comparator
    if (projectsMap == null) {
      return;
    }
    
    projectsMap = GameLibrary.sortProjects(projectsMap, comparator);
          
    final RootNode rootNode = (RootNode) model.getRoot();

    // Remove all child nodes
    rootNode.getChildren().clear();

    // Fill in
    for (final Map.Entry<String, Project> entry : projectsMap.entrySet()) {
      rootNode.addProject(entry.getKey(), entry.getValue());
    }

    // Signal that we have a new Root node
    model.modelChanged();
  }
  
  /**
   * Downloads initial Project list with progress updates.
   */
  protected void loadProjects() {
    progressBar.setIndeterminate(true);
    progressBar.setString(Resources.getString("LibraryBrowser.fetching_projects")); //NON-NLS
    progressBar.setVisible(true);

    // Create a swing worker (thread) to do the update. 
    final SwingWorker<Map<String, Project>, Void> worker =
      new SwingWorker<>() {
        /**
         * Run request in the background
         */
        @Override
        protected Map<String, Project> doInBackground() throws Exception {
          return library.getProjects(); // -1 for all
        }
        
        /**
         * When the request is done. Handles delayed exceptions.
         */
        @Override
        protected void done() {
          try {
            projectsMap = get();
            updateProjects();
          }
          catch (Exception e) {
            ErrorDialog.show(e,
                             "LibraryBrowser.failed_fetch_projects"); //NON-NLS
          }
          finally {
            progressBar.setVisible(false);
          }
        }
      };
    worker.execute();
  }

  /**
   * Lazy-loads project details (Packages, Releases, Files) when expanded.
   */
  private void loadProjectDetails(ProjectNode node) {
    node.setLoading(true);
    progressBar.setIndeterminate(true);
    progressBar.setString(Resources.getString("LibraryBrowser.loading_project", //NON-NLS
                                              node.getProject().getTitle()));
    // Delay the start-up of the progress bar by 400 ms. 
    final Timer timer = new Timer(DETAIL_PROGRESS_DELAY, e -> {
      if (node.isLoading())
        progressBar.setVisible(true);
    });
    timer.setRepeats(false);
    timer.start();

    // Create a swing worker (thread) to do the request
    final SwingWorker<Void, Void> worker = new SwingWorker<>() {
      @Override
      protected Void doInBackground() throws Exception {
        library.getProject(node.getSlug(), node.getProject());
        return null;
      }
        
      @Override
      protected void done() {
        try {
          get();
          node.setLoaded(true);
          // model.modelChanged();
          model.nodeStructureChanged(node);
        }
        catch (Exception e) {
          System.out.println("In swing worker: " + e);
          e.printStackTrace(System.out);
          System.out.println(Resources.getString("LibraryBrowser.failed_load_project_message", node.getProject().getTitle()));
          ErrorDialog.show(e, "LibraryBrowser.failed_load_project",
                           node.getProject().getTitle()); //NON-NLS
        }
        finally {
          node.setLoading(false);
          progressBar.setVisible(false);
        }
      }
    };
    try {
      worker.execute();
    }
    catch (Exception e) {
      System.out.println("Wrap around getting project details: " + e);
      e.printStackTrace(System.out);
    }
  }

  /**
   * Downloads the files in the selected LibraryFile items, as chosen
   * by the user.
   */
  private void downloadFiles() {
    final RootNode root = (RootNode) model.getRoot();
    final List<LibraryFile> selectedFiles =
      root.getSelectedFiles();

    if (selectedFiles.isEmpty()) {
      ErrorDialog.show("LibraryBrowser.at_least_one_file"); //NON-NLS
      return;
    }

    try {
      // Let user choose directory
      final FileChooser chooser =
        FileChooser.createFileChooser(this, null,
                                      FileChooser.DIRECTORIES_ONLY);
      if (chooser.showOpenDialog() != FileChooser.APPROVE_OPTION) {
        return;
      }

      final File targetDirectory = chooser.getSelectedFile();
    
      // Lock UI controls while downloading
      downloadButton.setEnabled(false);
      progressBar.setIndeterminate(false);
      progressBar.setMinimum(0);
      progressBar.setMaximum(selectedFiles.size());
      progressBar.setValue(0);
      progressBar.setString(Resources.getString("LibraryBrowser.start_downloads")); //NON-NLS
      progressBar.setVisible(true);

      final SwingWorker<Void, String> worker = new SwingWorker<>() {
          @Override
          protected Void doInBackground() throws Exception {
            int count = 0;
            long total = 0;
            for (final LibraryFile file : selectedFiles) 
              total += file.getSize();

            long current = 0;
            for (final LibraryFile file : selectedFiles) {
              current += file.getSize();
              publish(Resources.getString("LibraryBrowser.download", //NON-NLS
                                          file.getFileName(),
                                          count + 1, selectedFiles.size(),
                                          formatSize(current), formatSize(total)));
                  
              // Assumed downloading method
              final File target = library.downloadFile(file, targetDirectory);

              if (file.isModule()) {
                moduleList.add(target.getName());
                registeredModules.setValue(ArrayUtils.add((String[])registeredModules.getValue(),
                                                          target.toString()));
              }
            
              count++;
              setProgress(count);
            }
            return null;
          }

          @Override
          protected void process(List<String> chunks) {
            // Update label text with current file being downloaded
            progressBar.setString(chunks.get(chunks.size() - 1));
            progressBar.setValue(getProgress());
          }

          @Override
          protected void done() {
            try {
              get();
              JOptionPane.showMessageDialog(GameLibraryWindow.this,
                                            Resources.getString("LibraryBrowser.downloaded", //NON-NLS
                                                                selectedFiles.size()),
                                            Resources.getString("LibraryBrowser.download_complete"),
                                            JOptionPane.INFORMATION_MESSAGE);
            }
            catch (Exception e) {
              ErrorDialog.show(e, "LibraryBrowser.error_downloading"); //NON-NLS
            }
            finally {
              downloadButton.setEnabled(true);
              progressBar.setVisible(false);
            }
          }
        };

      // Standard SwingWorker progress listener
      worker.addPropertyChangeListener(evt -> {
        if ("progress".equals(evt.getPropertyName())) { //NON-NLS
          progressBar.setValue((Integer) evt.getNewValue());
        }
      });
      
      worker.execute();
    }
    catch (Exception e) {
      System.out.println("While downloading files: " + e);
      e.printStackTrace(System.out);
    }
  }

  /**
   * Common interface assumed for all nodes
   */
  interface TreeNode {
    TreeNode getParent();
    Object   getValueAt(int column);
    String   getType();
    void     setValueAt(Object value, int column);
    List<?>  getChildren();
    boolean  isLeaf();
    Boolean  isSelected();
    long     getSize();
    Date     getDate();
    void     setSelected(boolean selected);
    void     collectSelectedFiles(List<LibraryFile> list);
    String   getToolTip(int column);
    boolean  canEdit(int column);
  }

  /**
   * Implement most things for common node interface
   */
  public abstract static class AbstractNode implements TreeNode {
    protected Boolean selected = null;
    protected TreeNode parent = null;
    protected static boolean allowUpdate = false;
    /**
     * Whether we allow updating known modules
     */
    public static void setAllowUpdate(boolean allow) {
      allowUpdate = allow;
    }

    /**
     * Whether we allow updating known modules
     */
    public static boolean getAllowUpdate() {
      return allowUpdate;
    }

    /**
     * Construct from a parent
     */
    public AbstractNode(TreeNode p) {
      parent = p;
    }

    /**
     * Get the parent node - used to find path to a node
     */
    @Override
    public TreeNode getParent() {
      return parent;
    }

    /**
     * Has the user selected this node?
     */
    @Override
    public Boolean isSelected() {
      return selected;
    }

    /**
     * Get size - 0 by default
     */
    @Override
    public long getSize() {
      return 0;
    }
    /**
     * Mark this node
     */
    @Override
    public void setSelected(boolean selected) {
      this.selected = selected;
      // Cascade selection down to children
      for (final Object child : getChildren()) {
        if (child instanceof TreeNode) {
          ((TreeNode) child).setSelected(selected);
        }
      }
    }

    /**
     * Set the value of a column - selection only
     */
    @Override
    public void setValueAt(Object value, int column) {
      if (column == CHECK_COLUMN && value instanceof Boolean) {
        setSelected((Boolean) value);
      }
    }

    /**
     * Base method for getting values.  Derived classes override and
     * augment this by ultimately calling this method.
     */
    @Override
    public Object getValueAt(int column) {
      switch (column) {
      case CHECK_COLUMN: return isSelected();
      case SIZE_COLUMN:  return formatSize(getSize());
      case TYPE_COLUMN:  return getType();
      case DATE_COLUMN:  return getDate(); 
      case URL_COLUMN:   return null;
      default: return ""; //NON-NLS
      }
    }

    /**
     * Add selected files by recursing the hierarchy until we hit a
     * leaf
     */
    @Override
    public void collectSelectedFiles(List<LibraryFile> list) {
      for (final Object child : getChildren()) {
        if (child instanceof TreeNode) {
          ((TreeNode) child).collectSelectedFiles(list);
        }
      }
    }

    /**
     * Latest of all children
     */
    @Override
    public Date getDate() {
      Date ret = new Date(0);
      for (final Object child : getChildren()) {
        if (child instanceof TreeNode) {
          final Date d = ((TreeNode) child).getDate();
          if (d.after(ret))
            ret = d;
        }
      }
      return ret;
    }

    @Override
    public String getToolTip(int column) {
      return null;
    }

    @Override
    public boolean canEdit(int column) {
      return false;
    }
    
  }

  /**
   * The root of the hierarchy
   */
  public static class RootNode extends AbstractNode {
    private final List<ProjectNode> children = new ArrayList<>();

    public RootNode() {
      super(null);
    }
    
    public void addProject(String slug, Project project) {
      children.add(new ProjectNode(this, slug, project));
    }

    public List<LibraryFile> getSelectedFiles() {
      final List<LibraryFile> files = new ArrayList<>();
      collectSelectedFiles(files);
      return files;
    }

    @Override
    public String getType() {
      return "Root"; //NON-NLS
    }
    @Override
    public Object getValueAt(int column) {
      return "Root"; //NON-NLS
    }
    
    @Override
    public List<?> getChildren() {
      return children;
    }
    
    @Override
    public boolean isLeaf() {
      return false;
    }
  }

  /**
   * A node corresponding to a project.  Note, we do lazy loading of
   * the details of a project.
   */
  public static class ProjectNode extends AbstractNode {
    private final String slug;
    private final Project project;
    private final List<Object> dummyChildren = Collections.singletonList(Resources.getString("LibraryBrowser.dummy_children")); //NON-NLS
    private final List<PackageNode> children = new ArrayList<>();
    private boolean loaded = false;
    private boolean loading = false;

    public ProjectNode(TreeNode parent, String s, Project p) {
      super(parent);
      slug    = s;
      project = p;
    }

    public String  getSlug() {
      return slug;
    }
    public Project getProject() {
      return project;
    }
    public boolean isLoaded() {
      return loaded;
    }
    public void    setLoaded(boolean done) {
      loaded = done;
    }
    public boolean isLoading() {
      return loading;
    }
    public void    setLoading(boolean going) {
      loading = going;
    }

    @Override
    public String getType() {
      return Resources.getString("LibraryBrowser.project"); //NON-NLS
    }

    @Override
    public Object getValueAt(int column) {
      switch (column) {
      case NAME_COLUMN:
        return project.getTitle();
      case URL_COLUMN:
        try {
          return  new URL(LIBRARY_URL + getSlug()); 
        }
        catch (MalformedURLException ignored) {
          return null;
        }
      default:
        return super.getValueAt(column);
      }
    }

    @Override
    public List<?> getChildren() {
      if (!loaded) 
        return dummyChildren;

      if (!children.isEmpty())
        return children;
      
      for (final Package p : project.getPackages()) 
        children.add(new PackageNode(this, p));
      
      return children;
    }

    @Override
    public Date getDate() {
      return project.getDate();
    }
    
    @Override
    public boolean isLeaf() {
      return false;
    }
    
    @Override
    public boolean canEdit(int column) {
      return column == URL_COLUMN;
    }
  }

  /**
   * A node corresponding to a package
   */
  public static class PackageNode extends AbstractNode {
    private final Package pkg;
    private final List<ReleaseNode> children = new ArrayList<>();

    public PackageNode(TreeNode parent, Package p) {
      super(parent);
      pkg = p;
    }

    @Override
    public Object getValueAt(int column) {
      switch (column) {
      case NAME_COLUMN: return pkg.getName();
      default: return super.getValueAt(column);
      }
    }

    @Override
    public List<?> getChildren() {
      if (!children.isEmpty())
        return children;
      
      for (final Release r : pkg.getReleases()) {
        children.add(new ReleaseNode(this, r));
      }
      return children;
    }
        
    @Override
    public String getType() {
      return Resources.getString("LibraryBrowser.package"); //NON-NLS
    }

    @Override
    public boolean isLeaf() {
      return false;
    }
  }

  /**
   * A node corresponding to a release
   */
  public static class ReleaseNode extends AbstractNode {
    private final Release release;
    private final List<FileNode> children = new ArrayList<>();

    public ReleaseNode(TreeNode parent, Release rel) {
      super(parent);
      this.release = rel;
      selected = Boolean.FALSE;
    }

    @Override
    public Object getValueAt(int column) {
      switch (column) {
      case NAME_COLUMN: return release.getNumber();
      default: return super.getValueAt(column);
      }
    }

    @Override
    public List<?> getChildren() {
      if (!children.isEmpty())
        return children;
      
      for (final LibraryFile f : release.getFiles()) {
        children.add(new FileNode(this, f));
      }
      return children;
    }

    /**
     * Sum sizes of children
     */
    @Override
    public long getSize() {
      long ret = -1;
      for (final Object child : getChildren()) {
        if (child instanceof TreeNode) {
          ret += ((TreeNode) child).getSize();
        }
      }
      return ret;
    }
    
    @Override
    public String getType() {
      return Resources.getString("LibraryBrowser.release"); //NON-NLS
    }
    
    @Override
    public boolean isLeaf() {
      return false;
    }

    @Override
    public boolean canEdit(int column) {
      for (final Object child : getChildren()) {
        if (child instanceof TreeNode) {
          if (((TreeNode)child).canEdit(column))
            return true;
        }
      }
      return false;
    }
  }

  /**
   * A node (leaf) corresponding to a file
   */
  public static class FileNode extends AbstractNode {
    private final LibraryFile file;
    
    public FileNode(TreeNode parent, LibraryFile f) {
      super(parent);
      file     = f;
      selected = Boolean.FALSE;
    }

    public LibraryFile getFile() {
      return file;
    }

    @Override
    public Object getValueAt(int column) {
      switch (column) {
      case CHECK_COLUMN: return !canDownload() || isSelected(); 
      case NAME_COLUMN: return file.getFileName();
        // case URL_COLUMN:  return file.getURL();
      default: return super.getValueAt(column);
      }
    }

    @Override
    public List<?> getChildren() {
      return Collections.emptyList();
    }
    
    @Override
    public boolean isLeaf() {
      return true;
    }

    @Override
    public void collectSelectedFiles(List<LibraryFile> list) {
      if (Boolean.TRUE.equals(selected) && canDownload()) {
        list.add(file);
      }
    }
    /**
     * Sum sizes of children
     */
    @Override
    public long getSize() {
      return file.getSize();
    }

    /**
     * Get date of file
     */
    @Override
    public Date getDate() {
      return file.getDate();
    }

    /**
     * Get the type of the file
     */
    @Override
    public String getType() {
      if (file.isModule())
        return Resources.getString("LibraryBrowser.module"); //NON-NLS
      if (file.isSave())
        return Resources.getString("LibraryBrowser.save"); //NON-NLS
      if (file.isExtension())
        return Resources.getString("LibraryBrowser.extension"); //NON-NLS
      if (file.isLog())
        return Resources.getString("LibraryBrowser.log"); //NON-NLS
      return Resources.getString("LibraryBrowser.aux"); //NON-NLS
    }

    /**
     * Get the tool-tip - not currently used
     */
    @Override
    public String getToolTip(int column) {
      if (column == CHECK_COLUMN) {
        if (canDownload()) {
          return Resources.getString("LibraryBrowser.select_file"); //NON-NLS
        }
        else {
          return Resources.getString("LibraryBrwoser.has_file"); //NON-NLS
        }
      }
      return null;
    }

    /**
     * Whether we can "edit" this entry in the specified column
     */
    @Override
    public boolean canEdit(int column) {
      return column == CHECK_COLUMN && canDownload();
    }
    
    public boolean canDownload() {
      return getAllowUpdate() || !hasAlready();
    }
    
    public boolean hasAlready() {
      return moduleList.contains(file.getFileName());
    }
  }

  /**
   * Model used by the tree table.  Note, the column names are defined
   * at the top.
   */
  public static class LibraryTreeTableModel extends AbstractTreeTableModel {
    
    public LibraryTreeTableModel(RootNode root) {
      super(root);
    }

    
    /**
     * Second column show the hierarchy
     */
    @Override
    public int getHierarchicalColumn() {
      return NAME_COLUMN;
    }

    /**
     * Number of columns
     */
    @Override
    public int getColumnCount() {
      return COLUMN_NAMES.length;
    }

    /**
     * Name of a column
     */
    @Override
    public String getColumnName(int column) {
      return COLUMN_NAMES[column];
    }

    /**
     * Class of data of a column
     */
    @Override
    public Class<?> getColumnClass(int column) {
      switch (column) {
      case CHECK_COLUMN:
        return Boolean.class;
      case DATE_COLUMN:
        return Date.class;
      case URL_COLUMN:
        return URL.class;
      default:
        return String.class;
      }      
    }    
    
    /**
     * See if we can edit a cell
     */
    @Override
    public boolean isCellEditable(Object node, int column) {
      if (node instanceof TreeNode) {
        final TreeNode wrap = (TreeNode)node;
        // Check if the required VASSAL version is too new
        // if (column == CHECK_COLUMN && wrap instanceof FileNode) {
        //   FileNode fn = (FileNode)wrap;
        //   if (fn.getFile().getRequired().compareTo(vassalVersion) > 0)
        //     return false;
        // }
        return wrap.canEdit(column);
      }
      return false;
    }

    /**
     * Get value of a cell
     */
    @Override
    public Object getValueAt(Object node, int column) {
      if (node instanceof TreeNode) {
        return ((TreeNode) node).getValueAt(column);
      }
      return node.toString();
    }

    /**
     * Set the value of a cell after editing
     */
    @Override
    public void setValueAt(Object value, Object node, int column) {
      if (node instanceof TreeNode) {
        final TreeNode wrap = ((TreeNode) node);
        wrap.setValueAt(value, column);
        nodeStructureChanged(wrap);
      }
    }
    
    /**
     * Get the ith child of a node
     */
    @Override
    public Object getChild(Object parent, int index) {
      if (index < 0 || index >= getChildCount(parent))
        return null;
      if (parent instanceof TreeNode) {
        return ((TreeNode) parent).getChildren().get(index);
      }
      return null;
    }

    /**
     * Get number of children of a node
     */
    @Override
    public int getChildCount(Object parent) {
      if (parent instanceof TreeNode) {
        return ((TreeNode) parent).getChildren().size();
      }
      return 0;
    }

    /**
     * Check if the node is a leaf
     */
    @Override
    public boolean isLeaf(Object node) {
      if (node instanceof TreeNode) {
        return ((TreeNode) node).isLeaf();
      }
      return true;
    }

    /**
     * Get the index of a child node
     */
    @Override
    public int getIndexOfChild(Object parent, Object child) {
      if (parent instanceof TreeNode) {
        return ((TreeNode) parent).getChildren().indexOf(child);
      }
      return -1;
    }
    
    /**
     * Builds an array of nodes from the root down to the target node.
     * Required to build a valid TreePath for model notifications.
     */
    protected Object[] getPathToRoot(TreeNode node) {
      final List<TreeNode> path = new ArrayList<>();
      TreeNode current = node;

      while (current != null) {
        path.add(0, current); // Prepend so root ends up at index 0
        current = current.getParent();
      }

      return path.toArray();
    }

    /**
     * When a new root is made (happens once!) after loading 
     */
    public void modelChanged() {
      final Object[] path = getPathToRoot((TreeNode)getRoot());
      modelSupport.firePathChanged(new TreePath(path));      
      modelSupport.fireTreeStructureChanged(new TreePath(path));
      // modelSupport.fireNewRoot();
    }

    /**
     * Notifies listeners that the structure or state of a node has changed.
     */
    public void nodeStructureChanged(TreeNode node) {
      final Object[] path = getPathToRoot(node);
      if (path.length > 0) {
        modelSupport.firePathChanged(new TreePath(path));
        modelSupport.fireTreeStructureChanged(new TreePath(path));
      }
    }
  }

  /**
   * Test of this
   */
  public static void main(String[] args) {
    SwingUtilities.invokeLater(() -> {
      try {
        UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        final GameLibraryWindow window = new GameLibraryWindow();
        
        window.pack();
        window.setVisible(true);
      }
      catch (Exception e) {
        e.printStackTrace();
      }
    }
    );
  }
}
//
// EOF
//
