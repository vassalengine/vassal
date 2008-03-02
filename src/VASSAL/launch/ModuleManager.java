/*
 *
 * Copyright (c) 2000-2007 by Rodney Kinney
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
package VASSAL.launch;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.swing.AbstractAction;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.SwingConstants;
import javax.swing.border.TitledBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.ExtensionsManager;
import VASSAL.chat.CgiServerStatus;
import VASSAL.chat.ui.ServerStatusView;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.configure.ShowHelpAction;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.TranslateVassalAction;
import VASSAL.i18n.Resources;
import VASSAL.preferences.Prefs;
import VASSAL.tools.DataArchive;
import VASSAL.tools.FileChooser;
import VASSAL.tools.OrderedMenu;
import VASSAL.tools.imports.ImportAction;

/**
 * Tracks recently-used modules and builds the main GUI window for interacting with modules
 * 
 * @author rodneykinney
 * 
 */
public class ModuleManager {
  private static ModuleManager instance;
  private JFrame theFrame;
  private DefaultListModel modules = new DefaultListModel();
  private StringArrayConfigurer recentModuleConfig;
  private ExtensionControls extensionsControls;
  private File selectedModule;
  private CardLayout modulePanelLayout;
  private JPanel moduleView;

  public static ModuleManager getInstance() {
    if (instance == null) {
      instance = new ModuleManager();
    }
    return instance;
  }

  public void addModule(File f) {
    if (!modules.contains(f)) {
      int i = 0;
      while (i < modules.size() && ((File) modules.get(i)).getName().compareTo(f.getName()) < 0) {
        i++;
      }
      modules.add(i, f);
      List<String> l = new ArrayList<String>();
      for (int k = 0, n = modules.size(); k < n; ++k) {
        l.add(((File) modules.get(k)).getPath());
      }
      recentModuleConfig.setValue(l.toArray(new String[l.size()]));
      modulePanelLayout.show(moduleView, modules.size() == 0 ? "quickStart" : "modules");
    }
  }

  public void removeModule(File f) {
    if (modules.removeElement(f)) {
      List<String> l = new ArrayList<String>();
      for (int k = 0, n = modules.size(); k < n; ++k) {
        l.add(((File) modules.get(k)).getPath());
      }
      recentModuleConfig.setValue(l.toArray(new String[l.size()]));
      modulePanelLayout.show(moduleView, modules.size() == 0 ? "quickStart" : "modules");
    }
  }

  public void showFrame() {
    if (theFrame == null) {
      theFrame = new JFrame("VASSAL");
      theFrame.setLayout(new BoxLayout(theFrame.getContentPane(), BoxLayout.X_AXIS));
      JMenuBar menuBar = new JMenuBar();
      theFrame.setJMenuBar(menuBar);
      menuBar.add(buildFileMenu());
      menuBar.add(buildToolsMenu());
      menuBar.add(buildHelpMenu());
      JPanel p = new JPanel(new BorderLayout());
      theFrame.add(p);
      p.add(buildModuleControls(), BorderLayout.CENTER);
      extensionsControls = buildExtensionsControls();
      p.add(extensionsControls, BorderLayout.SOUTH);
      JComponent serverStatusControls = buildServerStatusControls();
      theFrame.add(serverStatusControls, BorderLayout.EAST);
      Rectangle r = Info.getScreenBounds(theFrame);
      extensionsControls.setPreferredSize(new Dimension(0, r.height / 4));
      serverStatusControls.setPreferredSize(new Dimension((int) (r.width / 3.5), 0));
      theFrame.setSize(3 * r.width / 4, 3 * r.height / 4);
      theFrame.setLocation(r.width / 8, r.height / 8);
      theFrame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
      theFrame.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent e) {
          new ShutDownAction().actionPerformed(null);
        }
      });
    }
    theFrame.setVisible(true);
  }

  protected JComponent buildModuleControls() {
    JPanel moduleControls = new JPanel(new BorderLayout());
    modulePanelLayout = new CardLayout();
    moduleView = new JPanel(modulePanelLayout);
    moduleView.add(new JScrollPane(buildModuleList()),"modules");
    JLabel l = new JLabel(Resources.getString("ModuleManager.quickstart"));
    l.setHorizontalAlignment(SwingConstants.CENTER);
    moduleView.add(l,"quickStart");
    modulePanelLayout.show(moduleView, modules.size() == 0 ? "quickStart" : "modules");
    moduleControls.add(moduleView, BorderLayout.CENTER);
    moduleControls.setBorder(new TitledBorder(Resources.getString("ModuleManager.recent_modules")));
    return moduleControls;
  }

  public JFrame getFrame() {
    return theFrame;
  }
  
  public void hideFrame() {
    if (theFrame != null) {
      theFrame.setVisible(false);
    }
  }

  protected ExtensionControls buildExtensionsControls() {
    return new ExtensionControls();
  }

  protected JComponent buildServerStatusControls() {
    ServerStatusView view = new ServerStatusView(new CgiServerStatus());
    view.setBorder(new TitledBorder(Resources.getString("Chat.server_status")));
    return view;
  }

  protected JMenu buildFileMenu() {
    JMenu menu = OrderedMenu.builder("General.file").create();
    menu.add(new LoadModuleAction(menu));
    menu.add(new EditModuleAction(menu));
    menu.add(new CreateModuleAction(menu));
    menu.add(new ImportAction(menu));
    menu.addSeparator();
    menu.add(new ShutDownAction());
    return menu;
  }

  protected JMenu buildHelpMenu() {
    JMenu menu = OrderedMenu.builder("General.help").create();
    menu.add(AboutVASSAL.getAction());
    final File readme = new File(Documentation.getDocumentationBaseDir(), "README.html");
    try {
      menu.add(new ShowHelpAction(readme.toURI().toURL(), null));
    }
    catch (MalformedURLException e) {
      e.printStackTrace();
    }
    return menu;
  }

  protected JMenu buildToolsMenu() {
    JMenu menu = OrderedMenu.builder("General.tools").create();
    menu.add(new TranslateVassalAction(theFrame));
    return menu;
  }

  protected JList buildModuleList() {
    recentModuleConfig = new StringArrayConfigurer("RecentModules", null);
    Prefs.getGlobalPrefs().addOption(null, recentModuleConfig);
    List<String> missingModules = new ArrayList<String>();
    List<File> moduleList = new ArrayList<File>();
    for (String s : recentModuleConfig.getStringArray()) {
      File f = new File(s);
      if (f.exists()) {
        moduleList.add(f);
      }
      else {
        missingModules.add(s);
      }
    }
    for (String s : missingModules) {
      moduleList.remove(s);
      recentModuleConfig.removeValue(s);
    }
    Collections.sort(moduleList, new Comparator<File>() {
      public int compare(File f1, File f2) {
        return f1.getName().compareTo(f2.getName());
      }
    });
    modules = new DefaultListModel();
    for (File f : moduleList) {
      modules.addElement(f);
    }
    final JList list = new JList(modules);
    list.setCellRenderer(new DefaultListCellRenderer() {
      public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
        Component c = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        setText(((File) value).getName());
        setToolTipText(((File) value).getPath());
        return c;
      }
    });
    list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
    list.addListSelectionListener(new ListSelectionListener() {
      public void valueChanged(ListSelectionEvent e) {
        File moduleFile = (File) ((JList) e.getSource()).getSelectedValue();
        setSelectedModule(moduleFile);
        if (moduleFile != null) {
          extensionsControls.setExtensionsManager(new ExtensionsManager(moduleFile));
        }
        else {
          extensionsControls.clear();
        }
      }
    });
    list.addMouseListener(new MouseAdapter() {
      public void mouseReleased(MouseEvent e) {
        if (e.isMetaDown()) {
          int index = list.locationToIndex(e.getPoint());
          if (index >= 0) {
            buildPopup(index).show(list, e.getX(), e.getY());
          }
        }
        else if (e.getClickCount() == 2) {
          int index = list.locationToIndex(e.getPoint());
          if (index >= 0) {
            final File module = (File) list.getModel().getElementAt(index);
            new LoadModuleAction(module).actionPerformed(null);
          }          
        }
      }

      private JPopupMenu buildPopup(int index) {
        JPopupMenu m = new JPopupMenu();
        final File module = (File) list.getModel().getElementAt(index);
        m.add(new LoadModuleAction(module));
        m.add(new EditModuleAction(module));
        m.add(new AbstractAction(Resources.getString("General.remove")) {
          public void actionPerformed(ActionEvent e) {
            removeModule(module);
          }
        });
        return m;
      }
    });
    return list;
  }
  private class ExtensionControls extends JPanel {
    private ExtensionsManager extMgr;
    private JList extList;
    private AbstractAction addExtensionAction = new AbstractAction(Resources.getString("ModuleManager.add")) {
      public void actionPerformed(ActionEvent e) {
        FileChooser fc = FileChooser.createFileChooser(theFrame, (DirectoryConfigurer) Prefs.getGlobalPrefs().getOption(Prefs.MODULES_DIR_KEY));
        if (fc.showOpenDialog() == FileChooser.APPROVE_OPTION) {
          extMgr.setActive(fc.getSelectedFile(), true);
          refresh();
        }
      }
    };
    private AbstractAction newExtensionAction = new AbstractAction(Resources.getString(Resources.NEW)) {
      public void actionPerformed(ActionEvent e) {
        try {
          GameModule.init(new BasicModule(new DataArchive(getSelectedModule().getPath())));
          GameModule.getGameModule().getFrame().setVisible(true);
          new NewExtensionAction(theFrame).actionPerformed(null);
        }
        catch (IOException e1) {
          e1.printStackTrace();
        }
      }
    };

    private ExtensionControls() {
      super(new BorderLayout());
      setBorder(new TitledBorder(Resources.getString("ModuleManager.extensions")));
      JToolBar tb = new JToolBar();
      tb.setFloatable(false);
      tb.add(newExtensionAction);
      tb.add(addExtensionAction);
      add(tb, BorderLayout.NORTH);
      extList = new JList();
      add(new JScrollPane(extList), BorderLayout.CENTER);
      extList.setCellRenderer(new DefaultListCellRenderer() {
        public Component getListCellRendererComponent(JList list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
          super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
          boolean active = ((Extension) value).isActive();
          setForeground(active ? Color.black : Color.gray);
          return this;
        }
      });
      extList.addMouseListener(new MouseAdapter() {
        public void mouseReleased(MouseEvent e) {
          if (e.isMetaDown() && extMgr != null) {
            buildPopup(extList.locationToIndex(e.getPoint())).show(extList, e.getX(), e.getY());
          }
        }
      });
      setExtensionsManager(null);
    }

    private JPopupMenu buildPopup(int index) {
      JPopupMenu m = new JPopupMenu();
      if (index >= 0) {
        final Extension ext = (Extension) extList.getModel().getElementAt(index);
        m.add(new AbstractAction(Resources.getString(ext.isActive() ? "ModuleManager.deactivate" : "ModuleManager.activate")) {
          public void actionPerformed(ActionEvent e) {
            extMgr.setActive(ext.getFile(), !ext.isActive());
            refresh();
          }
        });
        m.add(new EditExtensionAction(m));
        m.addSeparator();
      }
      m.add(newExtensionAction);
      m.add(addExtensionAction);
      return m;
    }

    public void clear() {
      setExtensionsManager(null);
    }

    public void refresh() {
      setExtensionsManager(extMgr);
    }

    public void setExtensionsManager(ExtensionsManager mgr) {
      extMgr = mgr;
      DefaultListModel m = new DefaultListModel();
      if (extMgr != null) {
        List<Extension> l = new ArrayList<Extension>();
        for (File f : extMgr.getActiveExtensions()) {
          l.add(new Extension(f, true));
        }
        for (File f : extMgr.getInactiveExtensions()) {
          l.add(new Extension(f, false));
        }
        Collections.sort(l);
        for (Extension e : l) {
          m.addElement(e);
        }
      }
      extList.setModel(m);
      newExtensionAction.setEnabled(extMgr != null);
      addExtensionAction.setEnabled(extMgr != null);
    }
    private class Extension implements Comparable<Extension> {
      private File extFile;
      private boolean active;

      public Extension(File extFile, boolean active) {
        super();
        this.extFile = extFile;
        this.active = active;
      }

      public File getFile() {
        return extFile;
      }

      public int compareTo(Extension e) {
        return extFile.compareTo(e.extFile);
      }

      public boolean isActive() {
        return active;
      }

      public String toString() {
        String s = extFile.getName();
        if (!active) {
          s += " (" + Resources.getString("ModuleManager.inactive") + ")";
        }
        return s;
      }
    }
  }

  public File getSelectedModule() {
    return selectedModule;
  }

  private void setSelectedModule(File selectedModule) {
    this.selectedModule = selectedModule;
  }
}