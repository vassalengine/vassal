/*
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman, Brent Easton
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.launch;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JOptionPane;

import VASSAL.build.GameModule;
import VASSAL.build.module.ModuleExtension;
import VASSAL.configure.ExtensionTree;

public class ExtensionEditorWindow extends EditorWindow {

  private static final long serialVersionUID = 1L;
  protected static ExtensionEditorWindow instance = null;  
  protected ModuleExtension extension;
 
  public static EditorWindow getInstance() {
    if (instance == null) instance = new ExtensionEditorWindow();
    return instance;
  }
  
  public ExtensionEditorWindow() {
    super();
    setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    addWindowListener(new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
        close();
      }
    });
  }
  
  /**
   * The Extension is loading
   */
  public void moduleLoading(GameModule mod, ModuleExtension ext) {
    extension = ext;
    tree = new ExtensionTree(mod, helpWindow, ext, this);
    treeStateChanged(false);
    scrollPane.setViewportView(tree);
    
    tree.populateEditMenu(this);
    componentHelpItem.setAction(tree.getHelpAction());
    
    saveAction.setEnabled(true);
    saveAsAction.setEnabled(true);
    close.setEnabled(true);
    createUpdater.setEnabled(true);

    pack();
    
    setVisible(true);
    ModuleEditorWindow.getInstance().setVisible(false);
  }

  public void moduleLoading(GameModule mod) {
    
  }

  public String getEditorType() {
    return "Extension";
  }

  protected void populateFileMenu(JMenu menu) {
    addCloseMenuItem(menu);
    addSaveMenuItem(menu);
    addSaveAsMenuItem(menu);
    menu.addSeparator();
    addQuitMenuItem(menu);
  }
  
  protected void populateToolsMenu(JMenu menu) {
    addUpdaterMenuItem(menu);
    menu.addSeparator();
    addTranslateMenuItem(menu);    
  }

  protected void save() {
    ExtensionEditorWindow.this.saver(new Runnable() {
      public void run() {
        try {
          extension.save();
        }
        catch (IOException e) {
          JOptionPane.showMessageDialog(ExtensionEditorWindow.this,e.getMessage(),"Save Failed",JOptionPane.ERROR_MESSAGE);
        }
      }
    });  
  }

  protected void saveAs() {
    ExtensionEditorWindow.this.saver(new Runnable() {
      public void run() {
        try {
          extension.saveAs();
        }
        catch (IOException e) {
          JOptionPane.showMessageDialog(ExtensionEditorWindow.this,e.getMessage(),"Save Failed",JOptionPane.ERROR_MESSAGE);
        }
      }
    }); 
  }
  
  /**
   * Close Extension and return to the Module Editor
   */
  protected void close() {
    if (extension.confirmExit()) {
      extension = null;
      tree = null;      
      setVisible(false);
      ModuleEditorWindow.getInstance().setVisible(true);
      dispose();
    }
  }
}
