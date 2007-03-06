/*
 * $Id$
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.configure;

import java.awt.Component;
import java.io.IOException;
import javax.swing.JDialog;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import VASSAL.build.GameModule;
import VASSAL.build.module.ModuleExtension;
import VASSAL.tools.ScrollPane;

/**
 * Editing window for a module extension
 */
public class ExtensionEditWindow extends VASSAL.configure.ModuleEditWindow {
  private static final long serialVersionUID = 1L;

  private ModuleExtension extension;

  public ExtensionEditWindow(ModuleExtension extension) {
    this.extension = extension;
    initExtensionComponents();
  }

  protected void initComponents(Component view) {
  }

  private void initExtensionComponents() {
    super.initComponents(
      new ScrollPane(
         new VASSAL.configure.ExtensionTree(GameModule.getGameModule(),
                                            helpWindow, extension)));
    toolbar.addSeparator();
    toolbar.add(extension.getEditAction(new JDialog(this)));
  }

  protected void refreshTitle() {
    if (extension != null) {
      setTitle("Edit "+extension.getName());
    }
    else {
      setTitle("Edit Extension");
    }
  }

  protected JMenu createUpdateMenu() {
    JMenu m = super.createUpdateMenu();
    ((JMenuItem)m.getMenuComponent(0)).setText("Create extension updater");
    m.remove(1);
    return m;
  }

  protected void save() {
    ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(),report);
    if (report.getWarnings().size() == 0) {
      try {
        extension.save();
      }
      catch (IOException e) {
        JOptionPane.showMessageDialog(ExtensionEditWindow.this,e.getMessage(),"Save Failed",JOptionPane.ERROR_MESSAGE);
      }
    }
    else {
      new ValidationReportDialog(report, new ValidationReportDialog.CallBack() {
        public void ok() {
          try {
            extension.save();
            refreshTitle();
          }
          catch (IOException e) {
            JOptionPane.showMessageDialog(ExtensionEditWindow.this,e.getMessage(),"Save Failed",JOptionPane.ERROR_MESSAGE);
          }
        }
        public void cancel() {
        }
      }).setVisible(true);
    }
  }

  protected void saveAs() {
    ValidationReport report = new ValidationReport();
    GameModule.getGameModule().validate(GameModule.getGameModule(),report);
    if (report.getWarnings().size() == 0) {
      try {
        extension.save();
      }
      catch (IOException e) {
        JOptionPane.showMessageDialog(ExtensionEditWindow.this,e.getMessage(),"Save Failed",JOptionPane.ERROR_MESSAGE);
      }
    }
    else {
      new ValidationReportDialog(report, new ValidationReportDialog.CallBack() {
        public void ok() {
          try {
            extension.saveAs();
            refreshTitle();
          }
          catch (IOException e) {
            JOptionPane.showMessageDialog(ExtensionEditWindow.this,e.getMessage(),"Save Failed",JOptionPane.ERROR_MESSAGE);
          }
        }
        public void cancel() {
        }
      }).setVisible(true);
    }
  }
}
