/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney
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
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.HeadlessException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.util.Properties;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.SavedGameUpdater;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.io.IOUtils;

public class SavedGameUpdaterDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private DefaultListModel savedGamesModel;
  private SavedGameUpdater updater = new SavedGameUpdater();
  private Properties oldPieceInfo;
  private JFileChooser fc;
  private static final String VERSION_KEY = "moduleVerion";
  private static final String MODULE_NAME_KEY = "moduleName";
  private JButton updateButton;
  private JTextField versionField;

  public SavedGameUpdaterDialog(Frame owner) throws HeadlessException {
    super(owner, false);
    setTitle("Update Saved Games");
    initComponents();
    fc = new JFileChooser();
    fc.setCurrentDirectory(GameModule.getGameModule().getFileChooser().getCurrentDirectory());
  }

  private void initComponents() {
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    Box versionBox = Box.createHorizontalBox();
    versionBox.add(new JLabel("Module version of saved games:  "));
    versionField = new JTextField(8);
    versionField.setEditable(false);
    versionField.setMaximumSize(new Dimension(versionField.getMaximumSize().width, versionField.getPreferredSize().height));
    versionBox.add(versionField);
    JButton importButton = new JButton("Import GamePiece info");
    importButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        importPieceInfo();
      }
    });
    versionBox.add(importButton);
    add(versionBox);
    JButton exportButton = new JButton("Export GamePiece info");
    exportButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        exportPieceInfo();
      }
    });
    Box importExportBox = Box.createHorizontalBox();
    importExportBox.add(importButton);
    importExportBox.add(exportButton);
    add(importExportBox);

    Box savedGamesBox = Box.createHorizontalBox();
    Box left = Box.createVerticalBox();
    left.add(new JLabel("Saved Games:"));
    JButton chooseGamesButton = new JButton("Choose");
    chooseGamesButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        chooseSavedGames();
      }
    });
    left.add(chooseGamesButton);
    savedGamesBox.add(left);
    savedGamesModel = new DefaultListModel();
    JList savedGamesList = new JList(savedGamesModel);
    savedGamesList.setVisibleRowCount(5);
    savedGamesList.setCellRenderer(new DefaultListCellRenderer() {
      private static final long serialVersionUID = 1L;

      public Component getListCellRendererComponent(
          JList list,
          Object value,
          int index,
          boolean isSelected,
          boolean cellHasFocus) {
        super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        setText(((File) value).getName());
        return this;
      }
    });
    savedGamesBox.add(new ScrollPane(savedGamesList));
    add(savedGamesBox);

    Box buttonsBox = Box.createHorizontalBox();
    updateButton = new JButton("Update games");
    updateButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        updateGames();
      }
    });
    updateButton.setEnabled(false);
    buttonsBox.add(updateButton);
    JButton helpButton = new JButton("Help");

    HelpFile hf = null;
    try {
      hf = new HelpFile(null, new File(
        new File(Documentation.getDocumentationBaseDir(), "ReferenceManual"),
        "SavedGameUpdater.htm"));
    }
    catch (MalformedURLException ex) {
      ErrorDialog.bug(ex);
    }

    helpButton.addActionListener(new ShowHelpAction(hf.getContents(), null));
    buttonsBox.add(helpButton);
    JButton closeButton = new JButton("Close");
    closeButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        dispose();
      }
    });
    buttonsBox.add(closeButton);
    add(buttonsBox);
    pack();
    setLocationRelativeTo(getOwner());
  }

  private void updateGames() {
    updateButton.setEnabled(false);
    Runnable runnable = new Runnable() {
      public void run() {
        for (int i=0,n=savedGamesModel.size();i<n;++i) {
          try {
            File savedGame = (File)savedGamesModel.getElementAt(i);
            updater.updateSavedGame(oldPieceInfo,savedGame);
            GameModule.getGameModule().warn("Updated "+savedGame.getName()+" from version "+versionField.getText()+" to "+GameModule.getGameModule().getGameVersion());
          }
          // FIXME: review error message
          catch (final IOException e) {
            Runnable showError = new Runnable() {
              public void run() {
                showErrorMessage(e,"Update failed","Unable to save file");
              }
            };
            try {
              SwingUtilities.invokeAndWait(showError);
            }
            // FIXME: review error message
            catch (InterruptedException e1) {
            }
            // FIXME: review error message
            catch (InvocationTargetException e1) {
            }
          }
        }
        updateButton.setEnabled(true);
      }
    };
    new Thread(runnable).start();
  }

  private void chooseSavedGames() {
    fc.setMultiSelectionEnabled(true);
    if (JFileChooser.CANCEL_OPTION != fc.showOpenDialog(this)) {
      File[] selectedFiles = fc.getSelectedFiles();
      if (selectedFiles != null) {
        savedGamesModel.clear();
        for (int i = 0; i < selectedFiles.length; i++) {
          savedGamesModel.addElement(selectedFiles[i]);
        }
      }
    }
  }

  private void exportPieceInfo() {
    fc.setMultiSelectionEnabled(false);
    if (JFileChooser.CANCEL_OPTION != fc.showSaveDialog(this)) {
      final Properties p = updater.getPieceSlotsMap();
      p.put(MODULE_NAME_KEY, GameModule.getGameModule().getGameName());
      p.put(VERSION_KEY, GameModule.getGameModule().getGameVersion());

      BufferedOutputStream out = null;
      try {
        out = new BufferedOutputStream(
                new FileOutputStream(fc.getSelectedFile()));
        p.store(out, null);
        out.close();
      }
      // FIXME: review error message
      catch (IOException e) {
        showErrorMessage(e, "Export failed","Unable to write info");
      }
      finally {
        IOUtils.closeQuietly(out);
      }
    }
  }

  private void importPieceInfo() {
    fc.setMultiSelectionEnabled(false);
    if (JFileChooser.CANCEL_OPTION != fc.showOpenDialog(this)) {
      oldPieceInfo = new Properties();

      BufferedInputStream in = null;
      try {
        in = new BufferedInputStream(new FileInputStream(fc.getSelectedFile()));
        oldPieceInfo.load(in);
        in.close();

        String moduleVersion = oldPieceInfo.getProperty(VERSION_KEY);
        String moduleName = oldPieceInfo.getProperty(MODULE_NAME_KEY);
        if (!GameModule.getGameModule().getGameName().equals(moduleName)) {
          showErrorMessage(null,"Import failed","Imported info is from the wrong module:  "+moduleName);
          oldPieceInfo = null;
          versionField.setText(null);
        }
        else if (GameModule.getGameModule().getGameVersion().equals(moduleVersion)) {
          showErrorMessage(null,"Import failed","Imported info is from the current version, "+moduleVersion+".\nLoad the older version in the editor and export the GamePiece info,\nThen load this module again and import the older version's info");
          oldPieceInfo = null;
          versionField.setText(null);
        }
        else {
          versionField.setText(moduleVersion);
        }
      }
      // FIXME: review error message
      catch (IOException e) {
        showErrorMessage(e,"Import failed","Unable to import info");
        oldPieceInfo = null;
      }
      catch (IllegalArgumentException e) { // catches malformed input files
        showErrorMessage(e, "Import failed", "Malformed input file");
        oldPieceInfo = null;
      }
      finally {
        IOUtils.closeQuietly(in);
      }
    }
    updateButton.setEnabled(oldPieceInfo != null);
  }

  private void showErrorMessage(Exception e, String title, String defaultMessage) {
    String msg = e == null ? null : e.getMessage();
    if (msg == null) {
      msg = defaultMessage;
    }
    JOptionPane.showMessageDialog(this, msg, title, JOptionPane.ERROR_MESSAGE);
  }
}
