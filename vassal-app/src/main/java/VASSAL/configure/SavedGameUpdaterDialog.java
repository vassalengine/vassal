/*
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
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.MalformedURLException;
import java.nio.file.Files;
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
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.SavedGameUpdater;
import VASSAL.tools.ScrollPane;

public class SavedGameUpdaterDialog extends JDialog {
  private static final long serialVersionUID = 1L;

  private DefaultListModel<File> savedGamesModel;
  private final SavedGameUpdater updater = new SavedGameUpdater();
  private Properties oldPieceInfo;
  private final JFileChooser fc;

  //FIXME Is it really supposed to be moduleVerion[sic] below? Is it okay to "fix" it?
  private static final String VERSION_KEY = "moduleVerion"; //NON-NLS

  private static final String MODULE_NAME_KEY = "moduleName"; //NON-NLS
  private JButton updateButton;
  private JTextField versionField;

  public SavedGameUpdaterDialog(Frame owner) throws HeadlessException {
    super(owner, false);
    setTitle(Resources.getString("Editor.SavedGameUpdaterDialog.title"));
    initComponents();
    fc = new JFileChooser();
    fc.setCurrentDirectory(GameModule.getGameModule().getFileChooser().getCurrentDirectory());
  }

  private void initComponents() {
    setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));
    final Box versionBox = Box.createHorizontalBox();
    versionBox.add(new JLabel(Resources.getString("Editor.SavedGameUpdaterDialog.module_version")));
    versionField = new JTextField(8);
    versionField.setEditable(false);
    versionField.setMaximumSize(new Dimension(versionField.getMaximumSize().width, versionField.getPreferredSize().height));
    versionBox.add(versionField);
    final JButton importButton = new JButton(Resources.getString("Editor.SavedGameUpdaterDialog.import_gamepiece"));
    importButton.addActionListener(e -> importPieceInfo());
    versionBox.add(importButton);
    add(versionBox);
    final JButton exportButton = new JButton(Resources.getString("Editor.SavedGameUpdaterDialog.export_gamepiece"));
    exportButton.addActionListener(e -> exportPieceInfo());
    final Box importExportBox = Box.createHorizontalBox();
    importExportBox.add(importButton);
    importExportBox.add(exportButton);
    add(importExportBox);

    final Box savedGamesBox = Box.createHorizontalBox();
    final Box left = Box.createVerticalBox();
    left.add(new JLabel(Resources.getString("Editor.SavedGameUpdaterDialog.saved_games")));
    final JButton chooseGamesButton = new JButton(Resources.getString("Editor.SavedGameUpdaterDialog.choose"));
    chooseGamesButton.addActionListener(e -> chooseSavedGames());
    left.add(chooseGamesButton);
    savedGamesBox.add(left);
    savedGamesModel = new DefaultListModel<>();
    final JList<File> savedGamesList = new JList<>(savedGamesModel);
    savedGamesList.setVisibleRowCount(5);
    savedGamesList.setCellRenderer(new DefaultListCellRenderer() {
      private static final long serialVersionUID = 1L;

      @Override
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

    final Box buttonsBox = Box.createHorizontalBox();
    updateButton = new JButton(Resources.getString("Editor.SavedGameUpdaterDialog.update_games"));
    updateButton.addActionListener(e -> updateGames());
    updateButton.setEnabled(false);
    buttonsBox.add(updateButton);
    final JButton helpButton = new JButton(Resources.getString("Editor.SavedGameUpdaterDialog.help"));

    HelpFile hf = null;
    try {
      hf = new HelpFile(null, new File(
        new File(Documentation.getDocumentationBaseDir(), "ReferenceManual"),
        "SavedGameUpdater.html"));
    }
    catch (MalformedURLException ex) {
      ErrorDialog.bug(ex);
    }

    helpButton.addActionListener(new ShowHelpAction(hf.getContents(), null));
    buttonsBox.add(helpButton);
    final JButton closeButton = new JButton(Resources.getString("Editor.SavedGameUpdaterDialog.close"));
    closeButton.addActionListener(e -> dispose());
    buttonsBox.add(closeButton);
    add(buttonsBox);
    pack();
    setLocationRelativeTo(getOwner());
  }

  private void updateGames() {
    updateButton.setEnabled(false);
    final Runnable runnable = () -> {
      final int n =  savedGamesModel.size();
      for (int i = 0; i < n; ++i) {
        try {
          final File savedGame = savedGamesModel.getElementAt(i);
          updater.updateSavedGame(oldPieceInfo, savedGame);
          GameModule.getGameModule().warn(Resources.getString("Editor.SavedGameUpdaterDialog.updated_message", savedGame.getName(), versionField.getText(), GameModule.getGameModule().getGameVersion()));
        }
        // FIXME: review error message
        catch (final IOException e) {
          final Runnable showError = () -> showErrorMessage(e, Resources.getString("Editor.SavedGameUpdaterDialog.fail"), Resources.getString("Editor.SavedGameUpdaterDialog.unable"));
          try {
            SwingUtilities.invokeAndWait(showError);
          }
          // FIXME: review error message
          catch (InterruptedException | InvocationTargetException e1) {
          }
        }
      }
      updateButton.setEnabled(true);
    };
    new Thread(runnable).start();
  }

  private void chooseSavedGames() {
    fc.setMultiSelectionEnabled(true);
    if (JFileChooser.CANCEL_OPTION != fc.showOpenDialog(this)) {
      final File[] selectedFiles = fc.getSelectedFiles();
      if (selectedFiles != null) {
        savedGamesModel.clear();
        for (final File selectedFile : selectedFiles) {
          savedGamesModel.addElement(selectedFile);
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

      try (OutputStream fout = Files.newOutputStream(fc.getSelectedFile().toPath());
           BufferedOutputStream out = new BufferedOutputStream(fout)) {
        p.store(out, null);
      }
      catch (IOException e) {
        showErrorMessage(e, Resources.getString("Editor.SavedGameUpdaterDialog.ex_fail"), Resources.getString("Editor.SavedGameUpdaterDialog.ex_unable"));
      }
    }
  }

  private void importPieceInfo() {
    fc.setMultiSelectionEnabled(false);
    if (JFileChooser.CANCEL_OPTION != fc.showOpenDialog(this)) {
      oldPieceInfo = new Properties();

      try (InputStream fin = Files.newInputStream(fc.getSelectedFile().toPath());
           BufferedInputStream in = new BufferedInputStream(fin)) {
        oldPieceInfo.load(in);

        final String moduleVersion = oldPieceInfo.getProperty(VERSION_KEY);
        final String moduleName = oldPieceInfo.getProperty(MODULE_NAME_KEY);
        if (!GameModule.getGameModule().getGameName().equals(moduleName)) {
          showErrorMessage(null, Resources.getString("Editor.SavedGameUpdaterDialog.im_fail"), Resources.getString("Editor.SavedGameUpdaterDialog.im_wrong", moduleName));
          oldPieceInfo = null;
          versionField.setText(null);
        }
        else if (GameModule.getGameModule().getGameVersion().equals(moduleVersion)) {
          showErrorMessage(null, Resources.getString("Editor.SavedGameUpdaterDialog.im_fail"), Resources.getString("Editor.SavedGameUpdaterDialog.im_current", moduleVersion));
          oldPieceInfo = null;
          versionField.setText(null);
        }
        else {
          versionField.setText(moduleVersion);
        }
      }
      // FIXME: review error message
      catch (IOException e) {
        showErrorMessage(e, Resources.getString("Editor.SavedGameUpdaterDialog.im_fail"), Resources.getString("Editor.SavedGameUpdaterDialog.im_unable"));
        oldPieceInfo = null;
      }
      catch (IllegalArgumentException e) { // catches malformed input files
        showErrorMessage(e, Resources.getString("Editor.SavedGameUpdaterDialog.im_fail"), Resources.getString("Editor.SavedGameUpdaterDialog.im_malformed"));
        oldPieceInfo = null;
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
