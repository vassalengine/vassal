/*
 *
 * Copyright (c) 2007 by Brent Easton
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
package VASSAL.i18n;

import VASSAL.Info;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.ShowHelpAction;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.ExtensionFileFilter;
import VASSAL.tools.filechooser.FileChooser;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;
import java.awt.Component;
import java.awt.Frame;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Locale;

public class TranslateVassalWindow extends TranslateWindow {
  private static final long serialVersionUID = 1L;
  protected LocaleConfigurer localeConfig;

  protected FileChooser fileChooser;

  public TranslateVassalWindow(Frame owner,
                               boolean modal,
                               Translatable target,
                               ConfigureTree tree) {
    super(owner, modal, target, tree);
  }

  public TranslateVassalWindow(Frame owner) {
    super(owner, false, new VassalTranslation(), null);
    currentTranslation = (Translation) target;
    keyTable.setEnabled(true);
    newTranslation();
  }

  protected FileChooser getFileChooser() {
    if (fileChooser == null) {
      fileChooser = FileChooser.createFileChooser(this, null);
    }
    else {
      fileChooser.resetChoosableFileFilters();
      fileChooser.rescanCurrentDirectory();
    }
    return fileChooser;
  }

  @Override
  protected Component buildMainPanel() {
    final JSplitPane pane = (JSplitPane) super.buildMainPanel();
    return pane.getBottomComponent();
  }

  @Override
  protected Component getHeaderPanel() {
    final JPanel headPanel = new JPanel();
    localeConfig =
      new LocaleConfigurer(null, "",
        new Locale(Locale.getDefault().getLanguage()));

    localeConfig.addPropertyChangeListener(new PropertyChangeListener() {
      @Override
      public void propertyChange(PropertyChangeEvent evt) {
        Locale l = localeConfig.getValueLocale();
        if (!Resources.getSupportedLocales().contains(l)) {
          l = new Locale(l.getLanguage());
        }

        if (Resources.getSupportedLocales().contains(l)) {
          final String filename = "VASSAL_" + l + ".properties"; //NON-NLS
          final InputStream is = getClass().getResourceAsStream(filename);
          if (is != null) {
            try (BufferedInputStream in = new BufferedInputStream(is)) {
              ((VassalTranslation) target).loadProperties(in);
              ((MyTableModel) keyTable.getModel()).fireTableDataChanged();
            }
            catch (IOException e) {
              ReadErrorDialog.error(e, filename);
            }
          }
        }
        else {
          ((VassalTranslation) target).clearProperties();
          ((MyTableModel) keyTable.getModel()).fireTableDataChanged();
        }
      }
    });

    headPanel.add(localeConfig.getControls());
    return headPanel;
  }

  @Override
  protected Component getButtonPanel() {
    final JPanel buttonBox = new JPanel();
    final JButton helpButton = new JButton(Resources.getString(Resources.HELP));
    helpButton.addActionListener(new ShowHelpAction(HelpFile.getReferenceManualPage("Translations.html", "application").getContents(), null)); //NON-NLS

    final JButton loadButton = new JButton(Resources.getString(Resources.LOAD));
    loadButton.addActionListener(e -> loadTranslation());
    buttonBox.add(helpButton);
    buttonBox.add(loadButton);

    okButton = new JButton(Resources.getString(Resources.SAVE));
    okButton.addActionListener(e -> {
      try {
// FIXME: can this ever throw?
        save();
      }
      catch (IOException e1) {
// FIXME: error dialog
      }
    });
    buttonBox.add(okButton);

    cancelButton =
      new JButton(Resources.getString(Resources.CANCEL));
    cancelButton.addActionListener(e -> cancel());
    buttonBox.add(cancelButton);
    return buttonBox;
  }

  protected void newTranslation() {
    ((VassalTranslation) target).clearProperties();
    final ArrayList<String> keyList = new ArrayList<>(Resources.getVassalKeys());
    Collections.sort(keyList);
    keys = keyList.toArray(new String[0]);
    copyButtons = new CopyButton[keys.length];
    ((MyTableModel) keyTable.getModel()).update();
  }

  protected void loadTranslation() {
    if (currentTranslation.isDirty()) {
      try {
        if (!querySave()) {
          return;
        }
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, currentTranslation.getBundleFileName());
        return;
      }
    }

    final FileChooser fc = getFileChooser();
    fc.setFileFilter(new ExtensionFileFilter(Resources.getString("Editor.TranslateVassalWindow.property_files"),
                     new String[]{".properties"}));  //NON-NLS
    fc.setCurrentDirectory(Info.getConfDir());
    if (fc.showOpenDialog(this) != FileChooser.APPROVE_OPTION) return;

    final File file = fc.getSelectedFile();
    if (!file.getName().endsWith(".properties")) {  //NON-NLS
// FIXME: review error message
      loadError(Resources.getString("Editor.TranslateVassalWindow.must_end_in"));
      return;
    }
    else {
      final String language = file.getName().substring(7, 9);
      String country = "";
      if (file.getName().charAt(9) == '_') {
        country = file.getName().substring(10, 12);
      }
      final Locale locale = new Locale(language, country);
      localeConfig.setValue(locale);
    }

    try (InputStream fin = Files.newInputStream(file.toPath());
         BufferedInputStream in = new BufferedInputStream(fin)) {
      ((VassalTranslation) target).loadProperties(in);
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, file);
    }

    ((MyTableModel) keyTable.getModel()).fireTableDataChanged();
  }

  protected void loadError(String mess) {
    JOptionPane.showMessageDialog(this, mess, Resources.getString("Editor.TranslateVassalWindow.invalid"), JOptionPane.ERROR_MESSAGE);
  }

  @Override
  protected boolean saveTranslation() {
    final FileChooser fc = getFileChooser();
    final Locale l = localeConfig.getValueLocale();
    String bundle = "VASSAL_" + l.getLanguage();  //NON-NLS
    if (l.getCountry() != null && l.getCountry().length() > 0) {
      bundle += "_" + l.getCountry();
    }
    bundle += ".properties";  //NON-NLS

    fc.setSelectedFile(new File(Info.getConfDir(), bundle));
    if (fc.showSaveDialog(this) != FileChooser.APPROVE_OPTION) return false;

    final File outputFile = fc.getSelectedFile();
    try {
      ((VassalTranslation) target).saveProperties(
        outputFile, localeConfig.getValueLocale());
    }
    catch (IOException e) {
      WriteErrorDialog.error(e, outputFile);
      return false;
    }

    return true;
  }

  public static void main(String[] args) {
    SwingUtilities.invokeLater(() -> {
      final TranslateVassalWindow w = new TranslateVassalWindow(null);
      w.setVisible(true);
    });
  }
}
