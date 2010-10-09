/*
 * $Id$
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

import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Locale;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;

import VASSAL.Info;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.ShowHelpAction;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.filechooser.ExtensionFileFilter;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.io.IOUtils;

public class TranslateVassalWindow extends TranslateWindow {
  private static final long serialVersionUID = 1L;
  protected LocaleConfigurer localeConfig;

  protected FileChooser fileChooser;

  public TranslateVassalWindow(Frame owner,
                               boolean modal,
                               Translatable target,
                               HelpWindow helpWindow,
                               ConfigureTree tree) {
    super(owner, modal, target, helpWindow, tree);
  }

  public TranslateVassalWindow(Frame owner) {
    super(owner, false, new VassalTranslation(), null, null);
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
    JSplitPane pane = (JSplitPane) super.buildMainPanel();
    return pane.getBottomComponent();
  }

  @Override
  protected Component getHeaderPanel() {
    final JPanel headPanel = new JPanel();
    localeConfig =
      new LocaleConfigurer(null, "",
        new Locale(Locale.getDefault().getLanguage()));

    localeConfig.addPropertyChangeListener(new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        Locale l = localeConfig.getValueLocale();
        if (!Resources.getSupportedLocales().contains(l)) {
          l = new Locale(l.getLanguage());
        }

        if (Resources.getSupportedLocales().contains(l)) {
          final String filename = "VASSAL_" + l + ".properties";
          final InputStream is = getClass().getResourceAsStream(filename);
          if (is != null) {
            BufferedInputStream in = null;
            try {
              in = new BufferedInputStream(is);
              ((VassalTranslation)target).loadProperties(in);
              ((MyTableModel) keyTable.getModel()).fireTableDataChanged();
              in.close();
            }
            catch (IOException e) {
              ReadErrorDialog.error(e, filename);
            }
            finally {
              IOUtils.closeQuietly(in);
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
    helpButton.addActionListener(new ShowHelpAction(HelpFile.getReferenceManualPage("Translations.htm","application").getContents(),null));

    final JButton loadButton = new JButton(Resources.getString(Resources.LOAD));
    loadButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadTranslation();
      }
    });
    buttonBox.add(helpButton);
    buttonBox.add(loadButton);

    final JButton okButton = new JButton(Resources.getString(Resources.SAVE));
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
// FIXME: can this ever throw?
          save();
        }
        catch (IOException e1) {
// FIXME: error dialog
        }
      }
    });
    buttonBox.add(okButton);

    final JButton cancelButton =
      new JButton(Resources.getString(Resources.CANCEL));
    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        cancel();
      }
    });
    buttonBox.add(cancelButton);
    return buttonBox;
  }

  protected void newTranslation() {
    ((VassalTranslation) target).clearProperties();
    ArrayList<String> keyList = new ArrayList<String>(Resources.getVassalKeys());
    Collections.sort(keyList);
    keys = keyList.toArray(new String[keyList.size()]);
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
    fc.setFileFilter(new ExtensionFileFilter("Property Files",
                     new String[]{".properties"}));
    fc.setCurrentDirectory(Info.getHomeDir());
    if (fc.showOpenDialog(this) != FileChooser.APPROVE_OPTION) return;

    final File file = fc.getSelectedFile();
    if (!file.getName().endsWith(".properties")) {
// FIXME: review error message
      loadError("Module Properties files must end in '.properties'.");
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

    BufferedInputStream in = null;
    try {
      in = new BufferedInputStream(new FileInputStream(file));
      ((VassalTranslation) target).loadProperties(in);
      in.close();
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, file);
    }
    finally {
      IOUtils.closeQuietly(in);
    }

    ((MyTableModel) keyTable.getModel()).fireTableDataChanged();
  }

  protected void loadError(String mess) {
    JOptionPane.showMessageDialog(this, mess, "Invalid Properties file name", JOptionPane.ERROR_MESSAGE);
    return;
  }

  @Override
  protected boolean saveTranslation() {
    final FileChooser fc = getFileChooser();
    final Locale l = localeConfig.getValueLocale();
    String bundle = "VASSAL_" + l.getLanguage();
    if (l.getCountry() != null && l.getCountry().length() > 0) {
      bundle += "_" + l.getCountry();
    }
    bundle += ".properties";

    fc.setSelectedFile(new File(Info.getHomeDir(), bundle));
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

  public static void main(String args[]) {
    SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        final TranslateVassalWindow w = new TranslateVassalWindow(null);
        w.setVisible(true);
      }
    });
  }
}
