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
import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.ShowHelpAction;
import VASSAL.tools.ExtensionFileFilter;
import VASSAL.tools.FileChooser;

public class TranslateVassalWindow extends TranslateWindow {
  private static final long serialVersionUID = 1L;
  protected LocaleConfigurer localeConfig;

  public TranslateVassalWindow(Frame owner, boolean modal, Translatable target, HelpWindow helpWindow, ConfigureTree tree) {
    super(owner, modal, target, helpWindow, tree);
  }

  public TranslateVassalWindow(Frame owner) {
    super(owner, false, new VassalTranslation(), null, null);
    currentTranslation = (Translation) target;
    keyTable.setEnabled(true);
    newTranslation();
  }

  protected Component buildMainPanel() {
    JSplitPane pane = (JSplitPane) super.buildMainPanel();
    return pane.getBottomComponent();
  }

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
          final InputStream in =
            getClass().getResourceAsStream("VASSAL_"+l+".properties");
          if (in != null) {
            try {
              ((VassalTranslation)target).loadProperties(in);
              ((MyTableModel) keyTable.getModel()).fireTableDataChanged();
            }
            catch (IOException e) {
              e.printStackTrace();
            }
            finally {
              try {
                in.close();
              }
              catch (IOException e) {
                e.printStackTrace();
              }
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

  protected Component getButtonPanel() {
    JPanel buttonBox = new JPanel();
    JButton helpButton = new JButton(Resources.getString(Resources.HELP));
    helpButton.addActionListener(new ShowHelpAction(HelpFile.getReferenceManualPage("Translations.htm#application").getContents(),null));;
    JButton loadButton = new JButton(Resources.getString(Resources.LOAD));
    loadButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadTranslation();
      }
    });
    buttonBox.add(helpButton);
    buttonBox.add(loadButton);
    JButton okButton = new JButton(Resources.getString(Resources.SAVE));
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          save();
        }
        catch (IOException e1) {
          reportSaveError(e1);
        }
      }
    });
    buttonBox.add(okButton);
    JButton cancelButton = new JButton(Resources.getString(Resources.CANCEL));
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
        reportSaveError(e);
        return;
      }
    }

    final FileChooser fc = GameModule.getGameModule().getFileChooser();
    fc.setFileFilter(new ExtensionFileFilter("Property Files",
                     new String[]{".properties"}));
    if (fc.showOpenDialog(this) != FileChooser.APPROVE_OPTION) return;

    final File file = fc.getSelectedFile();
    if (!file.getName().endsWith(".properties")) {
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

    try {
      final FileInputStream in = new FileInputStream(file);
      try {
        ((VassalTranslation) target).loadProperties(in);
      }
      finally {
        try {
          in.close();
        }
        catch (IOException e) {
          e.printStackTrace();
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      String msg = e.getMessage();
      if (msg == null) {
        msg = "Unable to load translation";
      }
      loadError(msg);
    }

    ((MyTableModel) keyTable.getModel()).fireTableDataChanged();
  }

  protected void loadError(String mess) {
    JOptionPane.showMessageDialog(this, mess, "Invalid Properties file name", JOptionPane.ERROR_MESSAGE);
    return;
  }

  protected boolean saveTranslation() {
    FileChooser fc = GameModule.getGameModule().getFileChooser();
    Locale l = localeConfig.getValueLocale();
    String bundle = "VASSAL_" + l.getLanguage();
    if (l.getCountry() != null && l.getCountry().length() > 0) {
      bundle += "_" + l.getCountry();
    }
    bundle += ".properties";
    fc.setSelectedFile(new File(Info.getHomeDir(), bundle));
    if (fc.showSaveDialog(this) != FileChooser.APPROVE_OPTION)
      return false;
    File outputFile = fc.getSelectedFile();
    try {
      ((VassalTranslation) target).saveProperties(outputFile, localeConfig.getValueLocale());
    }
    catch (IOException e) {
      String msg = e.getMessage();
      if (msg == null) {
        msg = "Unable to save translation";
      }
      e.printStackTrace();
      JOptionPane.showMessageDialog(GameModule.getGameModule().getFrame(), msg);
      return false;
    }
    return true;
  }
}
