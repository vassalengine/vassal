package VASSAL.i18n;

import java.awt.Component;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Locale;

import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpWindow;
import VASSAL.configure.ConfigureTree;
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
    newTranslation();
  }

  protected Component getHeaderPanel() {
    JPanel headPanel = new JPanel();
    localeConfig = new LocaleConfigurer(null, "", new Locale(Locale.getDefault().getLanguage()));
    headPanel.add(localeConfig.getControls());
     
    return headPanel;
  }
  
  protected Component getButtonPanel() {
    JPanel buttonBox = new JPanel();
    
    JButton loadButton = new JButton(Resources.getString(Resources.LOAD));
    loadButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        loadTranslation();
      }});
    buttonBox.add(loadButton);
    
    
    JButton okButton = new JButton(Resources.getString(Resources.SAVE));
    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        save();
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
    
    ArrayList keyList = new ArrayList();
    for (Enumeration e = Resources.getVassalKeys(); e.hasMoreElements(); keyList.add(e.nextElement())) {
      ;
    }
    Collections.sort(keyList);
    keys = (String[]) keyList.toArray(new String[0]);
    copyButtons = new CopyButton[keys.length];
    ((MyTableModel) keyTable.getModel()).update();
  }
  
  
  protected void loadTranslation() {
    if (currentTranslation.isDirty()) {
      if (!querySave()) {
        return;
      }
    }
    FileChooser fc = GameModule.getGameModule().getFileChooser();
    fc.setFileFilter(new ExtensionFileFilter("Property Files", new String[] {".properties"}));
    if (fc.showOpenDialog(this) != FileChooser.APPROVE_OPTION)
      return;
    File file = fc.getSelectedFile();
    if (! file.getName().startsWith(Resources.MODULE_BUNDLE)) {
      loadError("Module Properties files must start with 'Module_'.");
      return;
    } 
    else if (! file.getName().endsWith(".properties")) {
      loadError("Module Properties files must end in '.properties'.");
      return;
    }
    else {
      String language = file.getName().substring(7,9);
      String country = "";
      if (file.getName().charAt(9) == '_') {
        country = file.getName().substring(10,12);
      }
      Locale locale = new Locale(language, country);
      localeConfig.setValue(locale);
    }

    try {
      ((VassalTranslation) target).loadProperties(file);
    }
    catch (IOException e) {
      e.printStackTrace();
      String msg = e.getMessage();
      if (msg == null) {
        msg = "Unable to load translation";
      }
      loadError(msg);
    }
  }

  protected void loadError(String mess) {
    JOptionPane.showMessageDialog(this, mess, "Invalid Properties file name", JOptionPane.ERROR_MESSAGE);
    return;
  }

  protected boolean saveTranslation() {
    FileChooser fc = GameModule.getGameModule().getFileChooser();
    Locale l = localeConfig.getValueLocale();
    String bundle = Resources.MODULE_BUNDLE + "_" + l.getLanguage();
    if (l.getCountry() != null && l.getCountry().length() > 0) {
      bundle += "_" + l.getCountry();
    }
    bundle += ".properties";
    fc.setSelectedFile(new File(bundle));

    if (fc.showSaveDialog(this) != FileChooser.APPROVE_OPTION)
      return false;
    
    File outputFile = fc.getSelectedFile();
    if (outputFile != null &&
        outputFile.exists() &&
        JOptionPane.NO_OPTION ==
         JOptionPane.showConfirmDialog(GameModule.getGameModule().getFrame(),
          outputFile.getName() + " already exists. OK to overwrite?", "File Exists", 
          JOptionPane.YES_NO_OPTION)) {
        return false;
    } 
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
