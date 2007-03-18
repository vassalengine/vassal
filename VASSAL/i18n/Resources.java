package VASSAL.i18n;

import java.io.File;
import java.io.FileInputStream;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Date;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.UIManager;

import VASSAL.build.GameModule;

public class Resources {

  protected static ResourceBundle vassalBundle;
  protected static ResourceBundle editorBundle;
  
  protected static String VASSAL_BUNDLE = "VASSAL.i18n.VASSAL";
  protected static String EDITOR_BUNDLE = "VASSAL.i18n.Editor";

  /*
   * Commonly used keys
   */
  public static final String VASSAL = "General.VASSAL";
  public static final String ADD = "General.add";
  public static final String REMOVE = "General.remove";
  public static final String INSERT = "General.insert";
  public static final String YES = "General.yes";
  public static final String NO = "General.no";
  public static final String CANCEL = "General.cancel";
  public static final String SAVE = "General.save";
  public static final String OK = "General.ok";
  public static final String MENU = "General.menu";
  public static final String LOAD = "General.load";
  public static final String QUIT = "General.quit";
  public static final String EDIT = "General.edit";
  public static final String NEW = "General.new";
  public static final String FILE = "General.file";
  public static final String HELP = "General.help";
  public static final String CLOSE = "General.close";
  public static final String DATE_DISPLAY = "General.date_display";
  public static final String NEXT = "General.next";
  public static final String REFRESH = "General.refresh";
  
  public static final String EDITOR_PREFIX = "Editor.";
  
  public static final String BUTTON_TEXT = "Editor.button_text_label";
  public static final String TOOLTIP_TEXT = "Editor.tooltip_text_label";
  public static final String BUTTON_ICON = "Editor.button_icon_label";
  public static final String HOTKEY_LABEL = "Editor.hotkey_label";
  public static final String COLOR_LABEL = "Editor.color_label";
  public static final String NAME_LABEL = "Editor.name_label";

  /*
   * Return the i18nized version of a String
   */
  public static boolean initialized = false;
  public static File homeDir;
  
  public static void init(File dir) {
    homeDir = dir;
  }

  static {
    UIManager.put("OptionPane.yesButtonText", getString(YES));
    UIManager.put("OptionPane.cancelButtonText", getString(CANCEL));
    UIManager.put("OptionPane.noButtonText", getString(NO));
    UIManager.put("OptionPane.okButtonText", getString(OK));
  }
  
  /*
   * Translate a VASSAL user interface String
   */
  public static String getString(String id) {
    if (id.startsWith(EDITOR_PREFIX)) {
      return getEditorString(id);
    }
    else {
      return getVassalString(id);
    }
  }
  public static String getVassalString(String id) {
    if (vassalBundle == null) {
      vassalBundle = ResourceBundle.getBundle(VASSAL_BUNDLE, Locale.getDefault(), homeDir == null ? Resources.class.getClassLoader() : new VassalPropertyClassLoader());
    }
    return getString(vassalBundle, id);
  }
  
  /*
   * Translate a Module editor String
   */
  public static String getEditorString(String id) {
    if (editorBundle == null) {
      editorBundle = ResourceBundle.getBundle(EDITOR_BUNDLE, Locale.getDefault(), homeDir == null ? Resources.class.getClassLoader() : new VassalPropertyClassLoader());
   }
   return getString(editorBundle, id);
  }
  
  /*
   * Translate a string using the supplied resource bundle
   */
  public static String getString(ResourceBundle bundle, String id) {
  
    String s = null;
    
    try {
      s = bundle.getString(id);
    }
    catch (Exception ex) {
      GameModule.getGameModule().warn("No Translation: " + id);
    }
    
    // 2. Worst case, return the key    
    if (s == null) {
      s = id;
    }
    
    return s;
  }

  /*
   * Format a string with options. 
   * Convenience methods for most common case of one or two parameters. 
   * Will be heavily used, so try and minimise the number of Object arrays being created
   */
  public static MessageFormat formatter = new MessageFormat("");
  public static Object[] object1 = new Object[1];
  public static Object[] object2 = new Object[2];

  public static String getString(String id, Date date) {
    formatter.applyPattern(getString(id));
    object1[0] = date; 
    return formatter.format(object1);
  }
  
  public static String getString(String id, String option) {
    formatter.applyPattern(getString(id));
    object1[0] = option;
    return formatter.format(object1);
  }

  public static String getString(String id, String option0, String option1) {
    formatter.applyPattern(getString(id));
    object2[0] = option0;
    object2[1] = option1;
    return formatter.format(object2);
  }
  
  public static String getString (String id, String[] options) {
    formatter.applyPattern(getString(id));
    return formatter.format(options);
  }
  
  
  /**
   * Custom Class Loader for loading VASSAL property files.
   * Check first for files in the VASSAL home directory
   *
   */
  public static class VassalPropertyClassLoader extends ClassLoader {
    public URL getResource(String name) {
      URL url = null;
      String propFileName = name.substring(name.lastIndexOf('/')+1);
      File propFile = new File(homeDir, propFileName);
 
      try {
        FileInputStream in = new FileInputStream(propFile);
        in.close();
        url = new URL("file", "", propFile.getCanonicalPath());
      }
      catch (Exception e) {    
      }
      
      /*
       * No openable file in Doc dir, so let Java find one for us in the standard classpath.
       */
      if (url == null) {
        url = super.getResource(name);
      }
      return url;
    }
  }
}