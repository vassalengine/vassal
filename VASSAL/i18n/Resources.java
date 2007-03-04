package VASSAL.i18n;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.TreeMap;

import VASSAL.build.GameModule;
import VASSAL.build.module.Documentation;
import VASSAL.configure.DirectoryConfigurer;

public class Resources {

  protected static HashMap strings = new HashMap();
  protected static ResourceBundle stringBundle;


  /*
   * Commonly used Strings
   */
  public static final String BUTTON_TEXT = registerString("general.button_text_label",
      "Button text:  ");
  public static final String TOOLTIP_TEXT = registerString("general.tooltip_text_label",
      "Tooltip Text:  ");
  public static final String BUTTON_ICON = registerString("general.button_icon_label",
      "Button Icon:  ");
  public static final String HOTKEY_LABEL = registerString("general.hotkey_label", "Hotkey:  ");
  public static final String COLOR_LABEL = registerString("general.color_label", "Color:  ");
  public static final String NAME_LABEL = registerString("general.name_label", "Name:  ");
  public static final String ADD = registerString("general.add_button_text", "Add");
  public static final String REMOVE = registerString("general.remove_button_text", "Remove");
  public static final String INSERT = registerString("general.insert_button_text", "Insert");
  public static final String YES = registerString("general.yes", "Yes");
  public static final String NO = registerString("general.no", "No");
  public static final String CANCEL = registerString("general.cancel", "Cancel");
  public static final String SAVE = registerString("general.save", "Save");
  public static final String OK = registerString("general.save", "OK");
  public static final String MENU = registerString("general.menu", "Menu");
  public static final String QUIT = registerString("general.quit", "Quit");

  /*
   * BasicModule
   */
  public static final String BASICMODULE_MODULE_ERROR = registerString("basicmodule.not_a_module",
      "Not a VASSAL module");
  public static final String BASICMODULE_MODULE_MESSAGE = registerString(
      "basicmodule.module_message", "$gameName$ version $moduleVersion$");
  /*
   * GameModule
   */
  public static final String GAMEMODULE_COMPONENT_TYPE = registerString(
      "gamemodule.component_type", "Module");
  public static final String GAMEMODULE_VERSION_ERROR = registerString(
      "gamemodule.version_error",
      "This module was created using version $requiredVersion$ of the VASSAL engine.\nYou are using version $currentVersion$\nIt's recommended you upgrade to the latest version of the VASSAL engine.");
  public static final String GAMEMODULE_VERSION_ERROR2 = registerString(
      "gamemodule.version_error_short", "Older version in use");
  public static final String GAMEMODULE_NAME_LABEL = registerString("gamemodule.name_label",
      "Game Name:  ");
  public static final String GAMEMODULE_VERSION_LABEL = registerString("gamemodule.version_label",
      "Version No.:  ");
  public static final String GAMEMODULE_FRAME_TITLE = registerString("gamemodule.frame_title",
      "$gameName$ controls");
  public static final String GAMEMODULE_SAVE_STRING = registerString("gamemodule.save",
      "Save Module?");
  public static final String GAMEMODULE_OPEN_ERROR = registerString("gamemodule.open_error",
      "Module $moduleName$ is already open");
  public static final String GAMEMODULE_SAVE_ERROR = registerString("gamemodule.save_error",
      "Couldn't save module.");
  public static final String GAMEMODULE_SAVE_ERROR2 = registerString("gamemodule.save_error_short",
      "Unable to save");

  /*
   * ToolbarMenu
   */
  public static final String TOOLBARMENU_COMPONENT_TYPE = registerString(
      "toolbarmenu.component_type", "Toolbar Menu");
  public static final String TOOLBARMENU_MENU_ENTRIES = registerString("toolbarmenu.menu_entries",
      "Menu Entries");

  /*
   * Register a translatable string
   */
  public static String registerString(String key, String value) {
    strings.put(key, value);
    return key;
  }

  /*
   * Return the i18nized version of a String
   */
  public static String getString(String id) {
    if (stringBundle == null) {
      GameModule.getGameModule().getGlobalPrefs().addOption(null, new DirectoryConfigurer(Documentation.DOCS_DIR, null));
      stringBundle = ResourceBundle.getBundle("VASSAL.i18n.VASSAL", Locale.getDefault(), new VassalPropertyClassLoader());
    }
    String s = null;
    
    // 1. Try Translating
    s = stringBundle.getString(id);    
    
    // 2. Try default string
    if (s == null) {
      s = (String) strings.get(id);
    }    

    // 3. Worst case, return the key    
    if (s == null) {
      s = id;
    }
    
    return s;
  }

  /*
   * Write the base VASSAL.properties file to disk
   */
  public static void writeProperties() {
    File dir = (File) GameModule.getGameModule().getGlobalPrefs().getValue(Documentation.DOCS_DIR);
    FileOutputStream out = null;
    Map sorted = new TreeMap(strings);
    try {
      out = new FileOutputStream(new File(dir, "VASSAL.properties"));
      String name;
      String value;
      for (Iterator i = sorted.keySet().iterator(); i.hasNext(); ) {
        name = (String) i.next();
        value = (String) strings.get(name);
        I18nSupport.writeProperty(out, name, value);
      }
      out.close();
    }
    catch (Exception e) {
      return;
    }
  }
  
  /**
   * Custom Class Loader for loading VASSAL property files.
   * Check first for files in the VASSAL install (documentation) directory
   *
   */
  static class VassalPropertyClassLoader extends ClassLoader {
    public URL getResource(String name) {
      URL url = null;
      String propFileName = name.substring(name.lastIndexOf('/')+1);
      File propFile = new File(Documentation.getDocumentationBaseDir(), propFileName);
 
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