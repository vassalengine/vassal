/*
 * $Id: Resources.java 1856 2007-03-09 04:30:49 +0000 (Fri, 09 Mar 2007) rodneykinney $
 *
 * Copyright (c) 2007 by Rodney Kinney, Brent Easton
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

import java.io.File;
import java.io.FileInputStream;
import java.net.URL;
import java.text.MessageFormat;
import java.util.Date;
import java.util.Enumeration;
import java.util.Locale;
import java.util.ResourceBundle;

import javax.swing.UIManager;

import VASSAL.build.GameModule;

public class Resources {

  /*
   * Translation of VASSAL is handled by standard Java I18N tools.
   *  
   *   vassalBundle - Resource Bundle for the VASSAL player interface
   *   editorBundle - Resource Bundle for the Module Editor
   *   
   *  These are implemented as PropertyResourceBundles, normally to be
   *  found in the VASSAL jar file. VASSAL will search first in the VASSAL
   *  install directory for bundles, then follow the standard Java Class Path
   */
  protected static ResourceBundle vassalBundle;
  protected static ResourceBundle editorBundle;
  
  protected static String VASSAL_BUNDLE = "VASSAL.i18n.VASSAL"; //$NON-NLS-1$
  protected static String EDITOR_BUNDLE = "VASSAL.i18n.Editor"; //$NON-NLS-1$

  public static Enumeration getVassalKeys() {
    return vassalBundle.getKeys();
  }
  
  public static Enumeration getEditorKeys() {
    return editorBundle.getKeys();
  }
  /*
   * Translation of individual modules is handled differently. There may be 
   * multiple Module.properties file active - Potentially one in the module
   * plus one in each Extension loaded. These will be read into UberProperties 
   * structures with each file loaded supplying defaults for subsequent files.
   */
  protected static String MODULE_BUNDLE = "Module"; //$NON-NLS-1$
 
  /*
   * Commonly used i18n keys used in multiple components. By defining them
   * centrally, they will only have to be translated once. Reference to these
   * string should be made as follows:
   * 
   *    Resources.getString(Resources.VASSAL)
   */
  public static final String VASSAL = "General.VASSAL"; //$NON-NLS-1$
  public static final String ADD = "General.add"; //$NON-NLS-1$
  public static final String REMOVE = "General.remove"; //$NON-NLS-1$
  public static final String INSERT = "General.insert"; //$NON-NLS-1$
  public static final String YES = "General.yes"; //$NON-NLS-1$
  public static final String NO = "General.no"; //$NON-NLS-1$
  public static final String CANCEL = "General.cancel"; //$NON-NLS-1$
  public static final String SAVE = "General.save"; //$NON-NLS-1$
  public static final String OK = "General.ok"; //$NON-NLS-1$
  public static final String MENU = "General.menu"; //$NON-NLS-1$
  public static final String LOAD = "General.load"; //$NON-NLS-1$
  public static final String QUIT = "General.quit"; //$NON-NLS-1$
  public static final String EDIT = "General.edit"; //$NON-NLS-1$
  public static final String NEW = "General.new"; //$NON-NLS-1$
  public static final String FILE = "General.file"; //$NON-NLS-1$
  public static final String HELP = "General.help"; //$NON-NLS-1$
  public static final String CLOSE = "General.close"; //$NON-NLS-1$
  public static final String DATE_DISPLAY = "General.date_display"; //$NON-NLS-1$
  public static final String NEXT = "General.next"; //$NON-NLS-1$
  public static final String REFRESH = "General.refresh"; //$NON-NLS-1$
  
  /*
   * All i18n keys for the Module Editor must commence with "Editor.", This
   * allows us to use a single Resources.getString() call for both resource 
   * bundles.
   */
  public static final String EDITOR_PREFIX = "Editor."; //$NON-NLS-1$
  
  /*
   * Common Editor labels that appear in many components.
   */
  public static final String BUTTON_TEXT = "Editor.button_text_label"; //$NON-NLS-1$
  public static final String TOOLTIP_TEXT = "Editor.tooltip_text_label"; //$NON-NLS-1$
  public static final String BUTTON_ICON = "Editor.button_icon_label"; //$NON-NLS-1$
  public static final String HOTKEY_LABEL = "Editor.hotkey_label"; //$NON-NLS-1$
  public static final String COLOR_LABEL = "Editor.color_label"; //$NON-NLS-1$
  public static final String NAME_LABEL = "Editor.name_label"; //$NON-NLS-1$

  /*
   * init() is called by Main() very early in the VASSAL initialization sequence
   * to supply the name of the VASSAL install directory before any strings have 
   * been translated. This allows us to find a possible over-riding properties
   * file in the home directory.
   * 
   * Also update the UIManagaer with translations of Yes/No/Cancel/Ok for the
   * JOptionPane buttons.
   */
  public static File homeDir; 
  public static void init(File dir) {
    homeDir = dir;
  }
  
  static {
    UIManager.put("OptionPane.yesButtonText", getString(YES)); //$NON-NLS-1$
    UIManager.put("OptionPane.cancelButtonText", getString(CANCEL)); //$NON-NLS-1$
    UIManager.put("OptionPane.noButtonText", getString(NO)); //$NON-NLS-1$
    UIManager.put("OptionPane.okButtonText", getString(OK)); //$NON-NLS-1$
  }
   
  /**
   * Localize a user interface String. 
   *
   * @param id String Id
   * @return Localized result
   */
  public static String getString(String id) {
    if (id.startsWith(EDITOR_PREFIX)) {
      return getEditorString(id);
    }
    else {
      return getVassalString(id);
    }
  }
  
  /**
   * Localize a VASSAL user interface string
   * @param id String id
   * @return Localized result
   */
  public static String getVassalString(String id) {
    if (vassalBundle == null) {
      vassalBundle = ResourceBundle.getBundle(VASSAL_BUNDLE, Locale.getDefault(), homeDir == null ? Resources.class.getClassLoader() : new VassalPropertyClassLoader());
    }
    return getString(vassalBundle, id);
  }
  
  /**
   * Localize a VASSAL Module Editor String
   * @param id String Id
   * @return Localized Result
   */
  public static String getEditorString(String id) {
    if (editorBundle == null) {
      editorBundle = ResourceBundle.getBundle(EDITOR_BUNDLE, Locale.getDefault(), homeDir == null ? Resources.class.getClassLoader() : new VassalPropertyClassLoader());
   }
   return getString(editorBundle, id);
  }
  
  /**
   * Localize a string using the supplied resource bundle
   * 
   * @param bundle Resource bundle
   * @param id String Id
   * @return Localized result
   */
  public static String getString(ResourceBundle bundle, String id) {
  
    String s = null;
    
    try {
      s = bundle.getString(id);
    }
    catch (Exception ex) {
      System.err.println("No Translation: " + id);
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
  public static MessageFormat formatter = new MessageFormat(""); //$NON-NLS-1$
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
   * @author Brent Easton
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
        url = new URL("file", "", propFile.getCanonicalPath()); //$NON-NLS-1$ //$NON-NLS-2$
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