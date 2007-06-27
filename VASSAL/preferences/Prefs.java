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
package VASSAL.preferences;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import VASSAL.Info;
import VASSAL.configure.Configurer;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;

/**
 * A set of preferences.  Each set of preferences is identified by a name, and
 * different sets may share a common editor, which is responsible for
 * writing the preferences to disk
 */
public class Prefs {
  /** Preferences key for the directory containing modules */
  public static final String MODULES_DIR_KEY = "modulesDir"; //$NON_NLS-1$
  private static Prefs globalPrefs;
  private Map<String,Configurer> options = new HashMap<String,Configurer>();
  private Properties storedValues = new Properties();
  private PrefsEditor editor;
  private String name;

  public Prefs(PrefsEditor editor, String name) {
    this.editor = editor;
    this.name = name;
    editor.addPrefs(this);
    init(name);
  }

  public PrefsEditor getEditor() {
    return editor;
  }

  public void addOption(Configurer o) {
    addOption(Resources.getString("Prefs.general_tab"), o); //$NON-NLS-1$
  }

  public void addOption(String category, Configurer o) {
    addOption(category, o, null);
  }

  /**
   * Add a configurable property to the preferences in the given category
   *
   * @param category the tab under which to add the Configurer's controls
   * in the editor window.  If null, do not add controls.
   *
   * @param prompt If non-null and the value was not read from the
   * preferences file on initialization (i.e. first-time setup),
   * prompt the user for an initial value
   */
  public void addOption(String category, Configurer o, String prompt) {
    if (o != null && options.get(o.getKey()) == null) {
      options.put(o.getKey(), o);
      String val = storedValues.getProperty(o.getKey());
      if (val != null) {
        o.setValue(val);
        prompt = null;
      }
      if (category != null && o.getControls() != null) {
        editor.addOption(category, o, prompt);
      }
    }
  }

  public void setValue(String option, Object value) {
    Configurer c = options.get(option);
    c.setValue(value);
  }

  public Configurer getOption(String s) {
    return options.get(s);
  }

  /**
   *
   * @param key
   * @return the value of the preferences setting stored under key
   */
  public Object getValue(String key) {
    Configurer c = options.get(key);
    return c == null ? null : c.getValue();
  }

  /**
   *
   * @param key the name of a Preferences option
   * @return the value for the option that was read from the Preferences file at startup
   */
  public String getStoredValue(String key) {
    return storedValues.getProperty(key);
  }

  public void init(String moduleName) {
    name = moduleName;
    try {
      InputStream in = editor.getArchive().getFileStream(name);
      storedValues.clear();
      storedValues.load(in);
      for (Enumeration e = storedValues.keys(); e.hasMoreElements();) {
        String key = (String) e.nextElement();
        String value = storedValues.getProperty(key);
        Configurer c = options.get(key);
        if (c != null) {
          c.setValue(value);
        }
      }
    }
    catch (java.io.IOException e) {
    }
  }

  /**
   * Store this set of preferences in the editor, but don't yet save to disk
   */
  public void save() throws IOException {
    for (Configurer c : options.values()) {
      String val = c.getValueString();
      if (val != null) {
        storedValues.put(c.getKey(), val);
      }
      else {
        storedValues.remove(c.getKey());
      }
    }
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    storedValues.store(out, null);
    editor.getArchive()
          .addFile(name, new java.io.ByteArrayInputStream(out.toByteArray()));
  }

  /** Save these preferences and write to disk */
  public void write() throws IOException {
    editor.write();
  }
  
  /**
   * A global set of preferences that exists independent of any individual module
   * @return
   */
  public static Prefs getGlobalPrefs() {
    if (globalPrefs == null) {
      File prefsFile = new File(Info.getHomeDir(), "Preferences");  //$NON-NLS-1$
      globalPrefs = new Prefs(new PrefsEditor(new ArchiveWriter(prefsFile.getPath())), "VASSAL");  //$NON-NLS-1$
      DirectoryConfigurer c = new DirectoryConfigurer(MODULES_DIR_KEY, null);
      c.setValue(new File(System.getProperty("user.home")));
      globalPrefs.addOption(null,c);
    }
    return globalPrefs;
  }
}
