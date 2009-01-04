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

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import VASSAL.Info;
import VASSAL.build.module.WizardSupport;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.WriteErrorDialog;
import VASSAL.tools.io.IOUtils;

/**
 * A set of preferences. Each set of preferences is identified by a name, and different sets may share a common editor,
 * which is responsible for writing the preferences to disk
 */
public class Prefs implements Closeable {
  /** Preferences key for the directory containing modules */
  public static final String MODULES_DIR_KEY = "modulesDir"; // $NON_NLS-1$
  public static final String DISABLE_D3D = "disableD3d";
  private static Prefs globalPrefs;
  private Map<String, Configurer> options = new HashMap<String, Configurer>();
  private Properties storedValues = new Properties();
  private PrefsEditor editor;
  private String name;
  private Set<String> changed = new HashSet<String>();
  private PropertyChangeListener changeListener;

  public Prefs(PrefsEditor editor, String name) {
    this.editor = editor;
    this.name = name;
    this.changeListener = new PropertyChangeListener() {
      public void propertyChange(PropertyChangeEvent evt) {
        changed.add(evt.getPropertyName());
      }
    };
    editor.addPrefs(this);
    init(name);
  }

  public PrefsEditor getEditor() {
    return editor;
  }

  public File getFile() {
    return new File(editor.getArchive().getName());
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
   * @param category
   *          the tab under which to add the Configurer's controls in the editor window. If null, do not add controls.
   * 
   * @param prompt
   *          If non-null and the value was not read from the preferences file on initialization (i.e. first-time
   *          setup), prompt the user for an initial value
   */
  public void addOption(String category, Configurer o, String prompt) {
    if (o != null && options.get(o.getKey()) == null) {
      options.put(o.getKey(), o);
      final String val = storedValues.getProperty(o.getKey());
      if (val != null) {
        o.setValue(val);
        prompt = null;
      }
      if (category != null && o.getControls() != null) {
        editor.addOption(category, o, prompt);
      }
    }
    o.addPropertyChangeListener(changeListener);
  }

  public void setValue(String option, Object value) {
    options.get(option).setValue(value);
  }

  public Configurer getOption(String s) {
    return options.get(s);
  }

  /**
   * @param key
   * @return the value of the preferences setting stored under key
   */
  public Object getValue(String key) {
    final Configurer c = options.get(key);
    return c == null ? null : c.getValue();
  }

  /**
   * Return the value of a given preference.
   * 
   * @param key
   *          the name of the preference to retrieve
   * @return the value of this option read from the Preferences file at startup, or <code>null</code> if no value is
   *         undefined
   */
  public String getStoredValue(String key) {
    return storedValues.getProperty(key);
  }

  public void init(String moduleName) {
    name = moduleName;
    read();
    editor.getArchive().closeWhenNotInUse();
    // FIXME: Use stringPropertyNames() in 1.6+
    // for (String key : storedValues.stringPropertyNames()) {
    for (Enumeration<?> e = storedValues.keys(); e.hasMoreElements();) {
      final String key = (String) e.nextElement();
      final String value = storedValues.getProperty(key);
      final Configurer c = options.get(key);
      if (c != null) {
        c.setValue(value);
      }
    }
  }

  private void read() {
    BufferedInputStream in = null;
    try {
      in = new BufferedInputStream(editor.getArchive().getInputStream(name));
      storedValues.clear();
      storedValues.load(in);
      in.close();
    }
    catch (FileNotFoundException e) {
      // First time for this module, not an error.
    }
    catch (IOException e) {
      ReadErrorDialog.error(e, editor.getArchive().getName());
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  /**
   * Store this set of preferences in the editor, but don't yet save to disk
   */
  public void save() throws IOException {
    editor.getArchive().uncacheFile(name);
    read();
    for (Configurer c : options.values()) {
      if (changed.contains(c.getKey())) {
        final String val = c.getValueString();
        if (val != null) {
          storedValues.put(c.getKey(), val);
        }
        else {
          storedValues.remove(c.getKey());
        }
      }
    }
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    storedValues.store(out, null);
    editor.getArchive().addFile(name, new ByteArrayInputStream(out.toByteArray()));
    changed.clear();
  }

  /** Save these preferences and write to disk. */
  public void write() throws IOException {
    editor.write();
  }

  public void close() throws IOException {
    editor.close();
    if (this == globalPrefs) {
      globalPrefs = null;
    }
  }

  /**
   * A global set of preferences that exists independent of any individual module.
   * 
   * @return the global <code>Prefs</code> object
   */
  public static Prefs getGlobalPrefs() {
    if (globalPrefs == null) {
      final File prefsFile = new File(Info.getHomeDir(), "Preferences"); //$NON-NLS-1$
      try {
        ArchiveWriter.ensureExists(prefsFile, "VASSAL");
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, prefsFile);
      }
      globalPrefs = new Prefs(new PrefsEditor(new ArchiveWriter(prefsFile.getPath())), "VASSAL"); //$NON-NLS-1$
      try {
        globalPrefs.write();
      }
      catch (IOException e) {
        WriteErrorDialog.error(e, prefsFile);
      }
      final DirectoryConfigurer c = new DirectoryConfigurer(MODULES_DIR_KEY, null);
      c.setValue(new File(System.getProperty("user.home")));
      globalPrefs.addOption(null, c);
    }
    return globalPrefs;
  }
  
  /**
   * Initialize visible Global Preferences that are shared between the
   * Module Manager and the Editor/Player.
   * 
   */
  public static void initSharedGlobalPrefs() {
    getGlobalPrefs();
    
    // Option to disable D3D pipeline
    if (Info.isWindows()) {
      final BooleanConfigurer d3dConf = new BooleanConfigurer(
        DISABLE_D3D,
        Resources.getString("Prefs.disable_d3d"),
        Boolean.FALSE
      );
      globalPrefs.addOption(d3dConf);
    }
    
    final BooleanConfigurer wizardConf = new BooleanConfigurer(
      WizardSupport.WELCOME_WIZARD_KEY,
      Resources.getString("WizardSupport.ShowWizard"),
      Boolean.TRUE
    );

    globalPrefs.addOption(wizardConf);
  }
}
