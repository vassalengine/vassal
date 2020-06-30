/*
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

import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;

import VASSAL.Info;
import VASSAL.build.module.WizardSupport;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.DirectoryConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ReadErrorDialog;
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

  private Map<String, Configurer> options = new HashMap<>();
  private Properties storedValues = new Properties();
  private PrefsEditor editor;
  private File file;

  public Prefs(PrefsEditor editor, String name) {
    this(editor, new File(Info.getPrefsDir(), sanitize(name)));
  }

  protected Prefs(PrefsEditor editor, File file) {
    this.editor = editor;
    this.file = file;

    read();

    for (String key : storedValues.stringPropertyNames()) {
      final String value = storedValues.getProperty(key);
      final Configurer c = options.get(key);
      if (c != null) {
        c.setValue(value);
      }
    }

    editor.addPrefs(this);
  }

  public PrefsEditor getEditor() {
    return editor;
  }

  public File getFile() {
    return file;
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

  public static String sanitize(String str) {
    /*
      Java gives us no way of checking whether a string is a valid
      filename on the filesystem we're using. Filenames matching
      [0-9A-Za-z_]+ are safe pretty much everywhere. Any code point
      in [0-9A-Za-z] is passed through; every other code point c is
      escaped as "_hex(c)_". This mapping is a surjection and will
      produce filenames safe on every sane filesystem, so long as the
      input strings are not too long.
    */
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < str.length(); ++i) {
      final int cp = str.codePointAt(i);
      if (('0' <= cp && cp <= '9') ||
          ('A' <= cp && cp <= 'Z') ||
          ('a' <= cp && cp <= 'z')) {
        sb.append((char) cp);
      }
      else {
        sb.append('_')
          .append(Integer.toHexString(cp).toUpperCase())
          .append('_');
      }
    }

    return sb.toString();
  }

  protected void read() {
    try (InputStream fin = new FileInputStream(file);
         InputStream in = new BufferedInputStream(fin)) {
      storedValues.clear();
      storedValues.load(in);
    }
    catch (FileNotFoundException e) {
      // First time for this module, not an error.
    }
    catch (IOException e) {
      ReadErrorDialog.errorNoI18N(e, file);
    }
  }

  /**
   * Store this set of preferences
   */
  public void save() throws IOException {
    storedValues.clear();

    // ensure that the prefs dir exists
    if (!Info.getPrefsDir().exists()) {
      FileUtils.forceMkdir(Info.getPrefsDir());
    }

    try (RandomAccessFile raf = new RandomAccessFile(file, "rw")) {
      final FileChannel ch = raf.getChannel();

      // lock the prefs file
      final FileLock lock = ch.lock();

      // read the old key-value pairs
      final InputStream in = Channels.newInputStream(ch);
      storedValues.load(in);

      // merge in the current key-value pairs
      for (Configurer c : options.values()) {
        final String val = c.getValueString();
        if (val != null) {
          storedValues.put(c.getKey(), val);
        }
      }

      // write back the key-value pairs
      ch.truncate(0);
      ch.position(0);
      final OutputStream out = Channels.newOutputStream(ch);
      storedValues.store(out, null);
      out.flush();
    }
    // channel and streams closed, lock released
  }

  /** Save these preferences and write to disk. */
  public void write() throws IOException {
    save();
  }

  @Override
  public void close() throws IOException {
    save();
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
      final PrefsEditor ed = new PrefsEditor();
      // The underscore prevents collisions with module prefs
      globalPrefs = new Prefs(ed, new File(Info.getPrefsDir(), "V_Global"));

      final DirectoryConfigurer c =
        new DirectoryConfigurer(MODULES_DIR_KEY, null);
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
    if (SystemUtils.IS_OS_WINDOWS) {
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
