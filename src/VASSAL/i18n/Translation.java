/*
 * $Id$
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

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Locale;
import java.util.Properties;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.ReadErrorDialog;
import VASSAL.tools.io.IOUtils;

public class Translation extends AbstractConfigurable
                         implements Comparable<Translation> {
  public static final String LOCALE = "locale"; //$NON-NLS-1$
  protected Locale locale;
  protected boolean dirty = false;
  protected Properties localProperties;

  public Translation() {
    locale = new Locale(Locale.getDefault().getLanguage());
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Locale:  "};
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      LocalePrompt.class
    };
  }

  public static class LocalePrompt implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new LocaleConfigurer(key, name, "");
    }
  }

  public String[] getAttributeNames() {
    return new String[]{LOCALE};
  }

  public String getAttributeValueString(String key) {
    if (LOCALE.equals(key)) {
      return LocaleConfigurer.localeToString(locale);
    }
    else
      return null;
  }

  public void setAttribute(String key, Object value) {
    if (LOCALE.equals(key)) {
      locale = LocaleConfigurer.stringToLocale((String) value);
      setConfigureName(locale.getDisplayName());
    }
  }

  protected String getDescription() {
    return locale.getDisplayName(Locale.getDefault());
  }

  public String getLanguageCode() {
    return locale.getLanguage();
  }

  public String getConfigureName() {
    return getDescription();
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
    Localization.getInstance().removeTranslation(this);
  }

  public void addTo(Buildable parent) {
    Localization.getInstance().addTranslation(this);
    if (!GameModule.getGameModule().isLocalizationEnabled()) {
      try {
        loadProperties();
      }
// FIXME: review error message
// FIXME: should we catch a FileNotFoundException here instead?
      catch (IOException e) {
        // Fail quietly: This error will occur when adding a new translation.
      }
    }
  }

  /**
   * Set a property into our property map. i.e. an attribute has been translated
   *
   * @param key
   *          property key
   * @param value
   *          property value
   */
  public void setProperty(String key, String value) {
    if (value == null || value.length() == 0) {
      getProperties().remove(key);
    }
    else {
      getProperties().setProperty(key, value);
    }
    dirty = true;
  }

  /**
   * Return true if this translation has unsaved modifications
   *
   * @return true if undaved changes
   */
  public boolean isDirty() {
    return dirty;
  }

  /**
   * Return the translation for the supplied key
   *
   * @param s
   * @return
   */
  public String translate(String key) {
    return getProperties().getProperty(key);
  }

  /**
   * Load properties from the bundle file in the module/extension
   * @throws IOException
   *
   */
  protected void loadProperties() throws IOException {
    if (localProperties == null) {
      localProperties = new Properties();
    }

    if (GameModule.getGameModule() != null) {
      BufferedInputStream in = null;
      try {
        try {
          in = new BufferedInputStream(
            GameModule.getGameModule()
                      .getDataArchive()
                      .getInputStream(getBundleFileName())
          );
        }
        catch (FileNotFoundException e) {
          // ignore, properties have not been saved yet
          dirty = false;
          return;
        }

        localProperties.load(in);
        in.close();
      }
      finally {
        IOUtils.closeQuietly(in);
      }
    }

    dirty = false;
  }

  protected VassalResourceBundle getBundle() throws IOException {
    BufferedInputStream in = null;
    try {
      in = new BufferedInputStream(
        GameModule.getGameModule()
                  .getDataArchive()
                  .getInputStream(getBundleFileName())
      );

      final VassalResourceBundle b = new VassalResourceBundle(in);
      in.close();
      return b;
    }
    finally {
      IOUtils.closeQuietly(in);
    }
  }

  /**
   * Reload the properties from the module/extension
   * @throws IOException
   *
   */
  public void reloadProperties() throws IOException {
    localProperties = new Properties();
    loadProperties();
  }

  /**
   * Save the properties back to the module/extension
   * @throws IOException
   *
   */
  protected void saveProperties() throws IOException {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    getProperties().store(out, "Module translation");

    final ArchiveWriter writer = GameModule.getGameModule().getArchiveWriter();
    if (writer != null) {
      writer.addFile(getBundleFileName(), out.toByteArray());
      dirty = false;
    }
  }

  /**
   * Return the properties map for this translation. Create and load from
   * the module if necessary.
   *
   * @return properties
   */
  public Properties getProperties() {
    if (localProperties == null) {
      try {
        loadProperties();
      }
      catch (IOException e) {
        ReadErrorDialog.error(e, getBundleFileName());
        localProperties = new Properties();
      }
    }
    return localProperties;
  }

  /**
   * Build the bundle name
   *
   * @return bundle name
   */
  public String getBundleName() {
    return Resources.MODULE_BUNDLE + "_" + locale.getLanguage() //$NON-NLS-1$
        + (locale.getCountry().length() > 0 ? ("_" + locale.getCountry()) : ""); //$NON-NLS-1$ //$NON-NLS-2$
  }

  /**
   * Build the bundle file name
   *
   * @return bundle file name
   */
  public String getBundleFileName() {
    return getBundleName() + ".properties"; //$NON-NLS-1$
  }

  public int compareTo(Translation o) {
    return getDescription().compareTo(o.getDescription());
  }

  public Locale getLocale() {
    return locale;
  }
}
