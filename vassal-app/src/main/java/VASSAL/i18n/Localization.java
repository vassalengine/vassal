/*
 * Copyright (c) 2000-2007 by Rodney Kinney
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

import VASSAL.build.GameModule;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.MissingResourceException;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Singleton class for managing the translation of a module into other languages
 * @author rodneykinney
 *
 */
public class Localization extends Language {
  private static final Logger logger =
    LoggerFactory.getLogger(Localization.class);

  private static Localization instance;

  private Localization() {
    moduleBundle = Resources.MODULE_BUNDLE;
    languageBundle = moduleBundle + "_" + Resources.getLocale().getLanguage(); //$NON-NLS-1$
    countryBundle = languageBundle + "_" + Resources.getLocale().getCountry(); //$NON-NLS-1$
    moduleBundle += ".properties"; //$NON-NLS-1$
    languageBundle += ".properties"; //$NON-NLS-1$
    countryBundle += ".properties"; //$NON-NLS-1$
  }

  public static Localization getInstance() {
    if (instance == null) {
      instance = new Localization();
    }
    return instance;
  }
  protected String moduleBundle;
  protected String languageBundle;
  protected String countryBundle;
  protected List<Translation> moduleTranslations = new ArrayList<>();
  protected List<Translation> languageTranslations = new ArrayList<>();
  protected List<Translation> countryTranslations = new ArrayList<>();
  protected List<Translation> translations = new ArrayList<>();

  /*
   * Master translation property list
   */
  protected VassalResourceBundle masterBundle;

  /**
   * Return a list of translations available for editing.
   *
   * @return Array of available translations
   */
  public String[] getTranslationList() {
    Collections.sort(translations);
    final String[] s = new String[translations.size()];
    int idx = 0;
    for (final Translation t : translations) {
      s[idx++] = t.getDescription();
    }
    return s;
  }

  /**
   * Return a specified translation
   *
   * @param description description
   * @return Translation object
   */
  public Translation getTranslation(String description) {
    for (final Translation t : translations) {
      if (t.getDescription().equals(description)) {
        return t;
      }
    }
    return null;
  }

  /**
   * Record attributes as the module is being built for later translation
   */
  protected Set<TranslatableAttribute> translatableItems = new HashSet<>();

  /**
   * Record an attribute that may need to be translated.
   *
   * @param component
   *          component to be translated
   * @param name
   *          Attribute name to be translated
   * @param value
   *          current value of attribute
   */
  public void saveTranslatableAttribute(Translatable component, String name, String value) {
    if (GameModule.getGameModule().isLocalizationEnabled()) {
      final TranslatableAttribute ta = new TranslatableAttribute(component, name, value);
      translatableItems.add(ta);
    }
  }

  /**
   * Translate the module. The module and all extensions have now been built,
   * so all Translations are available and all attributes that need to be
   * translated have been recorded. There may be multiple translations that
   * match this Locale, merge them in order - Country over-rides Language
   * over-rides default. NB - You cannot create a default translation
   * (Module.properties) using the VASSAL editor, but a default file can be
   * placed into a module or extension manually.
   *
   * @throws IOException oops
   */
  public void translate() throws IOException {
    if (GameModule.getGameModule().isLocalizationEnabled()) {
      for (final Translation t : moduleTranslations) {
        addBundle(t.getBundle());
      }
      for (final Translation t : languageTranslations) {
        addBundle(t.getBundle());
      }
      for (final Translation t : countryTranslations) {
        addBundle(t.getBundle());
      }
      if (masterBundle != null) {
        translationInProgress = true;
        for (final TranslatableAttribute attr : translatableItems) {
          if (attr.isTranslatable()) {
            final String key = attr.getKey();
            try {
              final String translation = masterBundle.getString(key);
              attr.applyTranslation(translation);
            }
            catch (MissingResourceException e) {
              // Assume that the translated text is the same as the original
            }
          }
        }
        translationInProgress = false;
        translationComplete = true;
        logger.info("Translated"); //NON-NLS
      }
      translatableItems.clear();
      GameModule.getGameModule().initFrameTitle();
    }
  }

  /**
   * Translate an individual attribute.
   *
   * @param key
   *          Attribute Key
   * @param defaultValue
   *          Default value if no translation available
   * @return translation
   */
  public String translate(String key, String defaultValue) {
    try {
      return masterBundle == null ? defaultValue : masterBundle.getString(key);
    }
    catch (MissingResourceException e) {
      return defaultValue;
    }
  }

  protected void addBundle(VassalResourceBundle child) {
    if (masterBundle != null) {
      child.setParent(masterBundle);
    }
    masterBundle = child;
  }
  protected boolean translationInProgress = false;
  protected boolean translationComplete = false;

  public boolean isTranslationInProgress() {
    return translationInProgress;
  }

  public boolean isTranslationComplete() {
    return translationComplete;
  }

  /**
   * Called whenever a Translation is added to a module or extension.
   * Check if the translation matches our locale. If so, add it to the list
   * of translations to use. There may multiple matching translations at
   * Country, Language and Module level from different extensions.
   *
   * @param t Translation
   */
  public void addTranslation(Translation t) {
    /*
     * Play and Translate mode - keep a record of all translations that
     * match our locale from various extensions. These will be merged
     * into one after all are loaded.
     */
    if (GameModule.getGameModule().isLocalizationEnabled()) {
      Resources.addSupportedLocale(t.getLocale());
      if (moduleBundle.equals(t.getBundleFileName())) {
        moduleTranslations.add(t);
      }
      else if (languageBundle.equals(t.getBundleFileName())) {
        languageTranslations.add(t);
      }
      else if (countryBundle.equals(t.getBundleFileName())) {
        countryTranslations.add(t);
      }
    }
    /*
     * Edit mode, keep a list of all translations available in this
     * Module or Extension to use in drop-down lists
     */
    else {
      translations.add(t);
    }
  }

  public void removeTranslation(Translation t) {
    translations.remove(t);
  }

}

