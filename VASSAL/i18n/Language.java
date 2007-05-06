/*
 * $Id: Language.java 1856 2007-03-09 04:30:49 +0000 (Fri, 09 Mar 2007) rodneykinney $
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

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.Set;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.SingleChildInstance;

/**
 * Container for definitions of Translations.
 */
public class Language extends AbstractConfigurable {
  public static final String LANGUAGE = "language"; //$NON-NLS-1$
  protected static String moduleBundle;
  protected static String languageBundle;
  protected static String countryBundle;
  protected static ArrayList moduleTranslations = new ArrayList();
  protected static ArrayList languageTranslations = new ArrayList();
  protected static ArrayList countryTranslations = new ArrayList();
  protected static ArrayList translations = new ArrayList();
  protected static Language instance;
  /*
   * Master translation property list
   */
  protected static VassalResourceBundle masterBundle;

  public static Language getInstance() {
    return instance;
  }

  /**
   * Return a list of translations available for editing.
   * 
   * @return Array of available translations
   */
  public static String[] getTranslationList() {
    Collections.sort(translations);
    String[] s = new String[translations.size()];
    int idx = 0;
    for (Iterator i = translations.iterator(); i.hasNext();) {
      s[idx++] = ((Translation) i.next()).getDescription();
    }
    return s;
  }

  /**
   * Return a specified translation
   * 
   * @param description
   * @return Translation object
   */
  public static Translation getTranslation(String description) {
    for (Iterator i = translations.iterator(); i.hasNext();) {
      Translation t = (Translation) i.next();
      if (t.getDescription().equals(description)) {
        return t;
      }
    }
    return null;
  }
  /*
   * Record attributes as the module is being built for later translation
   */
  protected static Set<TranslatableAttribute> translatableItems = new HashSet<TranslatableAttribute>();

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
  public static void saveTranslatableAttribute(Translatable component, String name, String value) {
    if (GameModule.getGameModule().isLocalizationEnabled()) {
      TranslatableAttribute ta = new TranslatableAttribute(component, name, value);
      translatableItems.add(ta);
    }
  }

  /**
   * Translate the module. The module and all extensions have now been built, so all Translations are available and all
   * attributes that need to be translated have been recorded. There may be multiple translations that match this
   * Locale, merge them in order - Country over-rides Language over-rides default. NB - You cannot create a default
   * translation (Module.properties) using the VASSAL editor, but a default file can be placed into a module or
   * extension manually.
   * 
   * @throws IOException
   * 
   */
  public static void translate() throws IOException {
    if (GameModule.getGameModule().isLocalizationEnabled()) {
      for (Iterator i = moduleTranslations.iterator(); i.hasNext();) {
        addBundle(((Translation) i.next()).getBundle());
      }
      for (Iterator i = languageTranslations.iterator(); i.hasNext();) {
        addBundle(((Translation) i.next()).getBundle());
      }
      for (Iterator i = countryTranslations.iterator(); i.hasNext();) {
        addBundle(((Translation) i.next()).getBundle());
      }
      if (masterBundle != null) {
        setTranslationInProgress(true);
        for (TranslatableAttribute attr : translatableItems) {
          if (attr.isTranslatable()) {
            String key = attr.getKey();
            try {
              String translation = masterBundle.getString(key);
              attr.applyTranslation(translation);
            }
            catch (MissingResourceException e) {
              // Assume that the translated text is the same as the original
            }
          }
        }
        setTranslationInProgress(false);
        setTranslationComplete(true);
      }
      translatableItems.clear();
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
  public static String translate(String key, String defaultValue) {
    try {
      return masterBundle.getString(key);
    }
    catch (MissingResourceException e) {
      return defaultValue;
    }
  }

  protected static void addBundle(VassalResourceBundle child) {
    if (masterBundle == null) {
      masterBundle = child;
    }
    else {
      child.setParent(masterBundle);
      masterBundle = child;
    }
  }
  protected static boolean translationInProgress = false;
  protected static boolean translationComplete = false;

  public static void setTranslationInProgress(boolean b) {
    translationInProgress = b;
  }

  public static boolean isTranslationInProgress() {
    return translationInProgress;
  }

  public static void setTranslationComplete(boolean b) {
    translationComplete = b;
  }

  public static boolean isTranslationComplete() {
    return translationComplete;
  }

  /*
   * -------------------------------------------------------- End Static Section
   */
  public Language() {
    moduleBundle = Resources.MODULE_BUNDLE;
    languageBundle = moduleBundle + "_" + Locale.getDefault().getLanguage(); //$NON-NLS-1$
    countryBundle = languageBundle + "_" + Locale.getDefault().getCountry(); //$NON-NLS-1$
    moduleBundle += ".properties"; //$NON-NLS-1$
    languageBundle += ".properties"; //$NON-NLS-1$
    countryBundle += ".properties"; //$NON-NLS-1$
    if (instance == null) {
      instance = this;
    }
  }

  /**
   * Called whenever a Translation is added to a module or extension. Check if the translation macthes our locale. If
   * so, add it to the list of translations to use. There may multiple matching translations at Country, Language and
   * Module level from different extensions.
   * 
   * @param t
   *          Translation
   */
  public void addTranslation(Translation t) {
    /*
     * Play and Translate mode - keep a record of all translations that match our locale from various extensions. These
     * will be merged into one after all are loaded.
     */
    if (GameModule.getGameModule().isLocalizationEnabled()) {
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
     * Edit mode, keep a list of all translations available in this Module or Extension to use in drop-down lists
     */
    else {
      translations.add(t);
    }
  }

  public void removeTranslation(Translation t) {
    translations.remove(t);
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class[] getAttributeTypes() {
    return new Class[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void setAttribute(String key, Object value) {
  }

  protected String getDescription() {
    return "";
  }

  public void addTo(Buildable parent) {
    validator = new SingleChildInstance(GameModule.getGameModule(), getClass());
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{Translation.class};
  }

  public static String getConfigureTypeName() {
    return "Translations";
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {
  }
}
