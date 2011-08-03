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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.swing.Box;
import javax.swing.JComboBox;
import javax.swing.JLabel;

import VASSAL.configure.Configurer;
import VASSAL.tools.SequenceEncoder;

/**
 * Configure a Locale Value using full, localized Language and Country names
 *
 * @author Brent Easton
 *
 */
public class LocaleConfigurer extends Configurer {

  protected static final String ANY_COUNTRY = "<Any Country>";
  protected Box panel;
  protected static Map<String,String> languages = new HashMap<String,String>();
  protected static String[] languageList;
  protected static Map<String,String> countries = new HashMap<String,String>();
  protected static String[] countryList;

  protected JComboBox langBox;
  protected JComboBox countryBox;

  public LocaleConfigurer(String key, String name) {
    this(key, name, "");
  }

  public LocaleConfigurer(String key, String name, Locale locale) {
    super(key, name);
    setValue(locale);
  }

  public LocaleConfigurer(String key, String name, String val) {
    super(key, name, val);
    setValue(val);
  }

  public String getValueString() {
    return (String) value;
  }

  public Locale getValueLocale() {
      return stringToLocale((String) value);
  }

  public void setValue(Locale l) {
    setValue(localeToString(l));
  }

  public void setValue(String s) {
    getControls();
    if (!noUpdate && langBox != null && countryBox != null) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
      setLanguage(sd.nextToken(Locale.getDefault().getLanguage()));
      setCountry(sd.nextToken(""));
    }
    setValue((Object) s);
  }

  protected void setLanguage(String l) {
    String lang = (new Locale(l, "")).getDisplayLanguage(Locale.getDefault());
    langBox.setSelectedItem(lang);
  }

  protected void setCountry(String c) {
    String country;
    if (c.length() == 0) {
      country = ANY_COUNTRY;
    }
    else {
      country = (new Locale(Locale.getDefault().getLanguage(), c)).getDisplayCountry(Locale.getDefault());
    }
    countryBox.setSelectedItem(country);
  }

  public java.awt.Component getControls() {
    if (panel == null) {
      panel = Box.createHorizontalBox();
      langBox = new JComboBox(getLanguageList());
      langBox.setSelectedItem(Locale.getDefault().getDisplayLanguage());
      langBox.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          updateValue();
        }});
      panel.add(new JLabel("Language:  "));
      panel.add(langBox);

      countryBox = new JComboBox(getCountryList());
      countryBox.setSelectedItem(ANY_COUNTRY);
      countryBox.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          updateValue();
        }});
      panel.add(new JLabel("  Country:  "));
      panel.add(countryBox);

    }
    return panel;
  }

  protected void updateValue() {
    String language = languages.get(langBox.getSelectedItem());
    String country = countries.get(countryBox.getSelectedItem());

    setValue(language + "," + country);
  }

   protected String[] getLanguageList() {
    if (languageList == null) {
      String[] langs = Locale.getISOLanguages();
      ArrayList<String> sortedLangs = new ArrayList<String>();
      for (int i = 0; i < langs.length; i++) {
        String lang =
          (new Locale(langs[i])).getDisplayLanguage(Locale.getDefault());
        languages.put(lang, langs[i]);
        sortedLangs.add(lang);
      }
      Collections.sort(sortedLangs, Collator.getInstance(Locale.getDefault()));
      languageList = sortedLangs.toArray(new String[sortedLangs.size()]);
    }
    return languageList;
  }

  protected String[] getCountryList() {
    if (countryList == null) {
      String[] c = Locale.getISOCountries();
      ArrayList<String> sortedCountries = new ArrayList<String>();
      for (int i = 0; i < c.length; i++) {
        String country =
          (new Locale("en", c[i])).getDisplayCountry(Locale.getDefault());
        countries.put(country, c[i]);
        sortedCountries.add(country);
      }
      Collections.sort(sortedCountries,
                       Collator.getInstance(Locale.getDefault()));
      countries.put(ANY_COUNTRY, "");
      sortedCountries.add(0, ANY_COUNTRY);
      countryList = sortedCountries.toArray(new String[sortedCountries.size()]);
    }
    return countryList;
  }

  public static Locale stringToLocale(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    return new Locale(sd.nextToken(""), sd.nextToken(""));
  }

  public static String localeToString(Locale l) {
    return l.getLanguage() + "," + l.getCountry();
  }
}
