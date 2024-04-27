/*
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

import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerPanel;
import VASSAL.tools.SequenceEncoder;

import java.text.Collator;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * Configure a Locale Value using full, localized Language and Country names
 *
 * @author Brent Easton
 *
 */
public class LocaleConfigurer extends Configurer {
  //FIXME needs an i18n strategy
  protected static final String ANY_COUNTRY = "[Any Country]";
  protected JPanel panel;
  protected static final Map<String, String> languages = new HashMap<>();
  protected static String[] languageList;
  protected static final Map<String, String> countries = new HashMap<>();
  protected static String[] countryList;

  protected JComboBox<String> langBox;
  protected JComboBox<String> countryBox;

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

  @Override
  public String getValueString() {
    return (String) value;
  }

  public Locale getValueLocale() {
    return stringToLocale((String) value);
  }

  public void setValue(Locale l) {
    setValue(localeToString(l));
  }

  @Override
  public void setValue(String s) {
    getControls();
    if (!noUpdate && langBox != null && countryBox != null) {
      final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
      setLanguage(sd.nextToken(Locale.getDefault().getLanguage()));
      setCountry(sd.nextToken(""));
    }
    setValue((Object) s);
  }

  protected void setLanguage(String l) {
    final String lang = (new Locale(l, "")).getDisplayLanguage(Locale.getDefault());
    langBox.setSelectedItem(lang);
  }

  protected void setCountry(String c) {
    final String country;
    if (c.length() == 0) {
      country = ANY_COUNTRY;
    }
    else {
      country = (new Locale(Locale.getDefault().getLanguage(), c)).getDisplayCountry(Locale.getDefault());
    }
    countryBox.setSelectedItem(country);
  }

  @Override
  public java.awt.Component getControls() {
    if (panel == null) {
      panel = new ConfigurerPanel(getName(), "[]rel[][]rel[]", "[]rel[]rel[][]rel[]"); // NON-NLS

      langBox = new JComboBox<>(getLanguageList());
      langBox.setSelectedItem(Locale.getDefault().getDisplayLanguage());
      langBox.addActionListener(e -> updateValue());
      panel.add(new JLabel(Resources.getString("Editor.LocaleConfigurer.language")));
      panel.add(langBox);

      countryBox = new JComboBox<>(getCountryList());
      countryBox.setSelectedItem(ANY_COUNTRY);
      countryBox.addActionListener(e -> updateValue());
      panel.add(new JLabel((Resources.getString("Editor.LocaleConfigurer.country"))));
      panel.add(countryBox);

    }
    return panel;
  }

  protected void updateValue() {
    final String language = languages.get(langBox.getSelectedItem());
    final String country = countries.get(countryBox.getSelectedItem());

    setValue(language + "," + country);
  }

  protected String[] getLanguageList() {
    if (languageList == null) {
      final String[] langs = Locale.getISOLanguages();
      final List<String> sortedLangs = new ArrayList<>();
      for (final String s : langs) {
        final String lang = (new Locale(s)).getDisplayLanguage(Locale.getDefault());
        languages.put(lang, s);
        sortedLangs.add(lang);
      }
      sortedLangs.sort(Collator.getInstance(Locale.getDefault()));
      languageList = sortedLangs.toArray(new String[0]);
    }
    return languageList;
  }

  protected String[] getCountryList() {
    if (countryList == null) {
      final String[] c = Locale.getISOCountries();
      final List<String> sortedCountries = new ArrayList<>();
      for (final String s : c) {
        final String country =
                (new Locale("en", s)).getDisplayCountry(Locale.getDefault()); //NON-NLS
        countries.put(country, s);
        sortedCountries.add(country);
      }
      sortedCountries.sort(Collator.getInstance(Locale.getDefault()));
      countries.put(ANY_COUNTRY, "");
      sortedCountries.add(0, ANY_COUNTRY);
      countryList = sortedCountries.toArray(new String[0]);
    }
    return countryList;
  }

  public static Locale stringToLocale(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    return new Locale(sd.nextToken(""), sd.nextToken(""));
  }

  public static String localeToString(Locale l) {
    return l.getLanguage() + "," + l.getCountry();
  }
}
