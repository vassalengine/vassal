/*
 * $Id$
 *
 * Copyright (c) 2004-2009 by Michael Blumohr, Rodney Kinney, Brent Easton
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
package VASSAL.build.module;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatableConfigurerFactory;
import VASSAL.tools.FormattedString;

public class SpecialDie extends AbstractConfigurable {

  private static final Logger logger =
    LoggerFactory.getLogger(SpecialDie.class);

  private List<SpecialDieFace> dieFaceList = new ArrayList<SpecialDieFace>();
  private FormattedString format = new FormattedString("$" + RESULT + "$"); //$NON-NLS-1$ //$NON-NLS-2$

  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String FORMAT = "format"; //$NON-NLS-1$
  public static final String RESULT = "result"; //$NON-NLS-1$
  public static final String NUMERICAL_VALUE = "numericalValue"; //$NON-NLS-1$


  public void addFace(SpecialDieFace f) {
    dieFaceList.add(f);
  }

  public void removeFace(SpecialDieFace f) {
    dieFaceList.remove(f);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.SpecialDie.component_type"); //$NON-NLS-1$
  }

  public String[] getAttributeDescriptions() {
    return new String[]{
        Resources.getString(Resources.NAME_LABEL),
        Resources.getString("Editor.SpecialDie.result_format") //$NON-NLS-1$
    };

  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, ResultFormatConfig.class};
  }

  public static class ResultFormatConfig implements TranslatableConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{NAME, RESULT, NUMERICAL_VALUE});
    }
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, FORMAT};
  }

  public void setAttribute(String key, Object o) {
    if (NAME.equals(key)) {
      setConfigureName((String) o);
    }
    else if (FORMAT.equals(key)) {
      format.setFormat((String) o);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (FORMAT.equals(key)) {
      return format.getFormat();
    }
    else {
      return null;
    }
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GameModule.htm", "SpecialDiceButton"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{SpecialDieFace.class};
  }

  public void addTo(Buildable parent) {
    ((SpecialDiceButton) parent).addSpecialDie(this);
  }

  public void removeFrom(Buildable parent) {
    ((SpecialDiceButton) parent).removeSpecialDie(this);
  }

  /** Return the number of faces on this die */
  public int getFaceCount() {
    return dieFaceList.size();
  }

  public String getTextValue(int face) {
    format.setProperty(NAME, getLocalizedConfigureName());
    // No Faces may be defined, or opponent may have a version of the module with more faces defined than we have
    final int faceCount = getFaceCount();
    if  (face < 0 || face >= faceCount)  {
      format.setProperty(RESULT, "undefined"); //$NON-NLS-1$
      format.setProperty(NUMERICAL_VALUE, "0"); //$NON-NLS-1$
      logger.warn("Special Die (" + getConfigureName() + "): no such face " + face);
    }
    else {
      final SpecialDieFace aFace = dieFaceList.get(face);
      format.setProperty(RESULT, aFace.getTextValue());
      format.setProperty(NUMERICAL_VALUE, aFace.getIntValue() + ""); //$NON-NLS-1$
    }
    return format.getLocalizedText();
  }

  public int getIntValue(int face) {
    // No Faces may be defined, or opponent may have a version of the module with more faces defined than we have
    final int faceCount = getFaceCount();
    if  (face < 0 || face >= faceCount)  {
      logger.warn("Special Die (" + getConfigureName() + "): no such face " + face);
      return 0;
    }
    else {
      return dieFaceList.get(face).getIntValue();
    }
  }

  public String getImageName(int face) {
    // No Faces may be defined, or opponent may have a version of the module with more faces defined than we have
    final int faceCount = getFaceCount();
    if  (face < 0 || face >= faceCount)  {
      logger.warn("Special Die (" + getConfigureName() + "): no such face " + face);
      return ""; //$NON-NLS-1$
    }
    else {
      return dieFaceList.get(face).getImageName();
    }
  }
}
