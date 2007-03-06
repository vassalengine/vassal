/*
 * $Id$
 *
 * Copyright (c) 2004 by Michael Blumohr, Rodney Kinney
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
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.tools.FormattedString;

public class SpecialDie extends AbstractConfigurable {

  private List dieFaceList = new ArrayList();
  private FormattedString format = new FormattedString("$" + RESULT + "$");

  public static final String NAME = "name";
  public static final String FORMAT = "format";
  public static final String RESULT = "result";
  public static final String NUMERICAL_VALUE = "numericalValue";


  public void addFace(SpecialDieFace f) {
    dieFaceList.add(f);
  }

  public void removeFace(SpecialDieFace f) {
    dieFaceList.remove(f);
  }

  public static String getConfigureTypeName() {
    return "Symbolic Die";
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name:  ", "Results format:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class, ResultFormatConfig.class};
  }

  public static class ResultFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[]{NAME, RESULT, NUMERICAL_VALUE});
    }
  }

  public String[] getAttributeNames() {
    String s[] = {NAME, FORMAT};
    return s;
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
    return HelpFile.getReferenceManualPage("GameModule.htm", "SpecialDiceButton");
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[]{SpecialDieFace.class};
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
    SpecialDieFace aFace = (SpecialDieFace) dieFaceList.get(face);
    format.setProperty(NAME, getConfigureName());
    format.setProperty(RESULT, aFace.getTextValue());
    format.setProperty(NUMERICAL_VALUE, aFace.getIntValue() + "");
    return format.getText();
  }

  public int getIntValue(int face) {
    SpecialDieFace aFace = (SpecialDieFace) dieFaceList.get(face);
    return aFace.getIntValue();
  }

  public String getImageName(int face) {
    SpecialDieFace aFace = (SpecialDieFace) dieFaceList.get(face);
    return aFace.getImageName();
  }
}