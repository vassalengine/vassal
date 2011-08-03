/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney, Brent Easton
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

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PlaceMarker;

/**
 *
 * @author Brent Easton
 *
 * A dummy AbstractConfigurable to hold a PlaceMarker or Replace definition while translating
 */
public class TranslatableMarker extends AbstractConfigurable {

  protected GamePiece markerDefinition;

  public TranslatableMarker(PlaceMarker p) {
    markerDefinition = p.createBaseMarker();
    setConfigureName(p.getDescription());

  }

  public static String getConfigureTypeName() {
    return "Marker Definition";
  }

  public ComponentI18nData getI18nData() {
    return new ComponentI18nData(this, markerDefinition);
  }

  public String getAttributeValueString(String attr) {
    return getI18nData().getLocalUntranslatedValue(attr);
  }


  public void setAttribute(String attr, Object value) {

  }

  public void add(Buildable child) {
  }

  public void addTo(Buildable parent) {

  }

  public void build(Element e) {

  }

  public Element getBuildElement(Document doc) {
    return null;
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
  }

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public void removeFrom(Buildable parent) {

  }

}
