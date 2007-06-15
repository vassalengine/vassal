/*
 * $Id: TurnComponent.java 919 2006-05-29 13:24:46 +0000 (Mon, 29 May 2006) swampwallaby $
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.build.module.turn;

import java.util.ArrayList;
import java.util.Iterator;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;

/**
 * Generic Turn Component
 */
public class TurnComponent extends AbstractConfigurable {

  protected ArrayList levels = new ArrayList();
  
  public void addLevel(TurnLevel t) {
    levels.add(t);
  }
  
  public void removeLevel(TurnLevel t) {
    levels.remove(t);
  }
  
  public String[] getAttributeDescriptions() {
    return null;
  }
  
  protected TurnLevel getTurnLevel(int i) {
    if (i >= levels.size()) {
      return null;
    }
    return (TurnLevel) levels.get(i);
  }

  protected int getTurnLevelCount() {
    return levels.size();
  }

  public Iterator getTurnLevels() {
    return levels.iterator();
  }
  
  public Class[] getAttributeTypes() {
    return null;
  }

  public String[] getAttributeNames() {
    return null;
  }

  public void setAttribute(String key, Object value) {
    
  }

  public String getAttributeValueString(String key) {
    return null;
  }

  public void removeFrom(Buildable parent) {

  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return null;
  }

  public void addTo(Buildable parent) {

  }
}

