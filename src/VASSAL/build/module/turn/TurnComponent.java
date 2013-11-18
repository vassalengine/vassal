/*
 * $Id$
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
import java.util.List;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.BadDataReport;
import VASSAL.build.Buildable;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

/**
 * Generic Turn Component
 */
public class TurnComponent extends AbstractConfigurable {

  protected List<TurnLevel> levels = new ArrayList<TurnLevel>();

  public void addLevel(TurnLevel t) {
    levels.add(t);
  }

  public void removeLevel(TurnLevel t) {
    levels.remove(t);
  }

  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  protected TurnLevel getTurnLevel(int i) {
    if (levels.size() == 0) {
      return null;
    }
    else {
      if (i >= levels.size()) {
        ErrorDialog.dataError(new BadDataReport(Resources.getString("TurnTracker.level_error", getConfigureName(), getConfigureName(), i, levels.size()), getConfigureName()));
        return levels.get(levels.size()-1);
      }
    }
    return levels.get(i);
  }

  protected int getTurnLevelCount() {
    return levels.size();
  }

  public Iterator<TurnLevel> getTurnLevels() {
    return levels.iterator();
  }

  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  public String[] getAttributeNames() {
    return new String[0];
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

  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  public void addTo(Buildable parent) {
  }
}

