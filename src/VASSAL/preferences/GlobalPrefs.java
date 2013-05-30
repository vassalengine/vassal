/*
 * $Id: GlobakPrefs.java 8428 2012-11-14 19:29:03Z uckelman $
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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

/**
 * Bug 10179
 * Global preferences are now written to disk each time a value is changed.
 * Each time a value is read, check first if there is an updated copy of prefs on disk
 * and read them in first.
 */
package VASSAL.preferences;

import java.beans.PropertyChangeEvent;
import java.io.IOException;

import VASSAL.tools.WriteErrorDialog;

public class GlobalPrefs extends Prefs {
  
  protected long dtm = 0;

  public GlobalPrefs(PrefsEditor editor, String name) {
    super(editor, name);    
  }
  
  protected void handleValueChange(PropertyChangeEvent evt) {
    super.handleValueChange(evt);
    try {
      write();
    }
    catch (IOException ex) {
      WriteErrorDialog.error(ex, getFile().getPath());
    }
  }
  
  public Object getValue(String key) {
    final long currentDtm = getFile().lastModified();
    if (currentDtm != dtm) {
      dtm = currentDtm;
      read();
      init(name);
    }
    return super.getValue(key);
  }
}