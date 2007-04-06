/*
 *
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
package VASSAL.build.module.properties;

import java.beans.PropertyChangeListener;
import java.util.Iterator;
import java.util.List;
import VASSAL.command.Command;

/**
 * A container for a String property that can be updated
 * @author rkinney
 */
public interface MutablePropertySource {
  String getPropertyValue();
  Command setPropertyValue(String newValue);
  void addPropertyChangeListener(PropertyChangeListener l);
  void removePropertyChangeListener(PropertyChangeListener l);
  
  public static class Util {
    /**
     * Look for a GlobalProperty in the list of GlobalPropertyContainers. Return the first one found, searching the lists
     * in order. The list may contain null references, which are skipped
     * 
     * @param propertyContainers
     * @return
     */
    public static MutablePropertySource findMutableProperty(String propertyName, List propertyContainers) {
      MutablePropertySource p = null;
      for (Iterator it = propertyContainers.iterator(); it.hasNext() && p == null;) {
        MutablePropertiesContainer c = (MutablePropertiesContainer) it.next();
        p = (c == null ? null : c.getGlobalProperty(propertyName));
      }
      return p;
    }

  }
}
