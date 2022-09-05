/*
 * Copyright (c) 2022 by Joel Uckelman
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

package VASSAL.launch;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.util.HashMap;
import java.util.Map;
 
public class UseTracker {

  private final Map<File, Integer> using = new HashMap<>();

  private final PropertyChangeSupport pcs = new PropertyChangeSupport(this);

  /**
   * @param file the file to check
   * @return <code>true</code> iff the file is in use
   */
  public synchronized boolean isInUse(File file) {
    return using.containsKey(file);
  }

  /**
   * @param file the file to check
   * @return <code>true</code> iff the file is being edited
   */
  public synchronized boolean isEditing(File file) {
    return Integer.valueOf(-1).equals(using.get(file));
  }

  public synchronized void incrementUsed(File file) {
    final boolean wasOpen = using.isEmpty();
    using.merge(file, 1, Integer::sum);
    final boolean isOpen = using.isEmpty();
    if (wasOpen != isOpen) {
      this.pcs.firePropertyChange("open", wasOpen, isOpen);
    }
  }

  public synchronized void decrementUsed(File file) {
    final boolean wasOpen = using.isEmpty();
    using.merge(file, 0, (v, n) -> v == 1 ? null : v - 1);
    final boolean isOpen = using.isEmpty();
    if (wasOpen != isOpen) {
      this.pcs.firePropertyChange("open", wasOpen, isOpen);
    }
  }

  public synchronized void markEditing(File file) {
    final boolean wasOpen = using.isEmpty();
    using.put(file, -1);
    final boolean isOpen = using.isEmpty();
    if (wasOpen != isOpen) {
      this.pcs.firePropertyChange("open", wasOpen, isOpen);
    }
  }

  public synchronized void unmarkEditing(File file) {
    final boolean wasOpen = using.isEmpty();
    using.remove(file);
    final boolean isOpen = using.isEmpty();
    if (wasOpen != isOpen) {
      this.pcs.firePropertyChange("open", wasOpen, isOpen);
    }
  }

  public void addPropertyChangeListener(PropertyChangeListener listener) {
    pcs.addPropertyChangeListener(listener);
  }

  public void removePropertyChangeListener(PropertyChangeListener listener) {
    pcs.removePropertyChangeListener(listener);
  }
}
