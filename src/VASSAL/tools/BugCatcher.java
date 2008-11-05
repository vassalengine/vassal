/*
 * $Id: BugDialog.java 4263 2008-10-15 21:46:16Z uckelman $
 *
 * Copyright (c) 2008 by Joel Uckelman
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
package VASSAL.tools;

import VASSAL.tools.logging.LogEntry;
import VASSAL.tools.logging.LogListener;
import VASSAL.tools.logging.LogManager;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class BugCatcher implements LogListener {
  public void handle(LogEntry entry) {
    if (entry.type == LogEntry.BUG) {
      if (DialogUtils.setDisabledIfNot(BugDialog.class)) {
        BugDialog.reportABug(entry.thrown);
        LogManager.removeLogListener(this);
      }
    }
  }
}
