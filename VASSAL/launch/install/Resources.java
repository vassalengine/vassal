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
package VASSAL.launch.install;

import java.text.MessageFormat;
import java.util.Locale;
import java.util.ResourceBundle;

/**
 * Resources class with minimal dependencies for use with the installer
 * @author rkinney
 */
public class Resources {
  public static final String NEXT = "General.next"; //$NON-NLS-1$
  public static final String SELECT = "General.select"; //$NON-NLS-1$
  private static ResourceBundle theBundle;
  private static String VASSAL_BUNDLE = "VASSAL.i18n.VASSAL"; //$NON-NLS-1$

  public static String getString(String id) {
    if (theBundle == null) {
      theBundle = ResourceBundle.getBundle(VASSAL_BUNDLE, Locale.getDefault());
    }
    return theBundle.getString(id);
  }

  public static String getString(String id, String param) {
    return new MessageFormat(getString(id)).format(new Object[]{param});
  }
}
