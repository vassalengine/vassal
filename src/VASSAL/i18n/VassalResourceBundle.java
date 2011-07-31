/*
 * $Id$
 *
 * Copyright (c) 2007 by Rodney Kinney, Brent Easton
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

import java.io.IOException;
import java.io.InputStream;
import java.util.PropertyResourceBundle;
import java.util.ResourceBundle;

/**
 * VASSAL PropertyResourceBundle
 *  - Expose setParent()
 *
 * @author Brent Easton
 *
 */
public class VassalResourceBundle extends PropertyResourceBundle {
  /**
   * Standard constructor - read properties from a file
   *
   * @param in Input stream
   * @throws IOException
   */
  public VassalResourceBundle(InputStream in) throws IOException {
    super(in);
  }

  /**
   * Expose the protected setParent() routine in the superclass as public.
   */
  public void setParent(ResourceBundle parent) {
    super.setParent(parent);
  }
}
