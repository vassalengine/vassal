/*
 * $Id$
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
package VASSAL.tools.imports.adc2;

import VASSAL.tools.filechooser.ExtensionFileFilter;

/**
 * A {@link FileFilter} for ADC2 symbol sets. Used by file choosers to filter
 * out files which are not Aide de Camp 2 symbol sets.
 *
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class SymbolSetFileFilter extends ExtensionFileFilter {
  public static final String[] types = { ".set" };

  public SymbolSetFileFilter() {
    super("ADC2 Symbol Sets", types);
  }
}
