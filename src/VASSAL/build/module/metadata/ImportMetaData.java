/*
 * $Id$
 *
 * Copyright (c) 2008 by Brent Easton and Joel Uckelman
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
package VASSAL.build.module.metadata;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 *
 * Class representing the metadata for an Importable file.
 *
 */
public class ImportMetaData extends AbstractMetaData {

  public static final String DATA_VERSION = "1";

  protected void addElements(Document doc, Element root) {

  }

  public String getMetaDataVersion() {
    return DATA_VERSION;
  }

  public String getZipEntryName() {
    return null;
  }

}