/*
 *
 * Copyright (c) 2021 by vassalengine.org, Brian Reynolds
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

package VASSAL.build;

import VASSAL.build.module.documentation.HelpFile;
import VASSAL.i18n.Resources;

/**
 * Abstract class for a Folder component that can contain/organize a particular type of configurable(s).
 *
 * Classes extending AbstractFolder should implement getAllowableConfigureComponents() to include the allowable
 * contained components (and include itself so that folders can be nested).
 *
 * Components that can be *contained* in folders will need to be call AbstractBuildable#getNonFolderAncestor() to
 * find their "functional parent".
 */
public abstract class AbstractFolder extends AbstractConfigurable {
  public static final String NAME        = "name"; //NON-NLS
  public static final String DESCRIPTION = "desc"; //NON-NLS

  String description = "";

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString(Resources.DESCRIPTION)
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      String.class,
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      NAME,
      DESCRIPTION
    };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DESCRIPTION.equals(key)) {
      return description;
    }
    else {
      return null;
    }
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String) value;
    }
  }


  @Override
  public void addTo(Buildable parent) {
    setAttributeTranslatable(NAME, false);
    setAttributeTranslatable(DESCRIPTION, false);
  }

  @Override
  public void removeFrom(Buildable parent) {
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Folder.html"); //NON-NLS
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Folder.component_type"); //$NON-NLS-1$
  }
}
