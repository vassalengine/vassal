/*
 *
 * Copyright (c) 2023 by vassalengine.org, Brian Reynolds
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

package VASSAL.build.module.folder;

import VASSAL.build.AbstractFolder;
import VASSAL.build.module.font.ModuleFont;

public class FontSubFolder extends AbstractFolder {
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] {
      this.getClass(),
      ModuleFont.class
    };
  }
}
