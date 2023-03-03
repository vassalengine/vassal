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

package VASSAL.build.module.folder;

import VASSAL.build.AbstractFolder;
import VASSAL.build.Buildable;
import VASSAL.build.module.PrototypeDefinition;
import VASSAL.build.module.PrototypesContainer;

public class PrototypeFolder extends AbstractFolder {
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { this.getClass(), PrototypeDefinition.class, };
  }

  @Override
  public void add(Buildable b) {
    super.add(b);
    if (b instanceof PrototypeDefinition) {
      final Buildable ancestor = getNonFolderAncestor();
      if (ancestor instanceof PrototypesContainer) {
        final PrototypesContainer protos = (PrototypesContainer) ancestor;
        protos.addDefinition((PrototypeDefinition) b);
      }
    }
  }
}
