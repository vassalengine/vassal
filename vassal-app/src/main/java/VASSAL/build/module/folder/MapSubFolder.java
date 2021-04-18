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
import VASSAL.build.module.MultiActionButton;
import VASSAL.build.module.ToolbarMenu;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.build.module.map.DrawPile;
import VASSAL.build.module.map.Flare;
import VASSAL.build.module.map.LOS_Thread;
import VASSAL.build.module.map.MapShader;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.build.module.map.SetupStack;
import VASSAL.i18n.Resources;

public class MapSubFolder extends AbstractFolder {
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[] { this.getClass(), LOS_Thread.class, ToolbarMenu.class, MultiActionButton.class, CounterDetailViewer.class, DrawPile.class, SetupStack.class, MassKeyCommand.class, MapShader.class, Flare.class };
  }
}
