/*
 * Copyright 2026 Vassal Development Team
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

package VASSAL.configure;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.Builder;
import VASSAL.build.GameModule;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.GameState;
import VASSAL.build.module.folder.ModuleSubFolder;
import VASSAL.tools.DataArchive;

import javax.swing.JToolBar;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.w3c.dom.Element;

public class ConfigureTreeTest {

  /**
   * Paste, clone and copy-drag build a component from XML before it is inserted into the tree. A toolbar item
   * inside a pasted folder still has to find the module to register its button and properties.
   */
  @Test
  public void folderChildrenBuiltBeforeInsertionReachTheModule() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getDataArchive()).thenReturn(new DataArchive() { });
      when(gm.getGameState()).thenReturn(mock(GameState.class));
      final JToolBar toolBar = new JToolBar();
      when(gm.getToolBar()).thenReturn(toolBar);

      final ModuleSubFolder source = new ModuleSubFolder();
      final DiceButton dice = new DiceButton();
      dice.setAttribute(AbstractToolbarItem.NAME, "Attack");
      source.add(dice);
      final Element e = source.getBuildElement(Builder.createNewDocument());

      ConfigureTree.buildAsChildOf(new ModuleSubFolder(), e, gm);

      verify(gm).addMutableProperty(eq("Attack_result"), any());
      assertEquals(1, toolBar.getComponentCount());
    }
  }
}
