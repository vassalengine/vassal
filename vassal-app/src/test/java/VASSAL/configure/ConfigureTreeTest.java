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
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import VASSAL.build.AbstractToolbarItem;
import VASSAL.build.Builder;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.DiceButton;
import VASSAL.build.module.GameState;
import VASSAL.build.module.Map;
import VASSAL.build.module.folder.MapSubFolder;
import VASSAL.build.module.folder.ModuleSubFolder;
import VASSAL.build.module.map.CounterDetailViewer;
import VASSAL.build.module.map.Zoomer;
import VASSAL.tools.DataArchive;

import javax.swing.JToolBar;
import javax.swing.tree.DefaultMutableTreeNode;

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

  @Test
  public void testUniqueAndMandatoryComponentMoveAndCopyRules() {
    try (MockedStatic<GameModule> staticGm = Mockito.mockStatic(GameModule.class)) {
      final GameModule gm = mock(GameModule.class);
      staticGm.when(GameModule::getGameModule).thenReturn(gm);
      when(gm.getDataArchive()).thenReturn(new DataArchive() { });
      when(gm.getGameState()).thenReturn(mock(GameState.class));
      final JToolBar toolBar = new JToolBar();
      when(gm.getToolBar()).thenReturn(toolBar);
      final VASSAL.preferences.Prefs prefs = mock(VASSAL.preferences.Prefs.class);
      when(prefs.getValue(any())).thenReturn(Boolean.FALSE);
      when(prefs.getValue(eq("scrollSpeed"))).thenReturn(10);
      when(gm.getPrefs()).thenReturn(prefs);

      final Map map1 = new Map();
      map1.build(Builder.createNewDocument().createElement("map"));
      final Zoomer zoomer = new Zoomer();
      map1.add(zoomer);

      final MapSubFolder folder = new MapSubFolder();
      map1.add(folder);

      final ConfigureTree tree = new ConfigureTree(map1, null);
      final DefaultMutableTreeNode zoomerNode = tree.getTreeNode(zoomer);

      // Rule 1: Immobile components cannot be pasted anywhere
      final Configurable immobile = mock(Configurable.class);
      when(immobile.isMovable()).thenReturn(false);
      final DefaultMutableTreeNode immobileNode = new DefaultMutableTreeNode(immobile);
      assertFalse(tree.isValidPasteTarget(map1, immobileNode, true));
      assertFalse(tree.isValidPasteTarget(map1, immobileNode, false));

      // Rule 3.2: Move (cut) of unique/mandatory Zoomer within the same parent is allowed (Fix for #12984)
      assertTrue(tree.isValidPasteTarget(map1, zoomerNode, true));

      // Rule 3.2: Copy of unique Zoomer to same parent that already contains it is disallowed
      assertFalse(tree.isValidPasteTarget(map1, zoomerNode, false));

      // Rule 2 & 3.1: Mandatory and Unique component cannot be cut and moved to a different parent
      final Map map2 = new Map();
      map2.build(Builder.createNewDocument().createElement("map"));
      assertFalse(tree.isValidPasteTarget(map2, zoomerNode, true));

      // Copying unique Zoomer to a parent that does not yet contain one is allowed
      assertTrue(tree.isValidPasteTarget(map2, zoomerNode, false));

      // If map2 already has a Zoomer, copying to it is disallowed
      final Zoomer zoomer2 = new Zoomer();
      map2.add(zoomer2);
      assertFalse(tree.isValidPasteTarget(map2, zoomerNode, false));

      // Rule 4: Zoomer cannot be pasted into MapSubFolder because it's not an allowed child of MapSubFolder
      assertFalse(tree.isValidPasteTarget(folder, zoomerNode, true));

      // Moving an allowed child into MapSubFolder is allowed
      final CounterDetailViewer cdv = new CounterDetailViewer();
      map1.add(cdv);
      final ConfigureTree treeWithCdv = new ConfigureTree(map1, null);
      final DefaultMutableTreeNode cdvNode = treeWithCdv.getTreeNode(cdv);
      assertTrue(treeWithCdv.isValidPasteTarget(folder, cdvNode, true));
    }
  }
}
