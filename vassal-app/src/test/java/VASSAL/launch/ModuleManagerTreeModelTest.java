/*
 *
 * Copyright (c) 2026 by The VASSAL Development Team
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
package VASSAL.launch;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;

import org.jdesktop.swingx.treetable.DefaultMutableTreeTableNode;
import org.jdesktop.swingx.treetable.DefaultTreeTableModel;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Pins down the behaviour of the tree model behind the Module Manager's
 * module list, which is where vassalengine/vassal#15104 surfaces.
 *
 * <p>The report is an {@code IllegalArgumentException} with no VASSAL frame
 * in it at all, thrown out of hit detection in {@code JXTreeTable} while the
 * user clicks. These tests say what has to be true of the model for that to
 * happen, so that the reason the Module Manager must only be changed on the
 * Event Dispatch Thread is written down and checked rather than asserted.</p>
 */
public class ModuleManagerTreeModelTest {
  /** The message the thirteen reports of #15104 all carry. */
  private static final String REPORTED =
    "node must be a TreeTableNode managed by this model";

  private DefaultMutableTreeTableNode root;
  private DefaultMutableTreeTableNode module;
  private DefaultTreeTableModel model;

  private void twoModules() {
    root = new DefaultMutableTreeTableNode("root");
    module = new DefaultMutableTreeTableNode("a.vmod");
    root.add(module);
    root.add(new DefaultMutableTreeTableNode("b.vmod"));
    model = new DefaultTreeTableModel(root);
  }

  /**
   * A node which is still in the tree is asked about without complaint. This
   * is the ordinary case, and it is here so that the test below can fail in
   * both directions rather than only in one.
   */
  @Test
  public void aNodeInTheTreeIsAcceptedByTheModel() {
    twoModules();
    assertTrue(model.isLeaf(module));
    assertFalse(model.isLeaf(root));
  }

  /**
   * Detaching a node is the whole of the reported fault: the model validates
   * by walking the node's parents up to its root, so a node which no longer
   * reaches the root is rejected -- with the exact message the reports carry.
   */
  @Test
  public void aDetachedNodeIsRejectedWithTheReportedMessage() {
    twoModules();
    model.removeNodeFromParent(module);
    assertNull(module.getParent());

    final IllegalArgumentException e = assertThrows(
      IllegalArgumentException.class, () -> model.isLeaf(module)
    );
    assertEquals(REPORTED, e.getMessage());
  }

  /**
   * The removal detaches the node before it announces it, so there is a
   * window in which a listener -- and a tree's row cache is a listener --
   * still has a row for a node the model will already refuse to answer for.
   *
   * <p>On one thread that window closes before anything can look. The Module
   * Manager used to change this tree from a launch worker as well as from the
   * EDT, and then the window is open to whatever the other thread is doing.</p>
   */
  @Test
  public void aListenerIsToldOfTheRemovalOnlyAfterTheNodeIsDetached() {
    twoModules();

    final AtomicBoolean alreadyDetached = new AtomicBoolean();
    final AtomicReference<String> message = new AtomicReference<>();

    model.addTreeModelListener(new TreeModelListener() {
      @Override
      public void treeNodesRemoved(TreeModelEvent e) {
        alreadyDetached.set(module.getParent() == null);
        try {
          model.isLeaf(module);
        }
        catch (IllegalArgumentException ex) {
          message.set(ex.getMessage());
        }
      }

      @Override
      public void treeNodesChanged(TreeModelEvent e) {
      }

      @Override
      public void treeNodesInserted(TreeModelEvent e) {
      }

      @Override
      public void treeStructureChanged(TreeModelEvent e) {
      }
    });

    model.removeNodeFromParent(module);

    assertTrue(
      alreadyDetached.get(),
      "the node should already be detached when the removal is announced"
    );
    assertEquals(REPORTED, message.get());
  }
}
