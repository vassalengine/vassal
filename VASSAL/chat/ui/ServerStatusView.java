/*
 * $Id: ServerStatusView.java,v 1.8 2006-12-10 06:34:54 rkinney Exp $
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
package VASSAL.chat.ui;

import VASSAL.chat.SimpleRoom;
import VASSAL.chat.Player;
import VASSAL.chat.Room;
import VASSAL.chat.ServerStatus;
import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.event.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

/**
 * Shows the current status of connections to the server
 */
public class ServerStatusView extends JTabbedPane implements ChangeListener, TreeSelectionListener {
  private static final long serialVersionUID = 1L;

  public static final String SELECTION_PROPERTY = "ServerStatusView.selection";
  private ServerStatus status;
  private DefaultTreeModel model;
  private DefaultTreeModel[] historicalModels;
  private JTree treeCurrent;
  private JTree[] historicalTrees;

  public ServerStatusView(ServerStatus status) {
    this.status = status;
    initComponents();
  }

  private void initComponents() {
    JPanel current = new JPanel(new BorderLayout());
    JToolBar toolbar = new JToolBar();
    toolbar.setFloatable(false);
    JButton b = new JButton("Refresh");
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
        try {
          refresh();
        }
        finally {
          setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
      }
    });
    toolbar.add(b);
    current.add(toolbar, BorderLayout.NORTH);
    treeCurrent = createTree();
    current.add(new JScrollPane(treeCurrent), BorderLayout.CENTER);
    model = (DefaultTreeModel) treeCurrent.getModel();
    addTab("Current", current);
    addChangeListener(this);
    setBorder(new TitledBorder("Server Connections"));
    setStatusServer(status);
  }

  private void buildHistoricalTabs() {
    while (getTabCount() > 1) {
      removeTabAt(getTabCount()-1);
    }
    if (status != null) {
      String[] supported = status.getSupportedTimeRanges();
      historicalTrees = new JTree[supported.length];
      historicalModels = new DefaultTreeModel[supported.length];
      for (int i = 0; i < supported.length; i++) {
        historicalTrees[i] = createTree();
        historicalModels[i] = (DefaultTreeModel) historicalTrees[i].getModel();
        addTab(supported[i], new JScrollPane(historicalTrees[i]));
      }
    }
  }

  private JTree createTree() {
    DefaultMutableTreeNode root = new DefaultMutableTreeNode("VASSAL");
    DefaultTreeModel m = new DefaultTreeModel(root, true);
    JTree tree = new JTree(m);
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.setCellRenderer(new Render());
    tree.expandRow(0);
    tree.addTreeSelectionListener(this);
    tree.addTreeExpansionListener(new TreeExpansionListener() {
      public void treeExpanded(TreeExpansionEvent event) {
        JComponent c = (JComponent) event.getSource();
        c.setSize(c.getPreferredSize());
        c.revalidate();
      }

      public void treeCollapsed(TreeExpansionEvent event) {
      }
    });
    return tree;
  }

  public void stateChanged(ChangeEvent e) {
    if (status == null) {
      return;
    }
    setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
    try {
      int sel = getSelectedIndex();
      switch (sel) {
      case 0:
        refresh();
        break;
      default:
        refresh(historicalModels[sel-1], status.getHistory(this.getTitleAt(sel)));
        break;
      }
    }
    finally {
      setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
    }
    fireSelectionChanged();
  }

  public void valueChanged(TreeSelectionEvent e) {
    fireSelectionChanged();
  }

  private void fireSelectionChanged() {
    Object selection = null;
    TreePath path = null;
    int sel = getSelectedIndex();
    switch (sel) {
    case 0:
      path = treeCurrent.getSelectionPath();
      break;
    default:
      path = historicalTrees[sel-1].getSelectionPath();
      break;
    }
    if (path != null) {
      selection = path.getLastPathComponent();
    }
    if (selection instanceof DefaultMutableTreeNode) {
      selection = ((DefaultMutableTreeNode) selection).getUserObject();
    }
    firePropertyChange(SELECTION_PROPERTY, null, selection);
  }

  public void refresh() {
    refresh(model, status.getStatus());
  }

  private void refresh(DefaultTreeModel m, ServerStatus.ModuleSummary[] modules) {
    MutableTreeNode root = (MutableTreeNode) m.getRoot();
    while (root.getChildCount() > 0) {
      m.removeNodeFromParent((MutableTreeNode) root.getChildAt(0));
    }
    if (modules.length == 0) {
      DefaultMutableTreeNode n = new DefaultMutableTreeNode("No connections");
      n.setAllowsChildren(false);
    }
    else {
      for (int i = 0; i < modules.length; ++i) {
        m.insertNodeInto(createNode(modules[i]), root, root.getChildCount());
      }
    }
  }

  private DefaultMutableTreeNode createNode(Object o) {
    Object[] children = null;
    if (o instanceof ServerStatus.ModuleSummary) {
      children = ((ServerStatus.ModuleSummary) o).getRooms();
    }
    else if (o instanceof SimpleRoom) {
      List l = ((Room)o).getPlayerList();
      children = (Player[]) l.toArray(new Player[l.size()]);
    }
    DefaultMutableTreeNode node = new DefaultMutableTreeNode(o);
    if (children != null) {
      for (int i = 0; i < children.length; ++i) {
        node.add(createNode(children[i]));
      }
    }
    node.setAllowsChildren(children != null);
    return node;
  }
  public static class Render extends DefaultTreeCellRenderer {
    private static final long serialVersionUID = 1L;

    public Component getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
      super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
      if (leaf) {
        setIcon(null);
      }
      return this;
    }
  }

  public void setStatusServer(ServerStatus status) {
    this.status = status;
    buildHistoricalTabs();
    setEnabled(status != null);
  }
}
