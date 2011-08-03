/*
 * $Id$
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.concurrent.ExecutionException;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.JTree;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import org.jdesktop.swingworker.SwingWorker;

import VASSAL.chat.Player;
import VASSAL.chat.ServerStatus;
import VASSAL.chat.SimpleRoom;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;

/**
 * Shows the current status of connections to the server
 */
public class ServerStatusView extends JTabbedPane implements ChangeListener, TreeSelectionListener {
  private static final long serialVersionUID = 1L;

  public static final String SELECTION_PROPERTY = "ServerStatusView.selection"; //$NON-NLS-1$
  private ServerStatus status;
  private DefaultTreeModel model;
  private DefaultTreeModel[] historicalModels;
  private JTree treeCurrent;
  private JTree[] historicalTrees;
  private int totalPlayers;

  public ServerStatusView(ServerStatus status) {
    this.status = status;
    initComponents();
  }

  private void initComponents() {
    JPanel current = new JPanel(new BorderLayout());
    JToolBar toolbar = new JToolBar();

    toolbar.setFloatable(false);
    JButton b = new JButton(Resources.getString("Chat.refresh")); //$NON-NLS-1$
    b.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        refresh();
      }
    });
    toolbar.add(b);
    current.add(toolbar, BorderLayout.NORTH);
    treeCurrent = createTree();
    current.add(new JScrollPane(treeCurrent), BorderLayout.CENTER);
    model = (DefaultTreeModel) treeCurrent.getModel();
    addTab(Resources.getString("Chat.current"), current); //$NON-NLS-1$
    addChangeListener(this);
    setBorder(new TitledBorder(Resources.getString("Chat.server_connections"))); //$NON-NLS-1$
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
    DefaultMutableTreeNode root = new DefaultMutableTreeNode(Resources.getString(Resources.VASSAL));
    DefaultTreeModel m = new DefaultTreeModel(root, true);
    JTree tree = new JTree(m);
    tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
    tree.setCellRenderer(new Render());
    tree.expandRow(0);
    tree.setLargeModel(true);
    tree.setRowHeight(18);  // FIXME: check whether this is necessary
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
    if (status == null) return;
    refresh(getSelectedIndex());
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
    refresh(0);
  }

  private SwingWorker<ServerStatus.ModuleSummary[],Void> cur_request = null;
  private SwingWorker<ServerStatus.ModuleSummary[],Void> hist_request = null;

  private void refresh(final int page) {
    if (page == 0) {
      if (cur_request != null && !cur_request.isDone()) return;

      setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

      cur_request = new SwingWorker<ServerStatus.ModuleSummary[],Void>() {
        @Override
        public ServerStatus.ModuleSummary[] doInBackground() {
          return status.getStatus();
        }

        @Override
        protected void done() {
          try {
            if (getSelectedIndex() == 0) {
              refresh(model, get());
              fireSelectionChanged();
            }
          }
          catch (InterruptedException ex) {
            ErrorDialog.bug(ex);
          }
          // FIXME: review error message
          catch (ExecutionException ex) {
            ex.printStackTrace();
          }

          if (hist_request == null || hist_request.isDone())
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));

          cur_request = null;
        }
      };

      cur_request.execute();
    }
    else {
      if (hist_request != null && !hist_request.isDone()) return;

      setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));

      hist_request = new SwingWorker<ServerStatus.ModuleSummary[],Void>() {
        @Override
        public ServerStatus.ModuleSummary[] doInBackground() {
          return status.getHistory(getTitleAt(page));
        }

        @Override
        protected void done() {
          final int sel = getSelectedIndex();
          if (sel == page) {
            // page didn't change, refresh with what we computed
            try {
              refresh(historicalModels[sel-1], get());
            }
            // FIXME: review error message
            catch (InterruptedException ex) {
              ex.printStackTrace();
            }
            // FIXME: review error message
            catch (ExecutionException ex) {
              ex.printStackTrace();
            }

            fireSelectionChanged();
          }
          else if (sel != 0) {
            // page changed, refresh that page instead
            hist_request = null;
            refresh(sel);
          }

          if (cur_request == null || cur_request.isDone())
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
      };

      hist_request.execute();
    }
  }

  private void refresh(DefaultTreeModel m,
                       ServerStatus.ModuleSummary[] modules) {
    final MutableTreeNode root = (MutableTreeNode) m.getRoot();
    totalPlayers = 0;

    while (root.getChildCount() > 0) {
      m.removeNodeFromParent((MutableTreeNode) root.getChildAt(0));
    }

    if (modules.length == 0) {
      final DefaultMutableTreeNode n = new DefaultMutableTreeNode(
        Resources.getString("Chat.no_connections")); //$NON-NLS-1$
      n.setAllowsChildren(false);
    }
    else {
      for (ServerStatus.ModuleSummary s : modules) {
        m.insertNodeInto(createNode(s), root, root.getChildCount());
      }
    }

    // append total number of players on server to root node
    root.setUserObject(
      Resources.getString(Resources.VASSAL) + " (" + totalPlayers + ")");
  }

  private DefaultMutableTreeNode createNode(Object o) {
    Object[] children = null;
    if (o instanceof ServerStatus.ModuleSummary) {
      final ServerStatus.ModuleSummary ms = (ServerStatus.ModuleSummary) o;

      children = ms.getRooms();

      final int players = ms.numPlayers();
      ms.setModuleName(ms.getModuleName() + " (" + players + ")");
      totalPlayers += players;
    }
    else if (o instanceof SimpleRoom) {
      final SimpleRoom r = (SimpleRoom) o;

      final List<Player> l = r.getPlayerList();

      // append the number of players to each room name
      r.setName(r.getName() + " (" + l.size() + ")");
      children = l.toArray(new Player[l.size()]);
    }

    final DefaultMutableTreeNode node = new DefaultMutableTreeNode(o);

    if (children != null) {
      for (Object c : children) {
        node.add(createNode(c));
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
      putClientProperty(Resources.getString("ServerStatusView.4"), Boolean.TRUE); //$NON-NLS-1$
      return this;
    }
  }

  public void setStatusServer(ServerStatus status) {
    this.status = status;
    buildHistoricalTabs();
    setEnabled(status != null);
  }
}
