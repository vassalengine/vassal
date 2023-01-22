/*
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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
package VASSAL.tools;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GlobalOptions;
import VASSAL.build.module.Map;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyBuffer;
import VASSAL.i18n.Resources;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.VisibilityOption;
import VASSAL.tools.menu.CheckBoxMenuItemProxy;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.FlowLabel;
import VASSAL.tools.swing.SplitPane;
import VASSAL.tools.swing.SwingUtils;
import net.miginfocom.swing.MigLayout;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.Timer;
import javax.swing.WindowConstants;
import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.net.URL;
import java.util.List;

public class DebugControls extends AbstractBuildable implements ActionListener {
  protected static final long MEGABYTE = 1024 * 1024;

  protected JButton launch;
  protected JPanel controlPanel;

  protected SplitPane splitPane;

  protected Point cursorLocation;
  protected Point cursorLocationBoard;

  protected JLabel cursorCoordsLabel;

  protected FlowLabel selectedNameLabel;
  protected JLabel selectedCoordsLabel;
  protected JLabel selectedCoordsBoardLabel;

  //protected JLabel heapSizeLabel;
  //protected JLabel heapMaxLabel;
  //protected JLabel heapFreeLabel;

  protected Timer timer = new Timer(100, this);

  private static CheckBoxMenuItemProxy checkbox;

  public static void setCheckBox(CheckBoxMenuItemProxy c) {
    checkbox = c;
  }

  public Point getCursorLocation() {
    return cursorLocation;
  }

  private Point getBoardLocation(Point pt, Map map) {
    if (map == null) return pt;
    final Board b = map.findBoard(pt);
    if (b == null) return pt;
    final Rectangle r = b.bounds();
    final Point bp = new Point(pt);
    bp.translate(-r.x, -r.y);
    return bp;
  }

  public void setCursorLocation(Point pt, Map map) {
    cursorLocation = pt;
    cursorLocationBoard = getBoardLocation(pt, map);

    updateCoords();
    updateSelected();
  }

  public void setCursorLocation(Point pt) {
    setCursorLocation(pt, null);
  }

  public DebugControls() {
    final JSplitPane split = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
    split.setResizeWeight(0.1);

    final JPanel leftPanel = new JPanel(new MigLayout("fill, nogrid, hidemode 3")); //NON-NLS
    leftPanel.setBorder(BorderFactory.createTitledBorder(
      BorderFactory.createRaisedBevelBorder(),
      Resources.getString("Debug.component_type")));

    final Box leftBox = Box.createVerticalBox();

    final Box cursorBox = Box.createVerticalBox();
    cursorCoordsLabel = new JLabel("");
    cursorBox.add(cursorCoordsLabel);

    final Box selectedBox = Box.createVerticalBox();
    selectedNameLabel = new FlowLabel("");
    selectedCoordsLabel = new JLabel("");
    selectedCoordsBoardLabel = new JLabel("");
    selectedBox.add(selectedNameLabel);
    selectedBox.add(selectedCoordsLabel);
    selectedBox.add(selectedCoordsBoardLabel);

    leftBox.add(cursorBox);
    leftBox.add(Box.createVerticalStrut(10));
    leftBox.add(selectedBox);
    leftPanel.add(leftBox);

    //split.setLeftComponent(leftPanel);

    //final JPanel rightPanel = new JPanel(new MigLayout("fill, nogrid, hidemode 3")); //NON-NLS

    //final Box heapBox = Box.createVerticalBox();
    //heapSizeLabel = new JLabel("");
    //heapMaxLabel = new JLabel("");
    //heapFreeLabel = new JLabel("");
    //heapBox.add(heapSizeLabel);
    //heapBox.add(heapMaxLabel);
    //heapBox.add(heapFreeLabel);
    //rightPanel.add(heapBox);

    //split.setRightComponent(rightPanel);
    //split.setDividerLocation(250);
    //split.setPreferredSize(new Dimension(500, 120));
    //split.setResizeWeight(0.5);

    controlPanel = new JPanel();
    controlPanel.setLayout(new BorderLayout());
    controlPanel.add("Center", leftPanel);  //$NON-NLS-1$

    timer.addActionListener(this);

    /*
    toolbar = new JToolBar();
    controlPanel.add("North", toolbar);  //$NON-NLS-1$
    toolbar.addSeparator();

    toolbar.add(configServerButton);
    */
  }

  private void updateCoords() {
    cursorCoordsLabel.setText(Resources.getString("Debug.cursor", cursorLocation.x, cursorLocation.y) +
      (!cursorLocationBoard.equals(cursorLocation) ? "  " + Resources.getString("Debug.cursor_board", cursorLocationBoard.x, cursorLocationBoard.y) : ""));
  }


  private void updateSelected() {
    final List<GamePiece> selected = KeyBuffer.getBuffer().asList();
    if (selected.isEmpty()) {
      selectedNameLabel.setText("");
      selectedCoordsLabel.setText("");
      selectedCoordsBoardLabel.setText("");
      return;
    }

    final GamePiece piece = selected.get(0);

    selectedNameLabel.setText(piece.getName());
    selectedCoordsLabel.setText(piece.getPosition().x + "," + piece.getPosition().y);

    final Point pt = piece.getPosition();
    final Point bp = getBoardLocation(pt, piece.getMap());
    if (!bp.equals(pt)) {
      selectedCoordsBoardLabel.setText(bp.x + "," + bp.y);
    }
    else {
      selectedCoordsBoardLabel.setText("");
    }
  }


  private void updateHeap() {
    // Get current size of heap in bytes
    //final long heapSize = Runtime.getRuntime().totalMemory() * 100 / MEGABYTE;
    //heapSizeLabel.setText("Heap Size: " + heapSize/100 + "." + heapSize % 100 + " mb");

    // Get maximum size of heap in bytes. The heap cannot grow beyond this size.// Any attempt will result in an OutOfMemoryException.
    //final long heapMaxSize = Runtime.getRuntime().maxMemory() * 100 / MEGABYTE;
    //heapMaxLabel.setText("Heap Max: " + heapMaxSize/100 + "." + heapMaxSize % 100 + " mb");

    // Get amount of free memory within the heap in bytes. This size will increase // after garbage collection and decrease as new objects are created.
    //final long heapFreeSize = Runtime.getRuntime().freeMemory() * 100 / MEGABYTE;
    //heapFreeLabel.setText("Heap Free: " + heapFreeSize/100 + "." + heapFreeSize % 100 + " mb");
  }

  @Override
  public void addTo(Buildable b) {
    final GameModule gm = GameModule.getGameModule();

    launch = new JButton(Resources.getString("Debug.debug_controls"));  //$NON-NLS-1$
    launch.setFocusable(false);
    launch.setAlignmentY(0.0F);
    final ActionListener al = evt -> toggleVisible();
    launch.addActionListener(al);
    final NamedKeyStrokeListener l = new NamedKeyStrokeListener(al);
    //l.setKeyStroke(NamedKeyStroke.of(KeyEvent.VK_D, InputEvent.ALT_DOWN_MASK + InputEvent.SHIFT_DOWN_MASK));
    final URL iconURL = getClass().getResource("/images/connect.gif");  //$NON-NLS-1$
    if (iconURL != null) {
      launch.setIcon(new ImageIcon(iconURL));
      launch.setText(null);
    }

    final IconConfigurer iconConfig = new IconConfigurer("debugControlsIcon", Resources.getString("Debug.debug_controls_button_icon"), ""); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    iconConfig.setValue("");  //$NON-NLS-1$
    GlobalOptions.getInstance().addOption(iconConfig);
    iconConfig.addPropertyChangeListener(evt -> {
      launch.setIcon(iconConfig.getIconValue());
      launch.setText((launch.getIcon() == null) ? " " : null);
      launch.setVisible(launch.getIcon() != null);
    });
    iconConfig.fireUpdate();

    final NamedHotKeyConfigurer keyConfig = new NamedHotKeyConfigurer("debugControlsHotKey", Resources.getString("Debug.debug_controls_hotkey"), l.getNamedKeyStroke());   //$NON-NLS-1$ //$NON-NLS-2$
    GlobalOptions.getInstance().addOption(keyConfig);
    keyConfig.addPropertyChangeListener(evt -> {
      l.setKeyStroke(keyConfig.getValueNamedKeyStroke());
      launch.setToolTipText(Resources.getString("Debug.debug_controls_tooltip", NamedHotKeyConfigurer.getString(l.getKeyStroke())));  //$NON-NLS-1$
    });
    keyConfig.fireUpdate();

    gm.addKeyStrokeListener(l);
    gm.getToolBar().add(launch);
    launch.setText((launch.getIcon() == null) ? " " : null);
    launch.setVisible(launch.getIcon() != null);

    if (checkbox != null) {
      checkbox.setSelected(isVisible());
    }
  }

  @Override
  public void actionPerformed(ActionEvent evt) {
    if (evt.getSource() == timer) {
      updateHeap();
      updateSelected();
    }
  }

  public boolean isVisible() {
    return (splitPane != null) ? splitPane.isRightVisible() : ((controlPanel != null) && (controlPanel.getTopLevelAncestor() != null) && controlPanel.getTopLevelAncestor().isVisible());
  }

  private void updateVisible() {
    final boolean visible = (splitPane != null) ? splitPane.isRightVisible() : controlPanel.getTopLevelAncestor().isVisible();
    if (checkbox != null) {
      checkbox.setSelected(visible);
    }
    if (visible) {
      timer.start();
    }
    else {
      timer.stop();
    }
  }

  public void toggleVisible() {
    if (controlPanel.getTopLevelAncestor() == null) {
      if (GlobalOptions.getInstance().isUseSingleWindow()) {
        final Component gmcp = GameModule.getGameModule().getControlPanel();
        final Container gmcppar = gmcp.getParent();
        final int i = SwingUtils.getIndexInParent(gmcp, gmcppar);

        splitPane = new SplitPane(SplitPane.HORIZONTAL_SPLIT, gmcp, controlPanel);
        splitPane.setResizeWeight(1.0);
        gmcppar.add(splitPane, i);
      }
      else {
        final JFrame frame = new JFrame(Resources.getString("Debug.debug_controls"));  //$NON-NLS-1$
        frame.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
        frame.add(controlPanel);
        frame.setJMenuBar(MenuManager.getInstance().getMenuBarFor(frame));

        final String key = "BoundsOfClientWindow";  //$NON-NLS-1$
        final PositionOption pos = new VisibilityOption(key, frame);
        GameModule.getGameModule().getPrefs().addOption(pos);
        frame.setVisible(true);
      }
    }
    else if (splitPane != null) {
      splitPane.toggleRight();
    }
    else {
      controlPanel.getTopLevelAncestor().setVisible(!controlPanel.getTopLevelAncestor().isVisible());
    }

    updateVisible();
  }

  public JPanel getControls() {
    return controlPanel;
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  @Override
  public void setAttribute(String name, Object value) {
  }

  @Override
  public String getAttributeValueString(String name) {
    return null;
  }

  //public JToolBar getToolbar() {
  //  return toolbar;
  //}
}
