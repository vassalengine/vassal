/*
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
package VASSAL.build.module;

import java.awt.Container;
import java.awt.Window;
import java.awt.dnd.DropTarget;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;

import org.apache.commons.lang3.ArrayUtils;

import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.i18n.Resources;
import VASSAL.tools.AdjustableSpeedScrollPane;
import VASSAL.tools.menu.MenuManager;

/**
 * A Map that may be configured to be visible only a particular side.
 * If visible to all, the map will respond to key/mouse events
 * only from the player playing the assigned side
 */
public class PrivateMap extends Map {
  protected String[] owners = new String[0];
  protected boolean visibleToAll;
  protected Map surrogate;

  public static final String VISIBLE = "visible"; //$NON-NLS-1$
  public static final String SIDE = "side"; //$NON-NLS-1$
  public static final String USE_BOARDS = "useBoards"; //$NON-NLS-1$

  @Override
  public String[] getAttributeNames() {
    return ArrayUtils.addAll(
      new String[]{
        SIDE,
        VISIBLE,
        USE_BOARDS
      },
      super.getAttributeNames()
    );
  }

  @Override
  public String[] getAttributeDescriptions() {
    return ArrayUtils.addAll(
      new String[]{
        Resources.getString("Editor.PrivateMap.belongs_to_one_side"),
        Resources.getString("Editor.PrivateMap.visible_to_all_player"),
        Resources.getString("Editor.PrivateMap.use_same_boards_as_this_map")
      },
      super.getAttributeDescriptions()
    );
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.addAll(
      new Class<?>[]{
        String[].class,
        Boolean.class,
        String.class
      },
      super.getAttributeTypes()
    );
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (VISIBLE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      visibleToAll = (Boolean) value;
    }
    else if (SIDE.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      owners = (String[]) value;
    }
    else if (USE_BOARDS.equals(key)) {
      for (final Map m : Map.getMapList()) {
        if (m.getMapName().equals(value)) {
          surrogate = m;
          break;
        }
      }
    }
    else {
      super.setAttribute(key, value);
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (VISIBLE.equals(key)) {
      return String.valueOf(visibleToAll);
    }
    else if (SIDE.equals(key)) {
      return StringArrayConfigurer.arrayToString(owners);
    }
    else if (USE_BOARDS.equals(key)) {
      return surrogate == null ? null :
          surrogate.getMapName();
    }
    else {
      return super.getAttributeValueString(key);
    }
  }

  @Override
  public JComponent getView() {
    if (theMap == null) {
      theMap = new View(this);
      scroll = new AdjustableSpeedScrollPane(
            theMap,
            JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
            JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
      scroll.unregisterKeyboardAction(KeyStroke.getKeyStroke(
            KeyEvent.VK_PAGE_DOWN, 0));
      scroll.unregisterKeyboardAction(KeyStroke.getKeyStroke(
            KeyEvent.VK_PAGE_UP, 0));

      layeredPane.setLayout(new InsetLayout(layeredPane, scroll));
      layeredPane.add(scroll, JLayeredPane.DEFAULT_LAYER);
    }
    return theMap;
  }

  @Override
  protected Window createParentFrame() {
    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      final JDialog d = new JDialog(GameModule.getGameModule().getPlayerWindow()) {
        private static final long serialVersionUID = 1L;

        @Override
        public void setVisible(boolean show) {
          super.setVisible(show &&
            (visibleToAll || isAccessibleTo(PlayerRoster.getMySide())));
        }
      };

      d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(getDefaultWindowTitle());
      return d;
    }
    else {
      final JFrame d = new JFrame() {
        private static final long serialVersionUID = 1L;

        @Override
        public void setVisible(boolean show) {
          super.setVisible(show &&
            (visibleToAll || isAccessibleTo(PlayerRoster.getMySide())));
        }
      };

      d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(getDefaultWindowTitle());
      d.setJMenuBar(MenuManager.getInstance().getMenuBarFor(d));
      return d;
    }
  }

  @Override
  public void sideChanged(String oldSide, String newSide) {
    super.sideChanged(oldSide, newSide);
    ((View)getView()).disableListeners();
    if (isAccessibleTo(newSide)) {
      ((View)getView()).enableListeners();
    }
    else if (isVisibleTo(newSide)) {
      ((View)getView()).enableMotionListeners();
    }
    getLaunchButton().setEnabled(isVisibleTo(PlayerRoster.getMySide()));

    // Close this private window if we've switched to a side not allowed to see it
    final Container tla = theMap.getTopLevelAncestor();
    if (tla != null) {
      if (tla.isVisible() && !isVisibleTo(newSide)) {
        tla.setVisible(false);
      }
    }
  }

  @Override
  public boolean shouldDockIntoMainWindow() {
    return false;
  }

  /** Return true if the player playing the given side can access this map
   * @see PlayerRoster
   */
  public boolean isAccessibleTo(String playerSide) {
    for (final String owner : owners) {
      if (owner.equals(playerSide)) {
        return true;
      }
    }
    return false;
  }

  public boolean isVisibleTo(String playerSide) {
    return (visibleToAll || isAccessibleTo(playerSide));
  }

  @Override
  public void setup(boolean show) {
    super.setup(show);
    if (!show) {
      ((View) theMap).disableListeners();
    }
    else if (isAccessibleTo(PlayerRoster.getMySide())) {
      ((View) theMap).enableListeners();
    }
    else if (isVisibleTo(PlayerRoster.getMySide())) {
      ((View) theMap).enableMotionListeners();
    }
    getLaunchButton().setEnabled(isVisibleTo(PlayerRoster.getMySide()));
  }

  @Override
  public void setBoards(Collection<Board> c) {
    if (surrogate != null) {
      c = surrogate.getBoards();
      edgeBuffer = surrogate.getEdgeBuffer();
    }
    super.setBoards(c);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PrivateMap.component_type"); //$NON-NLS-1$
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PrivateWindow.html"); //$NON-NLS-1$
  }

  @Override
  public void build(org.w3c.dom.Element el) {
    validator = new ValidityChecker() {
      @Override
      public void validate(Buildable target, ValidationReport report) {
        if (!PlayerRoster.isActive()) {
          report.addWarning(Resources.getString("Editor.PrivateMap.warning",
                            ConfigureTree.getConfigureName(PlayerRoster.class),
                            ConfigureTree.getConfigureName(getClass())));
        }
      }
    };
    surrogate = null;
    super.build(el);
  }

  public static class View extends Map.View {
    private static final long serialVersionUID = 1L;

    private boolean listenersActive;
    private boolean motionListenersActive;
    private final List<KeyListener> keyListeners = new ArrayList<>();
    private final List<MouseListener> mouseListeners = new ArrayList<>();
    private final List<MouseMotionListener> mouseMotionListeners = new ArrayList<>();
    private DropTarget dropTarget;

    public View(PrivateMap m) {
      super(m);
    }

    @Override
    public synchronized void setDropTarget(DropTarget dt) {
      if (dt != null) {
        dropTarget = dt;
      }
    }

    @Override
    public synchronized void addKeyListener(KeyListener l) {
      if (listenersActive) {
        super.addKeyListener(l);
      }
      else {
        keyListeners.add(l);
      }
    }

    @Override
    public synchronized void addMouseListener(MouseListener l) {
      if (listenersActive) {
        super.addMouseListener(l);
      }
      else {
        mouseListeners.add(l);
      }
    }

    @Override
    public synchronized void addMouseMotionListener(MouseMotionListener l) {
      if (listenersActive || motionListenersActive) {
        super.addMouseMotionListener(l);
      }
      else {
        mouseMotionListeners.add(l);
      }
    }

    /**
     * Disable all keyboard and mouse listeners on this component
     */
    protected void disableListeners() {
      for (final KeyListener l : keyListeners) {
        removeKeyListener(l);
      }
      for (final MouseListener l : mouseListeners) {
        removeMouseListener(l);
      }
      for (final MouseMotionListener l : mouseMotionListeners) {
        removeMouseMotionListener(l);
      }
      super.setDropTarget(null);
      listenersActive = false;
      motionListenersActive = false;
    }

    /**
     * Enable all keyboard and mouse listeners on this component
     */
    protected void enableListeners() {
      for (final KeyListener l : keyListeners) {
        super.addKeyListener(l);
      }
      for (final MouseListener l : mouseListeners) {
        super.addMouseListener(l);
      }
      for (final MouseMotionListener l : mouseMotionListeners) {
        super.addMouseMotionListener(l);
      }
      super.setDropTarget(dropTarget);
      listenersActive = true;
    }

    /**
     * Enable all mouse listeners ONLY on this component
     */
    protected void enableMotionListeners() {
      for (final MouseMotionListener l : mouseMotionListeners) {
        super.addMouseMotionListener(l);
      }
      motionListenersActive = true;
    }
  }


  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Arrays.asList(owners);
  }
}
