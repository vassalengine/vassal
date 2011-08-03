/*
 * $Id$
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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Jun 11, 2002
 * Time: 9:30:01 PM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.build.module;

import java.awt.Window;
import java.awt.dnd.DropTarget;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;

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
import VASSAL.tools.ArrayUtils;
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

  public String[] getAttributeNames() {
    return ArrayUtils.append(
      new String[]{
        SIDE,
        VISIBLE,
        USE_BOARDS
      },
      super.getAttributeNames()
    );
  }

  public String[] getAttributeDescriptions() {
    return ArrayUtils.append(
      new String[]{
        "Belongs to side",                //$NON-NLS-1$
        "Visible to all players?",        //$NON-NLS-1$
        "Use same boards as this map:  "  //$NON-NLS-1$
      },
      super.getAttributeDescriptions()
    );
  }

  public Class<?>[] getAttributeTypes() {
    return ArrayUtils.append(
      new Class<?>[]{
        String[].class,
        Boolean.class,
        String.class
      },
      super.getAttributeTypes()
    );
  }

  public void setAttribute(String key, Object value) {
    if (VISIBLE.equals(key)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String) value);
      }
      visibleToAll = ((Boolean) value).booleanValue();
    }
    else if (SIDE.equals(key)) {
      if (value instanceof String) {
        value = StringArrayConfigurer.stringToArray((String) value);
      }
      owners = (String[]) value;
    }
    else if (USE_BOARDS.equals(key)) {
      for (Map m : Map.getMapList()) {
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

  protected Window createParentFrame() {
    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      final JDialog d = new JDialog(GameModule.getGameModule().getFrame()) {
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

  public void sideChanged(String oldSide, String newSide) {
    super.sideChanged(oldSide, newSide);
    ((View)getView()).disableListeners();
    if (isAccessibleTo(newSide)) {
      ((View)getView()).enableListeners();
    }
    launchButton.setEnabled(isVisibleTo(PlayerRoster.getMySide()));
  }

  public boolean shouldDockIntoMainWindow() {
    return false;
  }

  /** Return true if the player playing the given side can access this map
   * @see PlayerRoster
   */
  public boolean isAccessibleTo(String playerSide) {
    for (int i = 0; i < owners.length; ++i) {
      if (owners[i].equals(playerSide)) {
        return true;
      }
    }
    return false;
  }

  public boolean isVisibleTo(String playerSide) {
    return (visibleToAll || isAccessibleTo(playerSide));
  }

  public void setup(boolean show) {
    super.setup(show);
    if (!show) {
      ((View) theMap).disableListeners();
    }
    else if (isAccessibleTo(PlayerRoster.getMySide())) {
      ((View) theMap).enableListeners();
    }
    launchButton.setEnabled(isVisibleTo(PlayerRoster.getMySide()));
  }

  @Override
  public void setBoards(Collection<Board> c) {
    if (surrogate != null) {
      c = surrogate.getBoards();
      edgeBuffer = surrogate.getEdgeBuffer();
    }
    super.setBoards(c);
  }

  /** @deprecated Use {@link #setBoards(Collection<Board>)} instead. */
  @Override
  @Deprecated
  public void setBoards(Enumeration<Board> boardList) {
    if (surrogate != null) {
      boardList = surrogate.getAllBoards();
      edgeBuffer = surrogate.getEdgeBuffer();
    }
    super.setBoards(boardList);
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PrivateMap.component_type"); //$NON-NLS-1$
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PrivateWindow.htm"); //$NON-NLS-1$
  }

  public void build(org.w3c.dom.Element el) {
    validator = new ValidityChecker() {
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
    private List<KeyListener> keyListeners = new ArrayList<KeyListener>();
    private List<MouseListener> mouseListeners =
      new ArrayList<MouseListener>();
    private List<MouseMotionListener> mouseMotionListeners =
      new ArrayList<MouseMotionListener>();
    private DropTarget dropTarget;

    public View(PrivateMap m) {
      super(m);
    }

    public synchronized void setDropTarget(DropTarget dt) {
      if (dt != null) {
        dropTarget = dt;
      }
    }

    public synchronized void addKeyListener(KeyListener l) {
      if (listenersActive) {
        super.addKeyListener(l);
      }
      else {
        keyListeners.add(l);
      }
    }

    public synchronized void addMouseListener(MouseListener l) {
      if (listenersActive) {
        super.addMouseListener(l);
      }
      else {
        mouseListeners.add(l);
      }
    }

    public synchronized void addMouseMotionListener(MouseMotionListener l) {
      if (listenersActive) {
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
      for (KeyListener l : keyListeners) {
        removeKeyListener(l);
      }
      for (MouseListener l : mouseListeners) {
        removeMouseListener(l);
      }
      for (MouseMotionListener l : mouseMotionListeners) {
        removeMouseMotionListener(l);
      }
      super.setDropTarget(null);
      listenersActive = false;
    }

    /**
     * Enable all keyboard and mouse listeners on this component
     */
    protected void enableListeners() {
      for (KeyListener l : keyListeners) {
        super.addKeyListener(l);
      }
      for (MouseListener l : mouseListeners) {
        super.addMouseListener(l);
      }
      for (MouseMotionListener l : mouseMotionListeners) {
        super.addMouseMotionListener(l);
      }
      super.setDropTarget(dropTarget);
      listenersActive = true;
    }
  }
}
