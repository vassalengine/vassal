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
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.KeyStroke;
import javax.swing.WindowConstants;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ConfigureTree;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.ValidationReport;
import VASSAL.configure.ValidityChecker;
import VASSAL.tools.AdjustableSpeedScrollPane;

/**
 * A Map that may be configured to be visible only a particular side.
 * If visible to all, the map will respond to key/mouse events
 * only from the player playing the assigned side
 */
public class PrivateMap extends Map {
  private String[] owners = new String[0];
  private boolean visibleToAll;
  private Map surrogate;

  public static final String VISIBLE = "visible";
  public static final String SIDE = "side";
  public static final String USE_BOARDS = "useBoards";

  public String[] getAttributeNames() {
    String[] s1 = new String[]{SIDE, VISIBLE, USE_BOARDS};
    String[] s2 = super.getAttributeNames();
    String[] s = new String[s1.length + s2.length];
    System.arraycopy(s1, 0, s, 0, s1.length);
    System.arraycopy(s2, 0, s, s1.length, s2.length);
    return s;
  }

  public String[] getAttributeDescriptions() {
    String[] s1 = new String[]{"Belongs to side", "Visible to all players", "Use same boards as this map"};
    String[] s2 = super.getAttributeDescriptions();
    String[] s = new String[s1.length + s2.length];
    System.arraycopy(s1, 0, s, 0, s1.length);
    System.arraycopy(s2, 0, s, s1.length, s2.length);
    return s;
  }

  public Class[] getAttributeTypes() {
    Class[] c1 = new Class[]{String[].class, Boolean.class, String.class};
    Class[] c2 = super.getAttributeTypes();
    Class[] c = new Class[c1.length + c2.length];
    System.arraycopy(c1, 0, c, 0, c1.length);
    System.arraycopy(c2, 0, c, c1.length, c2.length);
    return c;
  }

  public void setAttribute(String key, Object value) {
    if (VISIBLE.equals(key)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
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
      Iterator it = Map.getAllMaps();
      while (it.hasNext()) {
        Map m = (Map) it.next();
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
      return "" + visibleToAll;
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
    }
    return theMap;
  }

  protected Window createParentFrame() {
    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      JDialog d = new JDialog(GameModule.getGameModule().getFrame()) {
        public void setVisible(boolean show) {
          super.setVisible(show && (visibleToAll
                                    || isAccessibleTo(PlayerRoster.getMySide())));
        }
      };
      d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(getDefaultWindowTitle());
      return d;
    }
    else {
      JFrame d = new JFrame() {
        public void setVisible(boolean show) {
          super.setVisible(show && (visibleToAll
                                    || isAccessibleTo(PlayerRoster.getMySide())));
        }
      };
      d.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
      d.setTitle(getDefaultWindowTitle());
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

  public void setBoards(Enumeration boardList) {
    if (surrogate != null) {
      boardList = surrogate.getAllBoards();
      edgeBuffer = surrogate.getEdgeBuffer();
    }
    super.setBoards(boardList);
  }

  public static String getConfigureTypeName() {
    return "Private Window";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PrivateWindow.htm");
  }

  public void build(org.w3c.dom.Element el) {
    validator = new ValidityChecker() {
      public void validate(Buildable target, ValidationReport report) {
        if (!PlayerRoster.isActive()) {
          report.addWarning("Must add "+ConfigureTree.getConfigureName(PlayerRoster.class)
                            +" in order to use "+ConfigureTree.getConfigureName(getClass()));
        }
      }
    };
    surrogate = null;
    super.build(el);
  }

  public static class View extends Map.View {
    private Vector keyListeners = new Vector();
    private Vector mouseListeners = new Vector();
    private Vector mouseMotionListeners = new Vector();
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
      keyListeners.addElement(l);
    }

    public synchronized void addMouseListener(MouseListener l) {
      mouseListeners.addElement(l);
    }

    public synchronized void addMouseMotionListener(MouseMotionListener l) {
      mouseMotionListeners.addElement(l);
    }

    /**
     * Disable all keyboard and mouse listeners on this component
     */
    protected void disableListeners() {
      for (Enumeration e = keyListeners.elements(); e.hasMoreElements();) {
        removeKeyListener((KeyListener) e.nextElement());
      }
      for (Enumeration e = mouseListeners.elements(); e.hasMoreElements();) {
        removeMouseListener((MouseListener) e.nextElement());
      }
      for (Enumeration e = mouseMotionListeners.elements(); e.hasMoreElements();) {
        removeMouseMotionListener((MouseMotionListener) e.nextElement());
      }
      super.setDropTarget(null);
    }

    /**
     * Enable all keyboard and mouse listeners on this component
     */
    protected void enableListeners() {
      for (Enumeration e = keyListeners.elements(); e.hasMoreElements();) {
        super.addKeyListener((KeyListener) e.nextElement());
      }
      for (Enumeration e = mouseListeners.elements(); e.hasMoreElements();) {
        super.addMouseListener((MouseListener) e.nextElement());
      }
      for (Enumeration e = mouseMotionListeners.elements(); e.hasMoreElements();) {
        super.addMouseMotionListener((MouseMotionListener) e.nextElement());
      }
      super.setDropTarget(dropTarget);
    }
  }
}
