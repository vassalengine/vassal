package VASSAL.build.widget;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Point;
import java.awt.Window;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import org.w3c.dom.Element;
import VASSAL.build.Buildable;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;

public class MapWidget extends Widget {
  protected JPanel panel;
  protected JScrollPane mapScroll;
  protected WidgetMap map;
  protected Buildable parent;
  protected JTabbedPane tab;

  public MapWidget() {
    panel = new JPanel();
    panel.setLayout(new BorderLayout());
  }

  public static String getConfigureTypeName() {
    return "Map";
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChartWindow.htm", "Map");
  }

  public void build(Element e) {
    if (e == null) {
      WidgetMap map = new WidgetMap();
      map.build(null);
      map.addTo(this);
      add(map);
    }
    else {
      super.build(e);
    }
    /*
     * Maps must be built prior to game start, so force a rebuild immediately. Default for widgets is to defer build
     * until first call to getComponent()
     */
    rebuild();
  }

  /*
   * Parent Widget has now completed building, so set up Drag Target handling if our parent is a TabWidget
   */
  public Component getComponent() {
    if (tab == null && parent instanceof TabWidget) {
      tab = (JTabbedPane) ((TabWidget) parent).getComponent();
      if (tab.getClientProperty(TabSwitcher.class) == null) {
        TabSwitcher switcher = new TabSwitcher(tab);
        tab.putClientProperty(TabSwitcher.class, switcher);
        tab.setDropTarget(new DropTarget(tab, DnDConstants.ACTION_MOVE, switcher));
      }
    }
    return panel;
  }

  public void addTo(Buildable b) {
    super.addTo(b);
    parent = b;
  }

  public void add(Buildable b) {
    if (b instanceof WidgetMap) {
      if (mapScroll != null) {
        panel.remove(mapScroll);
        mapScroll = null;
      }
      map = (WidgetMap) b;
      mapScroll = map.getScroll();
      panel.add(mapScroll, BorderLayout.CENTER);
      panel.add(map.getToolBar(), BorderLayout.NORTH);
      // Fix for bug 1630993: toolbar buttons not appearing
      map.getToolBar().addHierarchyListener(new HierarchyListener() {
        public void hierarchyChanged(HierarchyEvent e) {
          Window w;
          if ((w = SwingUtilities.getWindowAncestor(map.getToolBar())) != null) {
            w.validate();
          }
          if (map.getToolBar().getSize().width > 0) {
            map.getToolBar().removeHierarchyListener(this);
          }
        }
      });
      panel.revalidate();
    }
    super.add(b);
  }

  public void remove(Buildable b) {
    if (b instanceof WidgetMap) {
      panel.remove(mapScroll);
      panel.remove(map.getToolBar());
      mapScroll = null;
    }
    super.remove(b);
  }

  public String[] getAttributeDescriptions() {
    return new String[]{"Name:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[]{String.class};
  }

  public String[] getAttributeNames() {
    return new String[]{NAME};
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }
  protected static class TabSwitcher implements DropTargetListener {
    protected JTabbedPane tab;

    public TabSwitcher(JTabbedPane tab) {
      this.tab = tab;
    }

    public void dragOver(DropTargetDragEvent e) {
      if (tab != null) {
        Point p = e.getLocation();
        int tabNumber = tab.getUI().tabForCoordinate(tab, p.x, p.y);
        if (tabNumber >= 0 && tabNumber != tab.getSelectedIndex()) {
          tab.setSelectedIndex(tabNumber);
          tab.repaint();
        }
      }
    }

    public void dragEnter(DropTargetDragEvent e) {
    }

    public void dropActionChanged(DropTargetDragEvent e) {
    }

    public void drop(DropTargetDropEvent e) {
    }

    public void dragExit(DropTargetEvent e) {
    }
  }

  public WidgetMap getMap() {
    return map;
  }
}
