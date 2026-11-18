package VASSAL.build.widget;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Point;
import java.awt.dnd.DnDConstants;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;

import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;

import VASSAL.i18n.Resources;
import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;

public class MapWidget extends Widget {
  protected JPanel panel;
  protected JComponent mapHolder;
  protected WidgetMap map;
  protected Buildable parent;
  protected JTabbedPane tab;

  public MapWidget() {
    panel = new JPanel();
    panel.setLayout(new BorderLayout());
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.MapWidget.component_type");
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ChartWindow.html", "Map"); //NON-NLS
  }

  @Override
  public void build(Element e) {
    if (e == null) {
      final WidgetMap map = new WidgetMap();
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
  @Override
  public Component getComponent() {
    if (tab == null && parent instanceof TabWidget) {
      tab = (JTabbedPane) ((TabWidget) parent).getComponent();
      if (tab.getClientProperty(TabSwitcher.class) == null) {
        final TabSwitcher switcher = new TabSwitcher(tab);
        tab.putClientProperty(TabSwitcher.class, switcher);
        tab.setDropTarget(new DropTarget(tab, DnDConstants.ACTION_MOVE, switcher));
      }
    }
    return panel;
  }

  @Override
  public void addTo(Buildable b) {
    super.addTo(b);
    parent = b;
  }

  @Override
  public void add(Buildable b) {
    if (b instanceof WidgetMap) {
      detachMap();
      map = (WidgetMap) b;
      mapHolder = map.getLayeredPane();
      panel.add(mapHolder, BorderLayout.CENTER);
      panel.add(map.getToolBar(), BorderLayout.NORTH);
      panel.revalidate();
    }
    super.add(b);
  }

  @Override
  public void remove(Buildable b) {
    // Only the map which is actually on display owns anything in the panel. This used to strip
    // the panel from the mapHolder and map fields whatever b was, so a second WidgetMap -- or
    // the same one a second time -- reached Container.remove(null) and threw a
    // NullPointerException before super.remove(b) could run. The component was then still in
    // the widget, which is why the deleted maps came back when the module was reopened.
    // See vassalengine/vassal#14667.
    if (b == map) {
      detachMap();
      panel.revalidate();
    }
    super.remove(b);
  }

  /**
   * Take the map currently on display, if there is one, out of the panel. The holder and the
   * toolbar go together: leaving the old map's toolbar behind when a new map replaced it left a
   * component in the panel which nothing owned any more.
   */
  private void detachMap() {
    if (mapHolder != null) {
      panel.remove(mapHolder);
      mapHolder = null;
    }
    if (map != null) {
      panel.remove(map.getToolBar());
      map = null;
    }
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{Resources.getString("Editor.name_label"), Resources.getString(Resources.DESCRIPTION)};
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{String.class, String.class};
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, DESCRIPTION};
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
    }
    else if (DESCRIPTION.equals(key)) {
      description = (String)value;
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (DESCRIPTION.equals(key)) {
      return description;
    }
    return null;
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  protected static class TabSwitcher implements DropTargetListener {
    protected JTabbedPane tab;

    public TabSwitcher(JTabbedPane tab) {
      this.tab = tab;
    }

    @Override
    public void dragOver(DropTargetDragEvent e) {
      if (tab != null) {
        final Point p = e.getLocation();
        final int tabNumber = tab.getUI().tabForCoordinate(tab, p.x, p.y);
        if (tabNumber >= 0 && tabNumber != tab.getSelectedIndex()) {
          tab.setSelectedIndex(tabNumber);
          tab.repaint();
        }
      }
    }

    @Override
    public void dragEnter(DropTargetDragEvent e) {
    }

    @Override
    public void dropActionChanged(DropTargetDragEvent e) {
    }

    @Override
    public void drop(DropTargetDropEvent e) {
    }

    @Override
    public void dragExit(DropTargetEvent e) {
    }
  }

  public WidgetMap getMap() {
    return map;
  }
}
