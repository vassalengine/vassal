/*
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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Window;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.WindowConstants;

import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.Widget;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.widget.BoxWidget;
import VASSAL.build.widget.ListWidget;
import VASSAL.build.widget.PanelWidget;
import VASSAL.build.widget.PieceSlot;
import VASSAL.build.widget.TabWidget;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.counters.GamePiece;
import VASSAL.i18n.Resources;
import VASSAL.preferences.PositionOption;
import VASSAL.preferences.VisibilityOption;
import VASSAL.tools.KeyStrokeSource;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.menu.MenuManager;
import VASSAL.tools.swing.SplitPane;
import VASSAL.tools.swing.SwingUtils;

/**
 * A window from which players can create new {@link GamePiece}s by
 * clicking and dragging from the PieceWindow.  The actual GamePieces
 * are contained in {@link PieceSlot} components.  PieceWindow extends
 * {@link Widget}, so it may be composed of various tabs, lists, etc.  */
public class PieceWindow extends Widget implements UniqueIdManager.Identifyable {
  protected String id;
  protected LaunchButton launch;
  protected boolean hidden;
  public static final String DEPRECATED_NAME = "entryName"; //$NON-NLS-1$
  public static final String NAME = "name"; //$NON-NLS-1$
  public static final String BUTTON_TEXT = "text"; //$NON-NLS-1$
  public static final String TOOLTIP = "tooltip"; //$NON-NLS-1$
  public static final String ICON = "icon"; //$NON-NLS-1$
  public static final String HOTKEY = "hotkey"; //$NON-NLS-1$
  public static final String HIDDEN = "hidden"; //$NON-NLS-1$
  public static final String SCALE = "scale"; //$NON-NLS-1$
  protected static final UniqueIdManager idMgr = new UniqueIdManager(PieceWindow.class.getSimpleName());
  protected JComponent root;
  protected String tooltip = ""; //$NON-NLS-1$
  protected double scale;

  @SuppressWarnings({"deprecation", "removal"})
  @Deprecated(since = "2020-11-15", forRemoval = true)
  protected VASSAL.tools.ComponentSplitter.SplitPane mainWindowDock;

  protected SplitPane splitPane;

  private static final String firstId = PieceWindow.class.getSimpleName() + '0';

  public PieceWindow() {
    root = new JPanel(new BorderLayout());

    launch = new LaunchButton(
      Resources.getString("Editor.PieceWindow.pieces"),
      TOOLTIP,
      BUTTON_TEXT,
      HOTKEY,
      ICON,
      e -> launchButtonPressed()
    );
    launch.setToolTipText(
      Resources.getString("Editor.PieceWindow.show_hide_pieces_window",
      Resources.getString("Editor.PieceWindow.pieces"))
    );

    scale = 1.0;
  }

  @Override
  public boolean hasScale() {
    return true;
  }

  @Override
  public double getScale() {
    return scale;
  }

  private Window initFrame() {
    if (GlobalOptions.getInstance().isUseSingleWindow()) {
      final JDialog d = new JDialog(GameModule.getGameModule().getPlayerWindow());
      d.add(root);
      d.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
      d.setTitle(getConfigureName());
      addPropertyChangeListener(e -> {
        if (Configurable.NAME_PROPERTY.equals(e.getPropertyName())) {
          d.setTitle((String) e.getNewValue());
        }
      });
      return d;
    }
    else {
      final JFrame d = new JFrame();
      d.add(root);
      d.setDefaultCloseOperation(WindowConstants.HIDE_ON_CLOSE);
      d.setTitle(getConfigureName());
      d.setJMenuBar(MenuManager.getInstance().getMenuBarFor(d));

      addPropertyChangeListener(e -> {
        if (Configurable.NAME_PROPERTY.equals(e.getPropertyName())) {
          d.setTitle((String) e.getNewValue());
        }
      });

      return d;
    }
  }

  public void launchButtonPressed() {
    if (splitPane != null) {
      splitPane.toggleLeft();
    }
    else {
      root.getTopLevelAncestor().setVisible(!root.getTopLevelAncestor().isVisible());
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PieceWindow.html"); //$NON-NLS-1$
  }

  @Override
  public void build(org.w3c.dom.Element e) {
    super.build(e);
    rebuild();
  }

  public boolean shouldDockIntoMainWindow() {
    return firstId.equals(id); //$NON-NLS-1$
  }

  @Override
  public Component getComponent() {
    return root;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.PieceWindow.component_type"); //$NON-NLS-1$
  }

  /**
   * Children of PieceWindow may contain a {@link TabWidget}, a {@link
   * PanelWidget}, a {@link BoxWidget}, a {@link ListWidget}, or a
   * {@link PieceSlot}
   **/
  @Override
  public Class<?>[] getChildAllowableConfigureComponents() {
    return new Class<?>[]{
      TabWidget.class,
      PanelWidget.class,
      BoxWidget.class,
      ListWidget.class,
      PieceSlot.class
    };
  }

  /**
   * A PieceWindow may contain a {@link TabWidget}, a {@link
   * PanelWidget}, a {@link BoxWidget}, a {@link ListWidget} -- but not a lone piece slot */
  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[]{
      TabWidget.class,
      PanelWidget.class,
      BoxWidget.class,
      ListWidget.class
    };
  }

  @Override
  public void add(Buildable b) {
    if (b instanceof Widget) {
      root.add(((Widget) b).getComponent());
    }
    super.add(b);
  }

  @Override
  public void remove(Buildable b) {
    if (b instanceof Widget) {
      root.remove(((Widget) b).getComponent());
    }
    super.remove(b);
  }

  /**
   * Each instanceof PieceWindow has a unique String identifier
   *
   * @return the identifier for this PieceWindow
   */
  @Override
  public String getId() {
    return id;
  }

  /**
   * Each instanceof PieceWindow has a unique String identifier
   */
  @Override
  public void setId(String s) {
    id = s;
  }

  /**
   * Docks us into the main window -- needs to be called AFTER Chatter has docked.
   */
  public void dockMe() {
    final Component controlPanel = GameModule.getGameModule().getControlPanel();
    final Container cppar = controlPanel.getParent();
    final int i = SwingUtils.getIndexInParent(controlPanel, cppar);

    splitPane = new SplitPane(SplitPane.HORIZONTAL_SPLIT, root, controlPanel);
    splitPane.hideLeft();
    cppar.add(splitPane, i);
  }

  /**
   * Expects to be added to a {@link GameModule}.  When added, sets
   * the containing window to visible */
  @Override
  public void addTo(Buildable parent) {
    idMgr.add(this);

    if (!hidden) {
      if (firstId.equals(id) && GlobalOptions.getInstance().isUseSingleWindow()) {
        // Register as the docked PieceWindow
        GameModule.getGameModule().setPieceWindow(this);
      }
      else {
        final String key = PositionOption.key + getConfigureName();
        final Window w = initFrame();
        final PositionOption pos = new VisibilityOption(key, w);
        GameModule.getGameModule().getPrefs().addOption(pos);

        GameModule.getGameModule().addKeyStrokeSource(new KeyStrokeSource(root, JComponent.WHEN_IN_FOCUSED_WINDOW));
      }
      GameModule.getGameModule().getToolBar().add(launch);
    }

    setAttributeTranslatable(NAME, false);
  }

  @Override
  public void removeFrom(Buildable parent) {
    if (splitPane == null && root != null && root.getTopLevelAncestor() != null) {
      root.getTopLevelAncestor().setVisible(false);
    }
    GameModule.getGameModule().getToolBar().remove(launch);
    idMgr.remove(this);
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      Resources.getString(Resources.NAME_LABEL),
      Resources.getString("Editor.PieceWindow.hidden"), //$NON-NLS-1$
      Resources.getString(Resources.BUTTON_TEXT),
      Resources.getString(Resources.TOOLTIP_TEXT),
      Resources.getString(Resources.BUTTON_ICON),
      Resources.getString("Editor.PieceWindow.show_hide"), //$NON-NLS-1$
      Resources.getString("Editor.PieceWindow.scale")
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      String.class,
      Boolean.class,
      String.class,
      String.class,
      IconConfig.class,
      NamedKeyStroke.class,
      Double.class
    };
  }

  public static class IconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, "/images/counter.gif"); //$NON-NLS-1$
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{NAME, HIDDEN, BUTTON_TEXT, TOOLTIP, ICON, HOTKEY, SCALE };
  }

  @Override
  public void setAttribute(String name, Object value) {
    if (DEPRECATED_NAME.equals(name)) {
      setAttribute(NAME, value);
      setAttribute(BUTTON_TEXT, value);
    }
    else if (NAME.equals(name)) {
      final String s = (String) value;
      setConfigureName(s);
      if (tooltip.isEmpty()) {
        launch.setToolTipText(Resources.getString("Editor.PieceWindow.show_hide_pieces_window", s));
      }
    }
    else if (HIDDEN.equals(name)) {
      if (value instanceof String) {
        value = Boolean.valueOf((String)value);
      }
      hidden = (Boolean) value;
    }
    else if (SCALE.equals(name)) {
      if (value instanceof String) {
        value = Double.valueOf((String)value);
      }
      scale = (Double) value;
      if (scale < 0.01) { //BR// Just gonna go with some sanity.
        scale = 0.01;
      }
      else if (scale >= 4) {
        scale = 4.0;
      }
    }
    else if (TOOLTIP.equals(name)) {
      tooltip = (String) value;
      launch.setAttribute(name, value);
    }
    else {
      launch.setAttribute(name, value);
    }
  }


  @Override
  public String getAttributeValueString(String name) {
    if (NAME.equals(name)) {
      return getConfigureName();
    }
    else if (HIDDEN.equals(name)) {
      return String.valueOf(hidden);
    }
    else if (SCALE.equals(name)) {
      return String.valueOf(scale);
    }
    else if (TOOLTIP.equals(name)) {
      return tooltip.isEmpty() ? launch.getAttributeValueString(name) : tooltip;
    }
    else {
      return launch.getAttributeValueString(name);
    }
  }


  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(getAttributeValueString(BUTTON_TEXT), getAttributeValueString(TOOLTIP));
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(HOTKEY)));
  }

  @Override
  public void addLocalImageNames(Collection<String> s) {
    final String fileName = launch.getAttributeValueString(ICON);
    if (fileName != null) {
      s.add(fileName);
    }
  }
}
