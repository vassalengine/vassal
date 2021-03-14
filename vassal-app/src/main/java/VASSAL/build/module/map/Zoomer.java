/*
 *
 * Copyright (c) 2000-2008 by Rodney Kinney, Joel Uckelman
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
package VASSAL.build.module.map;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import javax.swing.AbstractListModel;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.JTextField;
import javax.swing.ListModel;
import javax.swing.ListSelectionModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.Command;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.LaunchButton;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.swing.SwingUtils;

/**
 * Controls the zooming in/out of a {@link Map} window.
 *
 * @author Joel Uckelman
 */
public class Zoomer extends AbstractConfigurable implements GameComponent {
  protected Map map;

  @Deprecated(since = "2020-08-06", forRemoval = true) protected double zoom = 1.0;
  @Deprecated(since = "2020-08-06", forRemoval = true) protected int zoomLevel = 0;
  @Deprecated(since = "2020-08-06", forRemoval = true) protected int zoomStart = 1;
  @Deprecated(since = "2020-08-06", forRemoval = true) protected double[] zoomFactor;
  @Deprecated(since = "2020-08-06", forRemoval = true) protected int maxZoom = 4;

  protected LaunchButton zoomInButton;
  protected LaunchButton zoomPickButton;
  protected LaunchButton zoomOutButton;
  protected ZoomMenu zoomMenu;

  protected MouseWheelListener listener;

  protected State state;

  // the default zoom levels are powers of 1.6
  protected static final double[] defaultZoomLevels = {
    1.0 / 1.6 / 1.6,
    1.0 / 1.6,
    1.0,
    1.6
  };

  protected static final int defaultInitialZoomLevel = 2;

  /**
   * Stores the state information for the {@link Zoomer}. This class
   * exists to keep the <code>Zoomer</code> data separate from the
   * <code>Zoomer</code> GUI.
   *
   * <p>Predefined zoom levels are stored in <code>levels</code>. If we are
   * in a predefined zoom level, then <code>custom == -1</code> and
   * <code>levels[cur]</code> is the current zoom factor. If we are in
   * a user-defined zoom level, then <code>custom</code> is the current
   * zoom factor and <code>cur</code> is the greatest value such that
   * {@code custom < level[cur]}}.</p>
   *
   * @author Joel Uckelman
   * @since 3.1.0
   */
  protected static class State {
    private double custom;
    private final double[] levels;
    private int cur;
    private final int initial;

    public State(double[] levels, int initial) {
      this.levels = levels;
      Arrays.sort(this.levels);

      cur = this.initial = initial;
      custom = -1;
    }

    public State(Collection<Double> l, int initial) {
      levels = new double[l.size()];

      int i = 0;
      for (final Double d : l) levels[i++] = d;
      Arrays.sort(levels);

      cur = this.initial = initial;
      custom = -1;
    }

    public double getZoom() {
      return custom < 0 ? levels[cur] : custom;
    }

    public void setZoom(double z) {
      if (z <= 0.0) {
        // This should never happen, it's just a kludge to make sure that
        // we continue having valid data even if our caller is wrong.
        z = Double.MIN_VALUE;
      }

      cur = Arrays.binarySearch(levels, z);
      if (cur < 0) {
        // if z is not a level, set cur to the next level > z
        cur = -cur - 1;

        // check whether we are close to a level
        if (cur < levels.length && Math.abs(z - levels[cur]) < 0.005) {
          custom = -1;
        }
        else if (cur > 0 && Math.abs(z - levels[cur - 1]) < 0.005) {
          --cur;
          custom = -1;
        }
        else {
          custom = z;
        }
      }
      else {
        // custom is negative when we are in a predefined zoom level
        custom = -1;
      }
    }

    public int getLevel() {
      return cur;
    }

    public void setLevel(int l) {
      cur = l;
      custom = -1;
    }

    public int getInitialLevel() {
      return initial;
    }

    public int getLevelCount() {
      return levels.length;
    }

    public boolean atLevel() {
      return custom < 0;
    }

    public void lowerLevel() {
      if (custom >= 0) custom = -1;
      --cur;
    }

    public void higherLevel() {
      if (custom < 0) ++cur;
      else custom = -1;
    }

    public boolean hasLowerLevel() {
      return cur > 0;
    }

    public boolean hasHigherLevel() {
      return custom < 0 ? cur < levels.length - 1 : cur < levels.length;
    }

    public List<Double> getLevels() {
      final ArrayList<Double> l = new ArrayList<>(levels.length);
      for (final double d : levels) l.add(d);
      return l;
    }
  }

  public Zoomer() {
    state = new State(defaultZoomLevels, defaultInitialZoomLevel);

    final ActionListener zoomIn = e -> zoomIn();

    final ActionListener zoomOut = e -> zoomOut();

    zoomMenu = new ZoomMenu();

    final ActionListener zoomPick = e -> {
      if (zoomPickButton.isShowing()) {
        zoomMenu.show(zoomPickButton, 0, zoomPickButton.getHeight());
      }
    };

    zoomPickButton = new LaunchButton(null, PICK_TOOLTIP, PICK_BUTTON_TEXT,
      ZOOM_PICK, PICK_ICON_NAME, zoomPick);
    zoomPickButton.setAttribute(PICK_TOOLTIP,
      Resources.getString("Zoomer.zoom_select")); //$NON-NLS-1$
    zoomPickButton.setAttribute(PICK_ICON_NAME, PICK_DEFAULT_ICON);

    zoomInButton = new LaunchButton(null, IN_TOOLTIP, IN_BUTTON_TEXT,
      ZOOM_IN, IN_ICON_NAME, zoomIn);
    zoomInButton.setAttribute(IN_TOOLTIP,
      Resources.getString("Zoomer.zoom_in")); //$NON-NLS-1$
    zoomInButton.setAttribute(IN_ICON_NAME, IN_DEFAULT_ICON);

    zoomOutButton = new LaunchButton(null, OUT_TOOLTIP, OUT_BUTTON_TEXT,
      ZOOM_OUT, OUT_ICON_NAME, zoomOut);
    zoomOutButton.setAttribute(OUT_TOOLTIP,
      Resources.getString("Zoomer.zoom_out")); //$NON-NLS-1$
    zoomOutButton.setAttribute(OUT_ICON_NAME, OUT_DEFAULT_ICON);

    setConfigureName(null);

    init();
  }

  protected void init() {
    zoomInButton.setEnabled(state.hasHigherLevel());
    zoomPickButton.setEnabled(true);
    zoomOutButton.setEnabled(state.hasLowerLevel());
    zoomMenu.initZoomItems();
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.Zoom.component_type"); //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeNames() {
    return new String[]{
      ZOOM_START,
      ZOOM_LEVELS,
      IN_TOOLTIP,
      IN_BUTTON_TEXT,
      IN_ICON_NAME,
      ZOOM_IN,
      PICK_TOOLTIP,
      PICK_BUTTON_TEXT,
      PICK_ICON_NAME,
      ZOOM_PICK,
      OUT_TOOLTIP,
      OUT_BUTTON_TEXT,
      OUT_ICON_NAME,
      ZOOM_OUT
    };
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[]{
      "", //$NON-NLS-1$
      Resources.getString("Editor.Zoom.preset"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.in_tooltip"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.in_button"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.in_icon"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.in_key"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.select_tooltip"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.select_button"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.select_icon"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.select_key"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.out_tooltip"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.out_button"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.out_icon"), //$NON-NLS-1$
      Resources.getString("Editor.Zoom.out_key"), //$NON-NLS-1$
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[]{
      null,   // ZOOM_START is handled by the LevelConfigurer
      LevelConfig.class,
      String.class,
      String.class,
      InIconConfig.class,
      NamedKeyStroke.class,
      String.class,
      String.class,
      PickIconConfig.class,
      NamedKeyStroke.class,
      String.class,
      String.class,
      OutIconConfig.class,
      NamedKeyStroke.class
    };
  }

  public static class InIconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new IconConfigurer(key, name, IN_DEFAULT_ICON);
    }
  }

  public static class PickIconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new IconConfigurer(key, name, PICK_DEFAULT_ICON);
    }
  }

  public static class OutIconConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new IconConfigurer(key, name, OUT_DEFAULT_ICON);
    }
  }

  public static class LevelConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new LevelConfigurer((Zoomer) c, key, name);
    }
  }

  /**
   * The {@link Configurer} for {@link #ZOOM_LEVELS} and {@link #ZOOM_START}.
   *
   * @author Joel Uckelman
   * @since 3.1.0
   */
  protected static class LevelConfigurer extends Configurer {
    private final Zoomer z;

    private final JPanel panel;
    private final LevelModel model;
    private final JList<String> levelList;
    private final JButton addButton;
    private final JButton removeButton;
    private final JButton initialButton;
    private final JTextField levelField;

    public LevelConfigurer(final Zoomer z, String key, String name) {
      super(key, name);
      this.z = z;

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));

      final Box leftBox = Box.createVerticalBox();
      final Box addBox = Box.createHorizontalBox();

      // Add button
      addButton = new JButton(Resources.getString(Resources.ADD));
      addButton.addActionListener(e -> addLevel());

      addButton.setEnabled(false);
      addBox.add(addButton);

      levelField = new JTextField(8);
      levelField.setMaximumSize(new Dimension(
        Integer.MAX_VALUE, levelField.getPreferredSize().height));

      // validator for the level entry field
      levelField.getDocument().addDocumentListener(new DocumentListener() {
        @Override
        public void changedUpdate(DocumentEvent e) { }

        @Override
        public void insertUpdate(DocumentEvent e) {
          validate();
        }

        @Override
        public void removeUpdate(DocumentEvent e) {
          validate();
        }

        private static final String PATTERN =
          "^(\\d*[1-9]\\d*(/\\d*[1-9]\\d*|\\.\\d*)?|0*\\.\\d*[1-9]\\d*)$"; //$NON-NLS-1$

        private void validate() {
          // valid entries match the pattern and aren't already in the list
          final String text = levelField.getText();
          addButton.setEnabled(text.matches(PATTERN) &&
            !z.state.getLevels().contains(parseLevel(text)));
        }
      });

      // rely on addButton to do the validation
      levelField.addActionListener(e -> {
        if (addButton.isEnabled()) addLevel();
      });

      addBox.add(levelField);

      leftBox.add(addBox);

      final Box buttonBox = Box.createHorizontalBox();

      // Remove button
      removeButton = new JButton(Resources.getString(Resources.REMOVE));
      removeButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          // get the zoom level index to be removed
          final int rm_level = levelList.getSelectedIndex();
          final List<Double> l = z.state.getLevels();

          final int new_init;
          if (rm_level == z.state.getInitialLevel()) {
            // we're deleting the initial level; keep it the same position
            new_init = Math.min(rm_level, z.state.getLevelCount() - 2);
            l.remove(rm_level);
          }
          else {
            // find the new index of the old initial level
            final Double old_init_val = l.get(z.state.getInitialLevel());
            l.remove(rm_level);
            new_init = l.indexOf(old_init_val);
          }

          // adjust the state
          z.state = new State(l, new_init);
          z.init();
          model.updateModel();

          // adjust the selection
          levelList.setSelectedIndex(
            Math.max(Math.min(rm_level, l.size() - 1), 0));
          updateButtons();
        }
      });

      buttonBox.add(removeButton);

      // Set Initial button
      initialButton = new JButton(Resources.getString("Editor.zoom.set_initial")); //$NON-NLS-1$
      initialButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {
          // set the new initial scale level
          final int i = levelList.getSelectedIndex();
          z.state = new State(z.state.getLevels(), i);
          z.init();
          model.updateModel();
          updateButtons();
        }
      });

      buttonBox.add(initialButton);

      leftBox.add(buttonBox);

      final JLabel explanation =
        new JLabel(Resources.getString("Editor.zoom.initial_zoom")); //$NON-NLS-1$
      explanation.setAlignmentX(JLabel.CENTER_ALIGNMENT);

      leftBox.add(
        Box.createVerticalStrut(explanation.getPreferredSize().height));
      leftBox.add(explanation);
      leftBox.add(
        Box.createVerticalStrut(explanation.getPreferredSize().height));

      // level list
      model = new LevelModel();
      levelList = new JList<>(model);
      levelList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      levelList.setSelectedIndex(0);

      levelList.addListSelectionListener(e -> updateButtons());

      final JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
      pane.setLeftComponent(leftBox);
      pane.setRightComponent(new JScrollPane(levelList));

      panel.add(pane);
      panel.setBorder(new TitledBorder(name));
      updateButtons();
    }

    /**
     * Parse a <code>String</code> to a <code>double</code>.
     * Accepts fractions as "n/d".
     */
    protected double parseLevel(String text) {
      final String[] s = text.split("/"); //$NON-NLS-1$
      try {
        return s.length > 1 ?
          Double.parseDouble(s[0]) / Double.parseDouble(s[1]) :
          Double.parseDouble(s[0]);
      }
      catch (final NumberFormatException ex) {
        // should not happen, text already validated
        ErrorDialog.bug(ex);
      }
      return 0.0;
    }

    /**
     * Add a level to the level list. This method expects that the
     * input has already been validated.
     */
    protected void addLevel() {
      // get the initial scale level
      final List<Double> l = z.state.getLevels();
      final Double old_init_val = l.get(z.state.getInitialLevel());

      // add the new scale level
      final double new_level_val = parseLevel(levelField.getText());
      l.add(new_level_val);
      Collections.sort(l);

      // find the initial scale index
      final int new_init = l.indexOf(old_init_val);

      // adjust the state
      z.state = new State(l, new_init);
      z.init();
      model.updateModel();

      // adjust the selection
      final int new_level = l.indexOf(new_level_val);
      levelList.setSelectedIndex(new_level);

      levelField.setText("");
      updateButtons();
    }

    /**
     * Ensures that the buttons are properly en- or disabled.
     */
    protected void updateButtons() {
      removeButton.setEnabled(z.state.getLevelCount() > 1);
      initialButton.setEnabled(
        levelList.getSelectedIndex() != z.state.getInitialLevel());
    }

    /**
     * A {@link ListModel} built on the {@link State}.
     */
    protected class LevelModel extends AbstractListModel<String> {
      private static final long serialVersionUID = 1L;

      public void updateModel() {
        fireContentsChanged(this, 0, z.state.getLevelCount() - 1);
      }

      @Override
      public String getElementAt(int i) {
        return z.state.getLevels().get(i) +
          (z.state.getInitialLevel() == i ? " *" : ""); //$NON-NLS-1$ //$NON-NLS-2$
      }

      @Override
      public int getSize() {
        return z.state.getLevelCount();
      }
    }

    @Override
    public Component getControls() {
      return panel;
    }

    @Override
    public void setValue(Object o) {
    }

    @Override
    public void setValue(String s) {
    }

    @Override
    public String getValueString() {
      return null;
    }
  }

  protected static final String ZOOM_START = "zoomStart"; //$NON-NLS-1$
  protected static final String ZOOM_LEVELS = "zoomLevels"; //$NON-NLS-1$

  protected static final String ZOOM_IN = "zoomInKey"; //$NON-NLS-1$
  protected static final String IN_TOOLTIP = "inTooltip"; //$NON-NLS-1$
  protected static final String IN_BUTTON_TEXT = "inButtonText"; //$NON-NLS-1$
  protected static final String IN_ICON_NAME = "inIconName"; //$NON-NLS-1$
  protected static final String IN_DEFAULT_ICON = "/images/zoomIn.gif"; //$NON-NLS-1$

  protected static final String ZOOM_PICK = "zoomPickKey"; //$NON-NLS-1$
  protected static final String PICK_TOOLTIP = "pickTooltip"; //$NON-NLS-1$
  protected static final String PICK_BUTTON_TEXT = "pickButtonText"; //$NON-NLS-1$
  protected static final String PICK_ICON_NAME = "pickIconName"; //$NON-NLS-1$
  protected static final String PICK_DEFAULT_ICON = "/images/zoom.png"; //$NON-NLS-1$

  protected static final String ZOOM_OUT = "zoomOutKey"; //$NON-NLS-1$
  protected static final String OUT_TOOLTIP = "outTooltip"; //$NON-NLS-1$
  protected static final String OUT_BUTTON_TEXT = "outButtonText"; //$NON-NLS-1$
  protected static final String OUT_ICON_NAME = "outIconName"; //$NON-NLS-1$
  protected static final String OUT_DEFAULT_ICON = "/images/zoomOut.gif"; //$NON-NLS-1$

  @Override
  public void addTo(Buildable b) {
    GameModule.getGameModule().getGameState().addGameComponent(this);

    map = (Map) b;

    validator = new SingleChildInstance(map, getClass());

    map.setZoomer(this);
    map.getToolBar().add(zoomInButton);
    map.getToolBar().add(zoomPickButton);
    map.getToolBar().add(zoomOutButton);

    // Ctrl+Mousewheel to zoom in/out.
    listener = e -> {
      if (e.getScrollAmount() == 0) {
        return;
      }

      if ((e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) && SwingUtils.isSelectionToggle(e)) {
        final int units = e.getUnitsToScroll();

        if ((units < 0) && state.hasHigherLevel()) {
          zoomIn();
        }
        else if ((units > 0) && state.hasLowerLevel()) {
          zoomOut();
        }
      }

      map.getComponent().getParent().dispatchEvent(e); // So that the scrollbars can still find our event.
    };

    map.getComponent().addMouseWheelListener(listener);
  }


  @Override
  public String getAttributeValueString(String key) {
    if (ZOOM_START.equals(key)) {
      // Notes:
      //
      // 1. ZOOM_START is one-based, not zero-based.
      // 2. The levels in state run from zoomed out to zoomed in,
      // while the levels coming from outside Zoomer run from
      // zoomed in to zoomed out. Hence we reverse the initial
      // zoom level being returned here.
      //
      return String.valueOf(state.getLevelCount() - state.getInitialLevel());
    }
    else if (ZOOM_LEVELS.equals(key)) {
      final List<Double> levels = state.getLevels();
      final String[] s = new String[levels.size()];
      for (int i = 0; i < s.length; ++i) {
        s[i] = levels.get(i).toString();
      }

      return StringArrayConfigurer.arrayToString(s);
    }

    else if (zoomInButton.getAttributeValueString(key) != null) {
      return zoomInButton.getAttributeValueString(key);
    }
    else if (zoomPickButton.getAttributeValueString(key) != null) {
      return zoomPickButton.getAttributeValueString(key);
    }
    else {
      return zoomOutButton.getAttributeValueString(key);
    }
  }

  @Override
  public void setAttribute(String key, Object val) {
    if (ZOOM_START.equals(key)) {
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }

      if (val != null) {
        // Notes:
        //
        // 1. ZOOM_START is one-based, not zero-based.
        // 2. The levels in state run from zoomed out to zoomed in,
        // while the levels coming from outside Zoomer run from
        // zoomed in to zoomed out. Hence we reverse the initial
        // zoom level being set here.
        //
        final List<Double> levels = state.getLevels();
        final int initial =
          Math.max(0, Math.min(levels.size() - 1, levels.size() - (Integer) val));

        state = new State(levels, initial);

        if (deprecatedFactor > 0 && deprecatedMax > 0) {
          // zero these to prevent further adjustments due to old properties
          deprecatedFactor = 0.0;
          deprecatedMax = 0;
        }

        init();
      }
    }
    else if (ZOOM_LEVELS.equals(key)) {
      if (val instanceof String) {
        val = StringArrayConfigurer.stringToArray((String) val);
      }

      if (val != null) {
        // dump into a set to remove duplicates
        final HashSet<Double> levels = new HashSet<>();
        for (final String s : (String[]) val) {
          levels.add(Double.valueOf(s));
        }

        state = new State(levels,
          Math.min(state.getInitialLevel(), levels.size() - 1));
        init();
      }
    }
    else if (FACTOR.equals(key)) {  // deprecated key
      if (val instanceof String) {
        val = Double.valueOf((String) val);
      }

      if (val != null) {
        deprecatedFactor = (Double) val;

        if (deprecatedFactor > 0 && deprecatedMax > 0) {
          adjustStateForFactorAndMax();
        }
      }
    }
    else if (MAX.equals(key)) {     // deprecated key
      if (val instanceof String) {
        val = Integer.valueOf((String) val);
      }

      if (val != null) {
        deprecatedMax = (Integer) val;

        if (deprecatedFactor > 0 && deprecatedMax > 0) {
          adjustStateForFactorAndMax();
        }
      }
    }
    else {
      // FIXME: does having this as an extremal case cause weird behavior for
      // unrecognized keys?
      zoomInButton.setAttribute(key, val);
      zoomPickButton.setAttribute(key, val);
      zoomOutButton.setAttribute(key, val);
    }
  }

  // begin deprecated keys
  private static final String FACTOR = "factor"; //$NON-NLS-1$
  private static final String MAX = "max"; //$NON-NLS-1$

  private int deprecatedMax = -1;
  private double deprecatedFactor = -1.0;

  private void adjustStateForFactorAndMax() {
    final double[] levels = new double[deprecatedMax + 1];
    for (int i = 0; i < levels.length; ++i)
      levels[i] = Math.pow(deprecatedFactor, -(i - 1));
    final int initial = Math.min(state.getInitialLevel(), levels.length - 1);
    state = new State(levels, initial);
    init();
  }
  // end deprecated keys

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.setZoomer(null);
    map.getToolBar().remove(zoomInButton);
    map.getToolBar().remove(zoomPickButton);
    map.getToolBar().remove(zoomOutButton);

    if (listener != null) {
      map.getComponent().removeMouseWheelListener(listener);
      listener = null;
    }
  }

  public double getZoomFactor() {
    return state.getZoom();
  }

  protected Point getMapCenter() {
    final Rectangle r = map.getView().getVisibleRect();
    return map.componentToMap(new Point(r.x + r.width / 2, r.y + r.height / 2));
  }

  protected void updateZoomer(Point center) {
    zoomInButton.setEnabled(state.hasHigherLevel());
    zoomOutButton.setEnabled(state.hasLowerLevel());

    zoomMenu.updateZoom();

    final Dimension d = map.getPreferredSize();
    map.getView().setBounds(0, 0, d.width, d.height);  // calls revalidate()

    map.centerAt(center);
    map.repaint(true);
  }

  public void setZoomLevel(int l) {
    final Point center = getMapCenter();
    state.setLevel(l);
    updateZoomer(center);
  }

  public void setZoomFactor(double z) {
    final Point center = getMapCenter();
    state.setZoom(z);
    updateZoomer(center);
  }

  public void zoomIn() {
    if (state.hasHigherLevel()) {
      final Point center = getMapCenter();
      state.higherLevel();
      updateZoomer(center);
    }
  }

  public void zoomOut() {
    if (state.hasLowerLevel()) {
      final Point center = getMapCenter();
      state.lowerLevel();
      updateZoomer(center);
    }
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.html", "Zoom"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  @Override
  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      zoomInButton.setEnabled(state.hasHigherLevel());
      zoomOutButton.setEnabled(state.hasLowerLevel());
    }

    zoomPickButton.setEnabled(gameStarting);
  }

  @Override
  public Command getRestoreCommand() {
    return null;
  }

  /**
   * The menu which displays zoom levels.
   *
   * @author Joel Uckelman
   * @since 3.1.0
   */
  protected class ZoomMenu extends JPopupMenu implements ActionListener {
    protected final JRadioButtonMenuItem other;
    protected final JPopupMenu.Separator sep;
    protected final ButtonGroup bg;

    private static final String OTHER = "Other..."; //$NON-NLS-1$
    private static final String FIT_WIDTH = "Fit Width"; //$NON-NLS-1$
    private static final String FIT_HEIGHT = "Fit Height"; //$NON-NLS-1$
    private static final String FIT_VISIBLE = "Fit Visible";  //$NON-NLS-1$

    private static final long serialVersionUID = 1L;

    public ZoomMenu() {
      super();

      sep = new JPopupMenu.Separator();
      add(sep);

      bg = new ButtonGroup();

      other = new JRadioButtonMenuItem(
        Resources.getString("Zoomer.ZoomMenu.other")); //$NON-NLS-1$
      other.setActionCommand(OTHER);
      other.addActionListener(this);
      bg.add(other);
      add(other);

      addSeparator();

      final JMenuItem fw = new JMenuItem(
        Resources.getString("Zoomer.ZoomMenu.fit_width")); //$NON-NLS-1$
      fw.setActionCommand(FIT_WIDTH);
      fw.addActionListener(this);
      add(fw);

      final JMenuItem fh = new JMenuItem(
        Resources.getString("Zoomer.ZoomMenu.fit_height")); //$NON-NLS-1$
      fh.setActionCommand(FIT_HEIGHT);
      fh.addActionListener(this);
      add(fh);

      final JMenuItem fv = new JMenuItem(
        Resources.getString("Zoomer.ZoomMenu.fit_visible")); //$NON-NLS-1$
      fv.setActionCommand(FIT_VISIBLE);
      fv.addActionListener(this);
      add(fv);
    }

    public void initZoomItems() {
      while (getComponent(0) != sep) remove(0);

      final List<Double> levels = state.getLevels();
      for (int i = 0; i < levels.size(); ++i) {
        final String zs = Math.round(levels.get(i) * 100) + "%"; //$NON-NLS-1$
        final JMenuItem item = new JRadioButtonMenuItem(zs.intern());
        item.setActionCommand(Integer.toString(i).intern());
        item.addActionListener(this);
        bg.add(item);
        insert(item, 0);
      }

      ((JRadioButtonMenuItem) getComponent(
        state.getLevelCount() - state.getLevel() - 1)).setSelected(true);
    }

    @Override
    public void actionPerformed(ActionEvent a) {
      try {
        setZoomLevel(Integer.parseInt(a.getActionCommand()));
        return;
      }
      catch (final NumberFormatException ignored) {
      }

      final String cmd = a.getActionCommand();

      if (OTHER.equals(cmd)) {
        final ZoomDialog dialog = new ZoomDialog((Frame)
          SwingUtilities.getAncestorOfClass(Frame.class, map.getView()),
          Resources.getString("Zoomer.ZoomDialog.title"), true); //$NON-NLS-1$
        dialog.setVisible(true);

        final double z = dialog.getResult() / 100.0;
        if (z > 0 && z != state.getZoom()) {
          setZoomFactor(z);
        }
      }
      // FIXME: should be map.getSize() for consistency?
      else if (FIT_WIDTH.equals(cmd)) {
        final Dimension vd = map.getView().getVisibleRect().getSize();
        final Dimension md = map.mapSize();
        setZoomFactor(vd.getWidth() / md.getWidth());
      }
      else if (FIT_HEIGHT.equals(cmd)) {
        final Dimension vd = map.getView().getVisibleRect().getSize();
        final Dimension md = map.mapSize();
        setZoomFactor(vd.getHeight() / md.getHeight());
      }
      else if (FIT_VISIBLE.equals(cmd)) {
        final Dimension vd = map.getView().getVisibleRect().getSize();
        final Dimension md = map.mapSize();
        setZoomFactor(Math.min(vd.getWidth() / md.getWidth(),
                               vd.getHeight() / md.getHeight()));
      }
      else {
        // this should not happen!
        assert false;
      }
    }

    public void updateZoom() {
      if (state.atLevel()) {
        ((JRadioButtonMenuItem) getComponent(
          state.getLevelCount() - state.getLevel() - 1)).setSelected(true);
      }
      else {
        other.setSelected(true);
      }
    }
  }

  /**
   * The dialog for setting custom zoom levels.
   *
   * @author Joel Uckelman
   * @since 3.1.0
   */
  protected class ZoomDialog extends JDialog
                             implements ActionListener,
                                        ChangeListener {
    protected double result;
    protected final JSpinner ratioNumeratorSpinner;
    protected final JSpinner ratioDenominatorSpinner;
    protected final JSpinner percentSpinner;
    protected final SpinnerNumberModel ratioNumeratorModel;
    protected final SpinnerNumberModel ratioDenominatorModel;
    protected final SpinnerNumberModel percentModel;
    protected final JButton okButton;

    private static final long serialVersionUID = 1L;

    public ZoomDialog(Frame owner, String title, boolean modal) {
      super(owner, title, modal);

      final int hsep = 5;

      final JPanel controlsPane = new JPanel(new GridBagLayout());
      final GridBagConstraints c = new GridBagConstraints();
      c.fill = GridBagConstraints.HORIZONTAL;

      final Insets linset = new Insets(0, 0, 11, 11);
      final Insets dinset = new Insets(0, 0, 0, 0);

      final JLabel ratioLabel = new JLabel(
        Resources.getString("Zoomer.ZoomDialog.zoom_ratio")); //$NON-NLS-1$
      c.gridx = 0;
      c.gridy = 0;
      c.weightx = 0;
      c.weighty = 0;
      c.insets = linset;
      c.anchor = GridBagConstraints.LINE_START;
      controlsPane.add(ratioLabel, c);

      final Box ratioBox = new Box(BoxLayout.X_AXIS);
      ratioLabel.setLabelFor(ratioBox);
      c.gridx = 1;
      c.gridy = 0;
      c.weightx = 1;
      c.weighty = 0;
      c.insets = dinset;
      c.anchor = GridBagConstraints.LINE_START;
      controlsPane.add(ratioBox, c);

      ratioNumeratorModel = new SpinnerNumberModel(1, 1, 256, 1);
      ratioNumeratorSpinner = new JSpinner(ratioNumeratorModel);
      ratioNumeratorSpinner.addChangeListener(this);
      ratioBox.add(ratioNumeratorSpinner);

      ratioBox.add(Box.createHorizontalStrut(hsep));

      final JLabel ratioColon = new JLabel(":"); //$NON-NLS-1$
      ratioBox.add(ratioColon);

      ratioBox.add(Box.createHorizontalStrut(hsep));

      ratioDenominatorModel = new SpinnerNumberModel(1, 1, 256, 1);
      ratioDenominatorSpinner = new JSpinner(ratioDenominatorModel);
      ratioDenominatorSpinner.addChangeListener(this);
      ratioBox.add(ratioDenominatorSpinner);

      final JLabel percentLabel = new JLabel(
        Resources.getString("Zoomer.ZoomDialog.zoom_percent")); //$NON-NLS-1$
      c.gridx = 0;
      c.gridy = 1;
      c.weightx = 0;
      c.weighty = 0;
      c.insets = linset;
      c.anchor = GridBagConstraints.LINE_START;
      controlsPane.add(percentLabel, c);

      final Box percentBox = new Box(BoxLayout.X_AXIS);
      c.gridx = 1;
      c.gridy = 1;
      c.weightx = 1;
      c.weighty = 0;
      c.insets = dinset;
      c.anchor = GridBagConstraints.LINE_START;
      controlsPane.add(percentBox, c);

      percentModel =
        new SpinnerNumberModel(state.getZoom() * 100.0, 0.39, 25600.0, 10.0);
      percentSpinner = new JSpinner(percentModel);
      percentLabel.setLabelFor(percentSpinner);
      percentSpinner.addChangeListener(this);
      percentBox.add(percentSpinner);

      updateRatio();

      percentBox.add(Box.createHorizontalStrut(hsep));
      final JLabel percentSign = new JLabel("%"); //$NON-NLS-1$
      percentBox.add(percentSign);

      // buttons
      final Box buttonBox = new Box(BoxLayout.X_AXIS);
      buttonBox.add(Box.createHorizontalGlue());

      okButton = new JButton(Resources.getString(Resources.OK));
      okButton.addActionListener(this);
      getRootPane().setDefaultButton(okButton);
      buttonBox.add(okButton);

      buttonBox.add(Box.createHorizontalStrut(hsep));

      final JButton cancelButton = new JButton(
        Resources.getString(Resources.CANCEL));
      cancelButton.addActionListener(this);
      buttonBox.add(cancelButton);

      final Dimension okDim = okButton.getPreferredSize();
      final Dimension cancelDim = cancelButton.getPreferredSize();
      final Dimension buttonDimension = new Dimension(
        Math.max(okDim.width,  cancelDim.width),
        Math.max(okDim.height, cancelDim.height));
      okButton.setPreferredSize(buttonDimension);
      cancelButton.setPreferredSize(buttonDimension);

      final JComponent contentPane = (JComponent) getContentPane();
      contentPane.setBorder(new EmptyBorder(12, 12, 11, 11));
      contentPane.setLayout(new BorderLayout(0, 11));
      contentPane.add(controlsPane, BorderLayout.CENTER);
      contentPane.add(buttonBox, BorderLayout.PAGE_END);

      setResizable(false);
      pack();
    }

    public double getResult() {
      return result;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
      result = e.getSource() == okButton ?
               percentModel.getNumber().doubleValue() : 0.0;

      setVisible(false);
    }

    @Override
    public void stateChanged(ChangeEvent e) {
      if (e.getSource() == ratioNumeratorSpinner ||
          e.getSource() == ratioDenominatorSpinner) {
        updatePercent();
      }
      else if (e.getSource() == percentSpinner) {
        updateRatio();
      }
    }

    private void updatePercent() {
      // disconnect listener to prevent circularity
      percentSpinner.removeChangeListener(this);

      percentModel.setValue(
        ratioNumeratorModel.getNumber().doubleValue() /
        ratioDenominatorModel.getNumber().doubleValue() * 100.0
      );

      percentSpinner.addChangeListener(this);
    }

    private void updateRatio() {
      // Warning: Heavy maths ahead!
      //
      // This algorithm borrowed from gimpzoommodel.c in LIBGIMP:
      // http://svn.gnome.org/viewcvs/gimp/trunk/libgimpwidgets/gimpzoommodel.c
      //
      // See also http://www.virtualdub.org/blog/pivot/entry.php?id=81
      // for a discussion of calculating continued fractions by convergeants.

      double z = percentModel.getNumber().doubleValue() / 100.0;

      // we want symmetric behavior, so find reciprocal when zooming out
      boolean swapped = false;
      if (z < 1.0) {
        z = 1.0 / z;
        swapped = true;
      }

      // calculate convergeants
      int p0 = 1;
      int q0 = 0;
      int p1 = (int)Math.floor(z);
      int q1 = 1;
      int p2;
      int q2;

      double r = z - p1;
      double next_cf;

      while (Math.abs(r) >= 0.0001 &&
             Math.abs((double)p1 / q1 - z) > 0.0001) {
        r = 1.0 / r;
        next_cf = Math.floor(r);

        p2 = (int)(next_cf * p1 + p0);
        q2 = (int)(next_cf * q1 + q0);

        // We limit the numerator and denominator to be 256 or less,
        // and also exclude absurd ratios like 170:171.
        if (p2 > 256 || q2 > 256 || (p2 > 1 && q2 > 1 && p2 * q2 > 200))
          break;

        // remember the last two fractions
        p0 = p1;
        p1 = p2;
        q0 = q1;
        q1 = q2;

        r -= next_cf;
      }

      z = (double)p1 / q1;

      // hard upper and lower bounds for zoom ratio
      if (z > 256.0) {
        p1 = 256;
        q1 = 1;
      }
      else if (z < 1.0 / 256.0) {
        p1 = 1;
        q1 = 256;
      }

      if (swapped) {
        final int tmp = p1;
        p1 = q1;
        q1 = tmp;
      }

      // disconnect listeners to prevent circularity
      ratioNumeratorSpinner.removeChangeListener(this);
      ratioDenominatorSpinner.removeChangeListener(this);

      ratioNumeratorModel.setValue(p1);
      ratioDenominatorModel.setValue(q1);

      ratioNumeratorSpinner.addChangeListener(this);
      ratioDenominatorSpinner.addChangeListener(this);
    }
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of the Configurables string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    return Collections.emptyList();
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Message Format strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    return Collections.emptyList();
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Property Names referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Collections.emptyList();
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Menu/Button/Tooltip Text strings referenced in the Configurable, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(getAttributeValueString(IN_BUTTON_TEXT), getAttributeValueString(IN_TOOLTIP),
                   getAttributeValueString(OUT_BUTTON_TEXT), getAttributeValueString(OUT_TOOLTIP),
                   getAttributeValueString(PICK_BUTTON_TEXT), getAttributeValueString(PICK_TOOLTIP));
  }

  /**
   * {@link VASSAL.search.SearchTarget}
   * @return a list of any Named KeyStrokes referenced in the Configurable, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(NamedHotKeyConfigurer.decode(getAttributeValueString(ZOOM_IN)),
                         NamedHotKeyConfigurer.decode(getAttributeValueString(ZOOM_OUT)),
                         NamedHotKeyConfigurer.decode(getAttributeValueString(ZOOM_PICK)));
  }

  /**
   * Add our icons to the list of referenced images
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    final String [] iconAttr = { PICK_ICON_NAME, IN_ICON_NAME, OUT_ICON_NAME };
    for (final String i : iconAttr) {
      final String f = getAttributeValueString(i);
      if (f != null) {
        s.add(f);
      }
    }
  }
}
