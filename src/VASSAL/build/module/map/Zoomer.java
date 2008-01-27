/*
 * $Id$
 *
 * Copyright (c) 2000-2007 by Rodney Kinney, Joel Uckelman
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
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.Board;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.SingleChildInstance;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.LaunchButton;

/**
 * Controls the zooming in/out of a {@link Map} window.
 */
public class Zoomer extends AbstractConfigurable implements GameComponent {
  protected Map map;

  @Deprecated protected double zoom = 1.0;
  @Deprecated protected int zoomLevel = 0;
  @Deprecated protected int zoomStart = 1;
  @Deprecated protected double[] zoomFactor;
  @Deprecated protected int maxZoom = 4;

  protected LaunchButton zoomInButton;
  protected LaunchButton zoomPickButton;
  protected LaunchButton zoomOutButton;
  protected ZoomMenu zoomMenu;

  protected State state;

  // the default zoom levels are powers of 1.6
  protected static final double[] defaultZoomLevels = new double[] {
    1.0/1.6/1.6,
    1.0/1.6,
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
  
    public double getZoom() {
      return custom < 0 ? levels[cur] : custom;
    }  

    public void setZoom(double z) {
      cur = Arrays.binarySearch(levels, z);
      if (cur < 0) {
        // if z is not a level, set cur to the next level > z
        cur = -cur-1;

        // check whether we are close to a level
        if (cur < levels.length && Math.abs(z - levels[cur]) < 0.005) {
          custom = -1;
        }
        else if (cur > 0 && Math.abs(z - levels[cur-1]) < 0.005) {
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
      return custom < 0 ? cur < levels.length-1 : cur < levels.length;
    }

    public double[] getLevels() {
      double[] copy = new double[levels.length];
      System.arraycopy(levels, 0, copy, 0, levels.length);
      return copy;
    } 
  }

  public Zoomer() {
    state = new State(defaultZoomLevels, defaultInitialZoomLevel);

    ActionListener zoomIn = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        zoomIn();
      }
    };

    ActionListener zoomOut = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        zoomOut();
      }
    };

    zoomMenu = new ZoomMenu();

    ActionListener zoomPick = new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        zoomMenu.show(zoomPickButton, 0, zoomPickButton.getHeight());
      } 
    };

    zoomPickButton = new LaunchButton(null, PICK_TOOLTIP, PICK_BUTTON_TEXT,
      ZOOM_PICK, PICK_ICON_NAME, zoomPick); 
    zoomPickButton.setAttribute(PICK_TOOLTIP, "Select Zoom");
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
  }

  public static String getConfigureTypeName() {
    return "Zoom capability";
  }

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

  public String[] getAttributeDescriptions() {
    return new String[]{
      "Starting zoom level:  ",
      "Preset zoom levels:  ",
      "Zoom in tooltip text:  ",
      "Zoom in button text:  ",
      "Zoom in Icon:  ",
      "Zoom in hotkey:  ",
      "Zoom select tooltip text",
      "Zoom select button text",
      "Zoom select Icon",
      "Zoom select hotkey",
      "Zoom out tooltip text:  ",
      "Zoom out button text:  ",
      "Zoom out Icon:  ",
      "Zoom out hotkey:  "
    };
  }

  public Class[] getAttributeTypes() {
    return new Class[]{
      Integer.class,
      String[].class,
      String.class,
      String.class,
      InIconConfig.class,
      KeyStroke.class,
      String.class,
      String.class,
      PickIconConfig.class,
      KeyStroke.class,
      String.class,
      String.class,
      OutIconConfig.class,
      KeyStroke.class
    };
  }

  public static class InIconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new IconConfigurer(key, name, IN_DEFAULT_ICON);
    }
  }

  public static class PickIconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new IconConfigurer(key, name, PICK_DEFAULT_ICON);
    }
  }

  public static class OutIconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c,
                                    String key, String name) {
      return new IconConfigurer(key, name, OUT_DEFAULT_ICON);
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
  
  public void addTo(Buildable b) {
    GameModule.getGameModule().getGameState().addGameComponent(this);

    map = (Map) b;

    validator = new SingleChildInstance(map, getClass());

    map.setZoomer(this);
    map.getToolBar().add(zoomInButton);
    map.getToolBar().add(zoomPickButton);
    map.getToolBar().add(zoomOutButton);
  }

  public String getAttributeValueString(String key) {
    if (ZOOM_START.equals(key)) {
      // note that ZOOM_START is one-based, not zero-based
      // FIXME: and this is BAD!
      return String.valueOf(state.getInitialLevel()+1);
    }
    else if (ZOOM_LEVELS.equals(key)) {
      final double[] levels = state.getLevels();
      final String[] s = new String[levels.length];
      for (int i = 0; i < levels.length; ++i) {
        s[i] = String.valueOf(levels[i]);
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

  public void setAttribute(String key, Object val) {
    if (ZOOM_START.equals(key)) {
      if (val instanceof String) {
        val = new Integer((String) val);
      }

      if (val != null) {
        // note that ZOOM_START is one-based, not zero-based
        final double[] levels = state.getLevels();
        final int initial =
          Math.min(Math.max(1, (Integer) val), levels.length) - 1;

        state = new State(levels, initial); 

        zoomInButton.setEnabled(state.hasHigherLevel());
        zoomPickButton.setEnabled(true);
        zoomOutButton.setEnabled(state.hasLowerLevel());

        zoomMenu.initZoomItems();
      }
    }
    else if (ZOOM_LEVELS.equals(key)) {
      if (val instanceof String) {
        val = StringArrayConfigurer.stringToArray((String) val);
      }
      if (val != null) {
        // dump into a set to remove duplicates
        final HashSet<Double> set = new HashSet<Double>();
        for (String s : (String[]) val) {
          set.add(Double.valueOf(s));
        }

        // dump out of the set and unbox to get our levels array        
        final double[] levels = new double[set.size()];
        int j = 0;
        for (Iterator<Double> i = set.iterator(); i.hasNext() ; ++j)
          levels[j] = i.next();  
        
        state = new State(levels, state.getInitialLevel());

        zoomInButton.setEnabled(state.hasHigherLevel());
        zoomPickButton.setEnabled(true);
        zoomOutButton.setEnabled(state.hasLowerLevel());

        zoomMenu.initZoomItems();
      }
    }
    else {
      // FIXME: does having this as an extremal case cause weird behavior for
      // unrecognized keys?
      zoomInButton.setAttribute(key, val);
      zoomOutButton.setAttribute(key, val);
    }
  }
  
  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void removeFrom(Buildable b) {
    map = (Map) b;
    map.setZoomer(null);
    map.getToolBar().remove(zoomInButton);
    map.getToolBar().remove(zoomPickButton);
    map.getToolBar().remove(zoomOutButton);
  }

  public double getZoomFactor() {
    return state.getZoom();
  }

  protected void updateZoomer() {
    final Rectangle r = map.getView().getVisibleRect();
    final Point center =
      map.mapCoordinates(new Point(r.x + r.width/2, r.y + r.height/2));
  
    zoomInButton.setEnabled(state.hasHigherLevel());
    zoomOutButton.setEnabled(state.hasLowerLevel());

    zoomMenu.updateZoom();
    
    map.centerAt(center);
    map.repaint(true);
    map.getView().revalidate();
  }

  public void setZoomLevel(int l) {
    state.setLevel(l);
    updateZoomer();
  }

  public void setZoomFactor(double z) {
    state.setZoom(z);
    updateZoomer();
  }

  public void zoomIn() {
    if (state.hasHigherLevel()) {
      state.higherLevel();
      updateZoomer(); 
    }
  }

  public void zoomOut() {
    if (state.hasLowerLevel()) {
      state.lowerLevel();
      updateZoomer();
    }
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm", "Zoom"); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      zoomInButton.setEnabled(state.hasHigherLevel());
      zoomOutButton.setEnabled(state.hasLowerLevel());
    }

    zoomPickButton.setEnabled(gameStarting);
  }

  public VASSAL.command.Command getRestoreCommand() {
    return null;
  }

  /**
   * The menu which displays zoom levels.
   */
  protected class ZoomMenu extends JPopupMenu implements ActionListener {
    protected final JRadioButtonMenuItem other;
    protected final JPopupMenu.Separator sep;
    protected final ButtonGroup bg;

    public static final long serialVersionUID = 1L;

    public ZoomMenu() {
      super();

      sep = new JPopupMenu.Separator();
      add(sep);

      bg = new ButtonGroup();

      other = new JRadioButtonMenuItem("Other...");
      other.setActionCommand("Other...");
      other.addActionListener(this);
      bg.add(other);
      add(other);

      addSeparator();

      final JMenuItem fw = new JMenuItem("Fit Width");
      fw.setActionCommand("Fit Width");
      fw.addActionListener(this);
      add(fw);

      final JMenuItem fh = new JMenuItem("Fit Height");
      fh.setActionCommand("Fit Height");
      fh.addActionListener(this);
      add(fh);

      final JMenuItem fv = new JMenuItem("Fit Visible");
      fv.setActionCommand("Fit Visible");
      fv.addActionListener(this);
      add(fv);
    }

    public void initZoomItems() {
      while (getComponent(0) != sep) remove(0);

      final double[] levels = state.getLevels(); 
      for (int i = 0; i < levels.length; ++i) {
        final String zs = Long.toString(Math.round(levels[i]*100)) + "%";
        final JMenuItem item = new JRadioButtonMenuItem(zs);
        item.setActionCommand(Integer.toString(i));
        item.addActionListener(this);
        bg.add(item);
        zoomMenu.insert(item, 0);
      }

      ((JRadioButtonMenuItem) getComponent(
        state.getLevelCount() - state.getLevel() - 1)).setSelected(true);
    }

    public void actionPerformed(ActionEvent a) {
      try {
        setZoomLevel(Integer.parseInt(a.getActionCommand()));
        return;
      }
      catch (NumberFormatException e) {
      }
      
      if (a.getActionCommand().equals("Other...")) {      
        final ZoomDialog dialog = new ZoomDialog((Frame)
          SwingUtilities.getAncestorOfClass(Frame.class, map.getView()),
          "Select Zoom Ratio", true);
        dialog.setVisible(true);

        final double z = dialog.getResult()/100.0;
        if (z > 0 && z != state.getZoom()) {
          setZoomFactor(z);
        }
      }
      // FIXME: should be map.getSize() for consistency?
      else if (a.getActionCommand().equals("Fit Width")) {
        final Dimension vd = map.getView().getVisibleRect().getSize();
        final Dimension md = map.mapSize();
        setZoomFactor(vd.getWidth()/md.getWidth());
      }
      else if (a.getActionCommand().equals("Fit Height")) {
        final Dimension vd = map.getView().getVisibleRect().getSize();
        final Dimension md = map.mapSize();
        setZoomFactor(vd.getHeight()/md.getHeight());
      }
      else if (a.getActionCommand().equals("Fit Visible")) {
        final Dimension vd = map.getView().getVisibleRect().getSize();
        final Dimension md = map.mapSize();
        setZoomFactor(Math.min(vd.getWidth()/md.getWidth(),
                               vd.getHeight()/md.getHeight()));
      }
      else {
        // this cannot happen!
        throw new IllegalStateException();
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

    public static final long serialVersionUID = 1L;
 
    public ZoomDialog(Frame owner, String title, boolean modal) {
      super(owner, title, modal);

      final int hsep = 5;

      final JPanel controlsPane = new JPanel(new GridBagLayout());
      final GridBagConstraints c = new GridBagConstraints();
      c.fill = GridBagConstraints.HORIZONTAL;

      final Insets linset = new Insets(0, 0, 11, 11);
      final Insets dinset = new Insets(0, 0, 0, 0);

      final JLabel ratioLabel = new JLabel("Zoom Ratio:");
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

      final JLabel ratioColon = new JLabel(":");
      ratioBox.add(ratioColon);

      ratioBox.add(Box.createHorizontalStrut(hsep));

      ratioDenominatorModel = new SpinnerNumberModel(1, 1, 256, 1);
      ratioDenominatorSpinner = new JSpinner(ratioDenominatorModel);
      ratioDenominatorSpinner.addChangeListener(this);
      ratioBox.add(ratioDenominatorSpinner);

      final JLabel percentLabel = new JLabel("Zoom:");
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
        new SpinnerNumberModel(state.getZoom()*100.0, 0.39, 25600.0, 10.0);
      percentSpinner = new JSpinner(percentModel);
      percentLabel.setLabelFor(percentSpinner);
      percentSpinner.addChangeListener(this);
      percentBox.add(percentSpinner);
 
      updateRatio();

      percentBox.add(Box.createHorizontalStrut(hsep));
      final JLabel percentSign = new JLabel("%");
      percentBox.add(percentSign);

      // buttons
      final Box buttonBox = new Box(BoxLayout.X_AXIS);
      buttonBox.add(Box.createHorizontalGlue());

      okButton = new JButton("Ok");
      okButton.addActionListener(this);
      getRootPane().setDefaultButton(okButton);
      buttonBox.add(okButton);

      buttonBox.add(Box.createHorizontalStrut(hsep));
 
      final JButton cancelButton = new JButton("Cancel");
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

    public void actionPerformed(ActionEvent e) {
      result = e.getSource() == okButton ?
               percentModel.getNumber().doubleValue() : 0.0;

      setVisible(false);
    }

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

      percentModel.setValue(new Double(
        ratioNumeratorModel.getNumber().doubleValue() /
        ratioDenominatorModel.getNumber().doubleValue() * 100.0));

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
        z = 1.0/z;
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
             Math.abs((double)p1/q1 - z) > 0.0001) {
        r = 1.0/r;
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
  
      z = (double)p1/q1;
 
      // hard upper and lower bounds for zoom ratio 
      if (z > 256.0) {
        p1 = 256;
        q1 = 1;
      }
      else if (z < 1.0/256.0) {
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
   
      ratioNumeratorModel.setValue(new Integer(p1));
      ratioDenominatorModel.setValue(new Integer(q1));
  
      ratioNumeratorSpinner.addChangeListener(this);
      ratioDenominatorSpinner.addChangeListener(this);
    }
  }
}
