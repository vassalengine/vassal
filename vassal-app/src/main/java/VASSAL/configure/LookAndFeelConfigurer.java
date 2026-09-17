/*
 *
 * Copyright (c) 2026 by Christian Holm Christensen
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
package VASSAL.configure;

import VASSAL.tools.ErrorDialog;

import java.io.InputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import java.util.Properties;
import java.util.Map;
import java.util.HashMap;
import java.util.SortedSet;
import java.util.TreeSet;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ItemListener;

import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * A Configurer for {@link javax.swing.UIManager.LookAndFeel} values
 */
public class LookAndFeelConfigurer extends Configurer {

  /**
   * Parse a string that defines a color, either as an RGB value,
   * given either as hexadecimal number or in HTML style with a "#"
   * mark.  If neither, assume a named color.
   * 
   * @param value String to parse
   * @return an java.awt.Color or null
   */
  public static Color parseColor(String value) {
    Color color = null;
    try {
      Integer rgb = null;
      if (value.startsWith("0x")) { //NON-NLS
        rgb = Integer.valueOf(value.substring(2), 16);
      }
      else if (value.startsWith("#")) { //NON-NLS
        rgb = Integer.valueOf("0x" + value.substring(1), 16); //NON-NLS
      }
      if (rgb != null) {
        color = new Color(rgb);
      }
      else {
        color = Color.getColor((String)value);
      }
    }
    catch (NumberFormatException e) {
      e.printStackTrace(System.out);
    }
    return color;
  }
  /**
   * A metal theme that can be configured via properties. 
   */
  public static class CustomMetalTheme extends DefaultMetalTheme {
    public Map<String, ColorUIResource> mapping = new HashMap<>();
    
    public CustomMetalTheme(Properties props) {
      for (final Object key : props.keySet()) {
        final String value = (String)props.get(key);
        final Color  color = parseColor(value);
        if (color != null) {
          mapping.put((String)key, new ColorUIResource(color));
        }
      }
    }
    /**
     * @return "Custom"
     */
    @Override
    public String getName() {
      return "Custom"; //NON-NLS
    }

    protected ColorUIResource getColor(String name) {
      return mapping.get(name);
    }
    /**
     * @return the primary 1 color
     */
    @Override
    protected ColorUIResource getPrimary1() {
      return getColor("primary1");
    }
    /**
     * @return the primary 2 color
     */
    @Override
    protected ColorUIResource getPrimary2() {
      return getColor("primary2");
    }
    /**
     * @return the primary 1 color
     */
    @Override
    protected ColorUIResource getPrimary3() {
      return getColor("primary3");
    }
    /**
     * @return the secondary 3 color
     */
    @Override
    protected ColorUIResource getSecondary1() {
      return getColor("secondary1");
    }
    /**
     * @return the secondary 2 color
     */
    @Override
    protected ColorUIResource getSecondary2() {
      return getColor("secondary2");
    }
    /**
     * @return the secondary 3 color
     */
    @Override
    protected ColorUIResource getSecondary3() {
      return getColor("secondary3");
    }
    /**
     * @return text on controls
     */
    @Override
    public ColorUIResource getControlTextColor() {
      return getColor("text");
    }
    /**
     * @return system text
     */
    @Override
    public ColorUIResource getSystemTextColor() {
      return getColor("text");
    }
    /**
     * @return menu text
     */
    @Override
    public ColorUIResource getMenuForeground() {
      return getColor("text");
    }
    /**
     * @return Overall background
     */
    @Override
    public ColorUIResource getWindowBackground() {
      return getColor("background");
    }
    /**
     * @return User input text
     */
    @Override
    public ColorUIResource getUserTextColor() {
      return getColor("text");
    }
    /**
     * @return the control shadow color
     */
    @Override
    public ColorUIResource getControlShadow() {
      return getSecondary3();
    }
    /**
     * @return the control dark shadow color
     */
    @Override
    public ColorUIResource getControlDarkShadow() {
      return getSecondary2();
    }
    /**
     * @return the control info color
     */
    @Override
    public ColorUIResource getControlInfo() {
      return getColor("text");
    }
    /**
     * @return the control highlight color
     */
    @Override
    public ColorUIResource getControlHighlight() {
      return getPrimary1();
    }
    /**
     * @return the control disabled color
     */
    @Override
    public ColorUIResource getControlDisabled() {
      return getColor("text");
    }
    /**
     * @return the primary control color
     */
    @Override
    public ColorUIResource getPrimaryControl() {
      return getPrimary3();
    }
    /**
     * @return the primary control shadow color
     */
    @Override
    public ColorUIResource getPrimaryControlShadow() {
      return getPrimary1();
    }
    /**
     * @return the primary control dark shadow color
     */
    @Override
    public ColorUIResource getPrimaryControlDarkShadow() {
      return getPrimary2();
    }
    /**
     * @return the primary control info color
     */
    @Override
    public ColorUIResource getPrimaryControlInfo() {
      return getColor("text");
    }
    /**
     * @return the primary control info color
     */
    @Override
    public ColorUIResource getFocusColor() {
      return getPrimary3();
    }
    @Override
    public ColorUIResource getMenuSelectedForeground() {
      return getColor("text");
    }
    @Override
    public ColorUIResource getInactiveSystemTextColor() {
      return getPrimary3();
    }
    /**
     * @return the inactive control text color
     */
    @Override
    public ColorUIResource getInactiveControlTextColor() {
      return getControlDisabled();
    }
    
  }
  
  /**
   * Information about Look'n'Feel.
   *
   * Also contains properties that will overload Look'n'Feel settings,
   * so as to make the LnF themeable.
   */
  public static class Info extends LookAndFeelInfo {
    /**
     * Possible properties to set when loading this LnF - if
     * specified, it creates a themed-LnF.
     */ 
    public Properties properties = null;

    /**
     * Construct from a name and class name - typically from the
     * UIManager database of known LnFs.  Also, read properties from a
     * file can be specified, this creating a theme-derived LnF.
     */
    public Info(String name, String clsName, Properties props) {
      super(name, clsName);
      properties = props;
    }

    /**
     * Construct from a name and class name - typically from the
     * UIManager database of known LnFs. 
     */    
    public Info(String name, String clsName) {
      this(name, clsName, null);
    }

    /**
     * Construct UIManager's entry in its database of known LnFs.
     * Also, read properties from a file can be specified, this
     * creating a theme-derived LnF.
     */    
    public Info(LookAndFeelInfo info, Properties props) {
      this(info.getName(), info.getClassName(), props);
    }
    
    /**
     * Construct UIManager's entry in its database of known LnFs.
     */    
    public Info(LookAndFeelInfo info) {
      this(info.getName(), info.getClassName(), null);
    }

    /**
     * Override to simply give the name of the LnF - possibly with a
     * theme name appended to it.
     */
    @Override
    public String toString() {
      return getName();
    }
    
    /**
     * Get the value we want to write to the preference file (V_Global).
     */
    public String valueString() {
      return getName() + "," + getClassName();
    }

    /**
     * Static (class method) to read in theme properties
     *
     * @param name Base name of the LnF
     * @param theme Possible theme name
     * @return a java.util.Properties object, or null.
     */
    public static Properties getThemeProperties(String name, String theme) {
      String themeFile = (name + (theme == null ? "" : theme)
                          + ".properties"); //NON-NLS

      InputStream in = null;
      try {
        in = Files.newInputStream(Paths.get(themeFile));
      }
      catch (IOException e) {
      }

      if (in == null) {
        themeFile = "/VASSAL/theme/" + themeFile; //NON-NLS
        in = LookAndFeelConfigurer.class
          .getResourceAsStream(themeFile); //NON-NLS
      }
      
      if (in == null) {
        return null;
      }

      try {
        final Properties props = new Properties();
        props.load(in);
        return props;
      }
      catch (IOException |
             IllegalArgumentException |
             NullPointerException  e) {
        e.printStackTrace(System.out);
      }
      return null;
    }

    /**
     * Before loading the LnF, set the theme properties on the
     * UIManager.
     */
    public void loadThemeProperties() {
      if (properties == null)
        return;

      // Then load the properties from the theme
      for (final Object key : properties.keySet()) {
        final String val   = (String)properties.get(key);
        final Color  color = parseColor(val);
        if (color != null) 
          UIManager.put(key.toString(), color);
      }
    }

    /**
     * Before installing a new LnF and possibly theme, unset all the
     * know properties of this theme.
     */
    public void resetThemeProperties() {
      final boolean isMetal = getClassName().equals(MetalLookAndFeel.class.getName());
      if (properties == null || isMetal)
        return;

      // Then load the properties from the theme
      for (final Object key : properties.keySet()) {
        UIManager.put(key, null);
      }
    }

    /**
     * Install the LnF as the current LnF on the UI, and set theme
     * properties too.
     */
    protected void updateLnF() {
      try {
        // Special treatmean for the Meta LnF
        final boolean isMetal = getClassName()
          .equals(MetalLookAndFeel.class.getName());

        // If this is the Meta LnF, we install a theme that uses our
        // know properties.  If we have no properties (no theme), use
        // the default Metal theme.
        if (isMetal) {
          if (properties != null) {
            MetalLookAndFeel.setCurrentTheme(new CustomMetalTheme(properties));
          }
          else 
            MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
        }
        // Update UI with settings from the stored theme properties if
        // this is _not_ the Metal LnF.
        if (!isMetal)
          loadThemeProperties();
        
        // Set the LnF
        UIManager.setLookAndFeel(getClassName());      
      }
      catch (Exception e) {
        ErrorDialog.show(e, "Error.failed_to_set_lnf", getClassName());
      }
      
      // Update UI on all frames and windows
      for (final Frame frame : Frame.getFrames()) {
        if (frame == null)
          continue;
        SwingUtilities.updateComponentTreeUI(frame);
        
        for (final Window window : frame.getOwnedWindows()) 
          SwingUtilities.updateComponentTreeUI(window);
      }
    }
  }

  
  private       JPanel            panel;
  private       JComboBox<Info>   laf;
  private final Map<String, Info> lafs = new HashMap<>();
  private       boolean           updating = false;
  
  /**
   * A configurer that allows user to use a GUI element to select the
   * LnF (possibly themed) used by Vassal.  The setting is global and
   * persistent.
   */
  public LookAndFeelConfigurer(String key, String name) {
    super(key, name, null);
    cacheKnownLookAndFeels();
  }

  /**
   * Read in look and feels defined on the system.
   *
   * This is called every time we get the configurer controls, but the
   * map is only updated if a LnF is not known already.  This allows
   * us to load. and display, LnFs from third party JARs, and still
   * show them in this configurerer.
   */
  public final void cacheKnownLookAndFeels() {
    final LookAndFeelInfo[] known   = UIManager.getInstalledLookAndFeels();
    final String            current = UIManager.getLookAndFeel().getClass().getName();
    for (final LookAndFeelInfo info : known) {
      final boolean isCurrent = current.equals(info.getClassName());

      // Check if LnF is already known to us
      if (lafs.containsKey(info.getName())) {
        continue;
      }

      // Allow theming of an LnF 
      final String[] themes = {null, "Dark", "Light"}; //NON-NLS
      for (final String theme : themes) {
        final Properties props = Info.getThemeProperties(info.getName(),
                                                         theme);
        if (props == null && theme != null)
          continue;
              
        final Info i = new Info(info.getName() + 
                                (theme == null ? "" : " (" + theme + ")"), //NON-NLS
                                info.getClassName(),
                                props);
        lafs.put(i.getName(), i);

        if (isCurrent)
          setValue(info.getName());
      }
    }
  }
  

  protected Info getInfo() {
    return (Info)value;
  }

  /**
   * Get current value as a string
   */
  @Override
  public String getValueString() {
    return value != null ? ((Info) value).getName() : ""; //NON-NLS
  }

  /**
   * Set current value from a string
   */
  @Override
  public void setValue(String s) {
    final Info info = (Info)lafs.get(s);
    if (info == null) {
      return;
    }
    if (info == value)
      return;

    if (updating)
      return;
    
    setValue(info);
    getInfo().updateLnF();
  }
  
  /**
   * Set current value from an object
   */
  @Override
  public void setValue(Object o) {
    if (updating)
      return;
    
    if (value != null && value != o) {
      ((Info)value).resetThemeProperties();
    }
    super.setValue(o);
  }

  public void updateAvailableLookAndFeels() {
    if (laf == null)
      return;

    updating = true;
    
    // Clean out combo-box so we can start afresh
    laf.removeAllItems();

    // Now loop over known LnFs and add them to the combo-box
    final String sysClsName = UIManager.getSystemLookAndFeelClassName();
    Info         sysInfo    = null;
    Info         current    = null;
    final SortedSet<String> keys = new TreeSet<>(lafs.keySet());

    for (final String key : keys) {
      final Info info = lafs.get(key);
      if (info == null) // Shouldn't happen
        continue;

      final boolean isCurrent = value != null && value == info;
      final boolean isSys     = info.getClassName().equals(sysClsName);
      laf.addItem(info);
      
      if (isCurrent) {
        current = info;
      }
      if (isSys) {
        sysInfo = info;
      }
    }
    laf.setSelectedItem(current == null ? sysInfo : current);
    laf.setMaximumSize(new Dimension(laf.getMaximumSize().width,
                                     laf.getPreferredSize().height));

    updating = false;
  }
  
  /**
   * Get the UI controls
   */
  @Override
  public java.awt.Component getControls() {
    // See if we got new LnFs
    cacheKnownLookAndFeels();
    updateAvailableLookAndFeels();
    
    if (panel == null) {
      panel = new ConfigurerPanel(getName(), "[]rel[]", "[]rel[]"); // NON-NLS 
      laf   = new JComboBox<>();
      panel.add(laf);

      updateAvailableLookAndFeels();
      
      final ItemListener l = evt -> setValue((Info)laf.getSelectedItem());
      laf.addItemListener(l);
      laf.setMaximumSize(new Dimension(laf.getMaximumSize().width,
                                       laf.getPreferredSize().height));
    }

    return panel;
  }

  /**
   * On a detected update of the value, do the actual work to set the
   * chosen LnF.
   */
  @Override
  public void fireUpdate() {
    super.fireUpdate();
    if (getInfo() != null) 
      getInfo().updateLnF();
  }

  /**
   * Set the visibility of the label of this configurere
   */
  @Override
  public void setLabelVisible(boolean visible) {
    if (panel instanceof ConfigurerPanel) {
      ((ConfigurerPanel) panel).setLabelVisibility(visible);
    }
  }

  /**
   * Test of this configurer
   */
  public static void main(String[] args) {
    final JFrame frame = new JFrame();
    frame.setLayout(new BoxLayout(frame.getContentPane(), BoxLayout.Y_AXIS));
    final LookAndFeelConfigurer config = new LookAndFeelConfigurer("a", "LaF: "); //NON-NLS
    frame.add(config.getControls());
    config.addPropertyChangeListener(evt -> {
      final LookAndFeelConfigurer fconfig
        = new LookAndFeelConfigurer(null, "Testing");
      fconfig.setValue(config.getValueString());
      fconfig.fireUpdate();
      frame.pack();
    });
    frame.pack();
    frame.setVisible(true);
  }
}
//
// EOF
//
