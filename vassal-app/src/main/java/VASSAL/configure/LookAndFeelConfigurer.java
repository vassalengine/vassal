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
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.FileFilter;

import java.lang.reflect.InvocationTargetException;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import java.nio.file.Files;
import java.nio.file.Paths;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.stream.Collectors;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Window;
import java.awt.event.ItemListener;

import javax.swing.DefaultListModel;
import javax.swing.LookAndFeel;
import javax.swing.JComboBox;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;
import javax.swing.UnsupportedLookAndFeelException;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;

import net.miginfocom.swing.MigLayout;

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
  protected static Color parseColor(String value) {
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
      else if (value.startsWith("ui:"))
        color = UIManager.getColor(((String)value.substring(3)));
      else
        color = Color.getColor((String)value);
    }
    catch (NumberFormatException e) {
      e.printStackTrace(System.out);
    }
    return color;
  }
  
  /**
   * A metal theme that can be configured via properties. 
   */
  private static class CustomMetalTheme extends DefaultMetalTheme {
    protected final Map<String, ColorUIResource> mapping = new HashMap<>();
    
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
   * A specialised URL class loader that allows us to add URLs after
   * construction
   */
  private static class LafClassLoader extends URLClassLoader {
    /**
     * Construct
     */
    LafClassLoader() {
      super(new URL[]{});
    }
    /**
     * This exposes the addUrl method which is normally is protected
     */
    @Override
    public void addURL(URL url) {
      super.addURL(url);
    }
  }

  /** The class loader we will be using */
  protected LafClassLoader loader = new LafClassLoader();
  
  /**
   * Holds information about a JAR and selected class
   */
  private static class LafJarInfo {
    public       String       jarFileName;
    public       List<String> lafClassNames;
    public final List<String> attempted = new ArrayList<>();

    public LafJarInfo() {
      this("", new ArrayList<>());
    }

    public LafJarInfo(String f, List<String> n) {
      jarFileName = f;
      lafClassNames = n;
    }

    public void setValue(String s) {
      if (s == null || s.isBlank()) {
        jarFileName = "";
        lafClassNames.clear();
        return;
      }

      final String[] p = s.split("#");
      jarFileName      = p[0];
      lafClassNames.clear();
      lafClassNames.addAll(Arrays.asList(p[1].split("&")));
    }
    public void setValue(String filename, List<String> classNames) {
      jarFileName = filename != null ? filename : "";
      lafClassNames.clear();

      if (classNames != null)
        lafClassNames.addAll(classNames);
    }
    
    public boolean isValid() {
      return (jarFileName  != null && !jarFileName.isBlank() &&
              !lafClassNames.isEmpty());
    }
      
    /**
     * Return a string representation.  The returned string is
     * formatted as a URL. That is, the file name is the path, and the
     * class name is the anchor.  The two are separated by a hash mark
     * '#'
     */
    @Override
    public String toString() {
      if (!isValid())
        return "";
      return (jarFileName + "#" +
              lafClassNames.stream().collect(Collectors.joining("&")));
    }

    public URL getURL() {
      try {      
        return Paths.get(jarFileName).toUri().toURL();
      }
      catch (MalformedURLException e) {
        return null;
      }
    }
    
    public List<String> getClasses() {
      final List<String> ret = new ArrayList<>();

      // Check is a little bit redundant because we do that in the
      // property change listener.  However, we could be fired from
      // elsewhere, so we do the check nonetheless.
      if (jarFileName == null   ||
          jarFileName.isBlank() ||
          jarFileName.equals("null")) {
        return ret;
      }
      
      // Open JAR as a Zip file and loop over entries.
      try (ZipFile zip = new ZipFile(jarFileName)) {
        for (final Enumeration<ZipEntry> iter =
               (Enumeration<ZipEntry>)zip.entries();
               iter.hasMoreElements(); ) {
          final ZipEntry entry = iter.nextElement();
          final String   name  = entry.getName();

          // Not a class 
          if (!name.endsWith(".class"))
            continue;
          
          // Nested classes
          if (name.contains("$")) 
            continue;

          // Replace '/' with '.', and remove '.class' 
          final String fqName = name.replace("/", ".")
            .substring(0, name.length() - ".class".length());

          ret.add(fqName);
        }
        return ret;
      }
      catch (IOException e) {
        ErrorDialog.show(e,
                         "LookAndFeel.scan_jar_fail",
                         jarFileName);
      }
      
      return ret;
    }

    /**
     * Try to load the specified classes using our class loader.  If
     * that succeeds, then the class is added as a new Look and Feel.
     * Otherwise, we remove it from our selection
     */
    void makeAvailable(ClassLoader loader) {
      if (!isValid())
        return;

      final List<String> toRemove = new ArrayList<>();
      
      for (final String className : lafClassNames) {
        // Do not attempt at loading a class we already tried
        if (attempted.contains(className))
          continue;

        // Register this class as attempted
        attempted.add(className);
        try {
          final Class<?> cls = Class.forName(className, true, loader);
          if (LookAndFeel.class.isAssignableFrom(cls)) {
            final LookAndFeel lnf =
              (LookAndFeel)cls.getConstructors()[0].newInstance();
            
            UIManager.installLookAndFeel(lnf.getName(), cls.getName());
          }
          else {
            ErrorDialog.show("LookAndFeel.load_class_fail",
                             className, jarFileName);
            toRemove.add(className);
          }
        }
        catch (ClassNotFoundException    |
               InstantiationException    |
               InvocationTargetException |
               IllegalAccessException    |
               NullPointerException e) {
          ErrorDialog.show(e,
                           "LookAndFeel.load_class_fail",
                           className, jarFileName);
          toRemove.add(className);
        }        
      }

      for (final String del : toRemove) {
        lafClassNames.remove(del);
      }
    }
  }

  /**
   * Configure location of a JAR and select classes from it.
   */
  private static class LafJarConfigurer extends Configurer {
    private       JPanel                   panel;
    private final DefaultListModel<String> model = new DefaultListModel<>();
    private       JList<String>            classes;
    private       FileConfigurer           filename;
    private       boolean                  prunning = false;
    
    private class JarFileConfigurer extends FileConfigurer {
      public JarFileConfigurer(String key, String name) {
        super(key, name);
      }
      @Override
      protected FileChooser initFileChooser() {
        final FileChooser fc = super.initFileChooser();
        fc.setFileFilter(new FileFilter() {
            @Override
            public boolean accept(File file) {
              return (file.getName().endsWith(".jar") ||
                      file.getName().endsWith(".class"));
            }
            @Override
            public String getDescription() {
              return "JAR or class files";
            }
          });
        
        return fc;
      }
    }
      
    /**
     * Constructor
     */
    public LafJarConfigurer() {
      super("", "", new LafJarInfo());
    }
    
    /**
     * Constructor
     */
    public LafJarConfigurer(String fn, List<String> cls) {
      super("", "", new LafJarInfo(fn, cls));
    }
    
    public LafJarInfo getInfo() {
      return (LafJarInfo)value;
    }

    protected void selectClasses() {
      classes.clearSelection();
      for (final String cls : getInfo().lafClassNames) {
        final int index = model.indexOf(cls);
        if (index >= 0) {
          classes.addSelectionInterval(index, index);
        }
      }
    }
    
    public void updateClasses() {
      model.removeAllElements();
      classes.setEnabled(false);

      model.addAll(getInfo().getClasses());
      if (model.isEmpty())
        // Leave classes disabled 
        return;

      selectClasses();

      classes.setEnabled(true);
    }
    
    @Override
    public Component getControls() {
      if (panel != null)
        return panel;
      
      panel = new JPanel(new MigLayout(ConfigurerLayout.STANDARD_INSETS_GAPY,
                                       "[grow,fill][grow,fill]")); // NON-NLS
      
      filename = new JarFileConfigurer("", "");
      filename.setValue(getInfo().jarFileName);
      filename.addPropertyChangeListener(e -> {
        final String newValue = filename.getValueString();
        if (newValue == null        ||
            newValue.isBlank()      || 
            newValue.equals("null") ||
            newValue.equals(getInfo().jarFileName))
          return;

        getInfo().jarFileName   = filename.getValueString();
        getInfo().lafClassNames.clear();
        
        updateClasses();
      });
      panel.add(filename.getControls());
      
      classes  = new JList<>(model);
      classes.setVisibleRowCount(5);
      updateClasses();

      final JScrollPane scroll = new JScrollPane(classes);
      panel.add(scroll);
      
      classes.addListSelectionListener(e -> {
        if (prunning)
          return;
        setValue(filename.getValueString(),
                 classes.getSelectedValuesList());
      });
      return panel;
    }
    
    /**
     * Set the value from a string - e.g., when we read in from file
     */
    @Override
    public void setValue(String s) {
      getInfo().setValue(s);
    }

    /**
     * Set the value from an object.  Also when we add more entries to
     * the list
     */
    public void setValue(String fn, List<String> cn) {
      getInfo().setValue(fn, cn);
      setValue(value);

      fireUpdate(); // notify parent

      prunning = true;
      selectClasses();
      prunning = false;
    }

    @Override
    public String getValueString() {
      return getInfo().toString();
    }
  }

  /** Declare ahead of time */
  protected LafConfigurer lafConfigurer;
      
  /**
   * List of jars, and for each jar, a list of classes to add as Look
   * and Feel classes
   */
  private class LafJarsConfigurer extends ListConfigurer {
    public LafJarsConfigurer(String key, String name) {
      super(key, "");
    }

    /**
     * Get configurer for a new entry in list, or recreate one.
     */
    @Override
    protected Configurer buildChildConfigurer() {
      return new LafJarConfigurer();
    }

    @Override 
    public void setValue(String v) {
      super.setValue(v);
      fireUpdate();
    }

    @Override
    public void fireUpdate() {
      updateLoader();
      updateAvailable();
    }
      

    /**
     * Add JAR urls to our loader
     */
    public void updateLoader() {
      for (final Object o : getListValue()) {
        final LafJarInfo item = (LafJarInfo)o;
        final URL        url  = item.getURL();
        if (url == null)
          continue;
        
        loader.addURL(url);
      }
    }

    /**
     * Update available Look'n'Feel classes, based on configured JARs
     * and selected classes for each JAR.
     */
    public void updateAvailable() {
      for (final Object o : getListValue()) {
        final LafJarInfo info = (LafJarInfo)o;
        info.makeAvailable(loader);
      }
      if (lafConfigurer != null) {
        lafConfigurer.cacheKnownLookAndFeels();
        lafConfigurer.updateAvailableLookAndFeels();
      }
    }

    @Override
    protected void updateValue() {
      super.updateValue();
      fireUpdate();
    }
  }

  /**
   * Holds information about a look'n'feel, including a possible class
   * loader
   */
  private class LafInfo extends LookAndFeelInfo {
    /**
     * Possible properties to set when loading this LnF - if
     * specified, it creates a themed-LnF.
     */ 
    public Properties properties = null;
    
    /**
     * Construct our information
     */
    public LafInfo() {
      super("", "");
    }

    /**
     * Construct our information
     */
    public LafInfo(String n, String c, Properties props) {
      super(n, c);
      properties = props;
    }

    /**
     * Get string to show in selection box
     */
    @Override
    public String toString() {
      return getName();
    }

    /**
     * Static (class method) to read in theme properties
     *
     * @param name Base name of the LnF
     * @param theme Possible theme name
     * @return a java.util.Properties object, or null.
     */
    public static Properties getThemeProperties(String name, String theme) {
      String themeFile = (name.replace("/", "") + (theme == null ? "" : theme)
                          + ".properties"); //NON-NLS

      InputStream in = null;
      try {
        in = Files.newInputStream(Paths.get(themeFile));
      }
      catch (IOException e) {
        in = null;
      }

      if (in == null) {
        themeFile = "/VASSAL/theme/" + themeFile; //NON-NLS
        in = LookAndFeelConfigurer.class
          .getResourceAsStream(themeFile); //NON-NLS
      }
      
      if (in == null) 
        return null;

      try {
        final Properties props = new Properties();
        props.load(in);
        return props;
      }
      catch (IOException |
             IllegalArgumentException |
             NullPointerException  e) {
        // e.printStackTrace(System.out);
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
        if (color != null) {
          UIManager.put(key.toString(), color);
        }
      }
    }

    /**
     * Before installing a new LnF and possibly theme, unset all the
     * know properties of this theme.
     */
    public void resetThemeProperties() {
      if (properties == null)
        return;
      
      // Then load the properties from the theme
      for (final Object key : properties.keySet()) {
        UIManager.put(key, null);
      }
    }
    
    /**
     * Make this the current look'n'feel.  If a loader was passed at
     * construction time, then we will use that as the (temporary)
     * context class loader.
     */
    public void setCurrent(LafInfo previous) {
      if (previous != null)
        previous.resetThemeProperties();

      final ClassLoader old = Thread.currentThread().getContextClassLoader();
      try {
        if (loader != null) 
          Thread.currentThread().setContextClassLoader(loader);

        loadThemeProperties();
        
        UIManager.setLookAndFeel(getClassName());
      }
      catch (ClassNotFoundException          |
             InstantiationException          |
             IllegalAccessException          |
             UnsupportedLookAndFeelException | 
             ClassCastException              e) {
        // This should very rarely happen 
        ErrorDialog.show(e,
                         "LookAndFeel.set_fail",
                         getName(),
                         getClassName());
        return;
      }
      finally {
        // We reset unconditionally - old is what it is.
        Thread.currentThread().setContextClassLoader(old);
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
    
    public String valueString() {
      return getName() + "," + getClassName();
    }
  }
  /**
   * Holds information about a look'n'feel - Metal version
   * loader
   */
  private final class MetalLafInfo extends LafInfo {
    /**
     * Construct our information
     */
    public MetalLafInfo(String n, String c, Properties props) {
      super(n, c, props);
    }

    /**
     * No-op.  Properties are used to create a custom Metal theme
     * instead.
     */
    @Override
    public void loadThemeProperties() {
    }

    /**
     * No-op.  We do not set properties on the UIManager.  We loaded a
     * custom Metal theme instead.
     */
    @Override
    public void resetThemeProperties() {
    }
    
    /**
     * Make this the current look'n'feel.  If a loader was passed at
     * construction time, then we will use that as the (temporary)
     * context class loader.
     */
    @Override
    public void setCurrent(LafInfo previous) {
      if (properties != null) 
        MetalLookAndFeel.setCurrentTheme(new CustomMetalTheme(properties));
      else
        MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
      super.setCurrent(previous);
    }
  }

  /**
   * Configurer of current Look'n'Feel
   */
  private final class LafConfigurer extends Configurer {

    private       JPanel               panel;
    private       JComboBox<LafInfo>   laf;
    private final Map<String, LafInfo> lafs = new HashMap<>();
    private       boolean              updating = false;
    private       LafInfo              previous;

    public LafConfigurer(String key, String name) {
      super(key, name);
    }

    /**
     * Read in look and feels defined on the system.
     *
     * This is called every time we get the configurer controls, but the
     * map is only updated if a LnF is not known already.  This allows
     * us to load. and display, LnFs from third party JARs, and still
     * show them in this configurerer.
     */
    public void cacheKnownLookAndFeels() {
      final LookAndFeelInfo[] known   = UIManager.getInstalledLookAndFeels();
      final String            current = UIManager.getLookAndFeel().getClass().getName();
      for (final LookAndFeelInfo info : known) {
        final boolean isCurrent = current.equals(info.getClassName());

        // Check if LnF is already known to us
        if (lafs.containsKey(info.getName())) 
          continue;
        

        // Allow theming of an LnF 
        final String[] themes = {null, "Dark", "Light"}; //NON-NLS
        for (final String theme : themes) {
          final Properties props = LafInfo.getThemeProperties(info.getName(),
                                                              theme);
          if (props == null && theme != null)
            continue;

          final String name = info.getName() + (theme == null ? "" :
                                                " (" + theme + ")");
          LafInfo i = null;
          
          if (info.getClassName().equals(MetalLookAndFeel.class.getName()))
            i = new MetalLafInfo(name, info.getClassName(),  props);
          else
            i = new LafInfo(name, info.getClassName(),  props);
          
          lafs.put(i.getName(), i);

          if (isCurrent)
            setValue(info.getName());
        }
      }
    }

    /**
     * Update available Look and Feels in the combo box based on all
     * our known Look and Feels
     */
    public void updateAvailableLookAndFeels() {
      if (laf == null)
        return;
      
      updating = true;
      
      // Clean out combo-box so we can start afresh
      laf.removeAllItems();

      // Now loop over known LnFs and add them to the combo-box
      final String sysClsName = UIManager.getSystemLookAndFeelClassName();
      LafInfo      sysInfo    = null;
      LafInfo      current    = null;
      final SortedSet<String> keys = new TreeSet<>(lafs.keySet());

      for (final String key : keys) {
        final LafInfo info = lafs.get(key);
        if (info == null) // Shouldn't happen
          continue;

        final boolean isCurrent = value != null && value == info;
        final boolean isSys     = info.getClassName().equals(sysClsName);
        laf.addItem(info);
      
        if (isCurrent) 
          current = info;
        
        if (isSys) 
          sysInfo = info;
        
      }
      laf.setSelectedItem(current == null ? sysInfo : current);

      updating = false;
    }
  
    protected LafInfo getInfo() {
      return (LafInfo)value;
    }
    
    /**
     * Get current value as a string
     */
    @Override
    public String getValueString() {
      return value != null ? ((LafInfo) value).getName() : ""; //NON-NLS
    }
    
    /**
     * Set current value from a string
     */
    @Override
    public void setValue(String s) {
      final LafInfo info = (LafInfo)lafs.get(s);
      if (info == null || info == value || updating)
        return;

      previous = (LafInfo)value;
      setValue(info);
      getInfo().setCurrent(previous);
    }
  
    /**
     * On a detected update of the value, do the actual work to set the
     * chosen LnF.
     */
    @Override
    public void fireUpdate() {
      super.fireUpdate();
      if (getInfo() != null) 
        getInfo().setCurrent(previous);
    }

    /**
     * Set current value from an object
     */
    @Override
    public void setValue(Object o) {
      if (updating)
        return;

      if (value != o)
        previous = (LafInfo)value;
      
      // if (value != null && value != o) {
      //   ((Info)value).resetThemeProperties();
      // }
      super.setValue(o);
    }
    /**
     * Get the UI controls
     */
    @Override
    public Component getControls() {
      // See if we got new LnFs
      cacheKnownLookAndFeels();
      updateAvailableLookAndFeels();

      if (panel != null)
        return panel;

      panel = new ConfigurerPanel(getName(), "[]rel[fill,grow]", "[]rel[fill,grow]"); // NON-NLS 
      laf   = new JComboBox<>();
      panel.add(laf, "grow");

      updateAvailableLookAndFeels();
      
      final ItemListener l = evt -> setValue((LafInfo)laf.getSelectedItem());
      laf.addItemListener(l);
      laf.setMaximumSize(new Dimension(laf.getMaximumSize().width,
                                       laf.getPreferredSize().height));
      return panel;
    }
  }

  protected JPanel            panel;
  protected LafJarsConfigurer lafJarsConfigurer;
  /**
   * Main interface of this configurer
   */
  public LookAndFeelConfigurer(String name,
                               String lafKey, String lafJarKey,
                               String lafName, String lafJarName) {
    super("", name);
    lafJarsConfigurer = new LafJarsConfigurer(lafJarKey, lafJarName);
    lafConfigurer     = new LafConfigurer(lafKey,    lafName);
  }

  public Configurer getJarClassesConfigurer() {
    return lafJarsConfigurer;
  }
  public Configurer getCurrentConfigurer() {
    return lafConfigurer;
  }
  
  /**
   * Get the UI controls
   */
  @Override
  public Component getControls() {
    if (panel != null)
      return panel;

    panel = new ConfigurerPanel(null, "[]rel[fill,grow]", "[]rel[fill,grow]"); // NON-NLS 
    final JPanel right = new JPanel(new MigLayout("fill", "[fill]"));
    panel.add(right);

    right.add(lafConfigurer.getControls(), "growx, wrap");
    right.add(lafJarsConfigurer.getControls(), "growx");

    lafConfigurer.addPropertyChangeListener(e -> setValue(lafConfigurer.getValueString()));

    return panel;
  }

  @Override
  public void fireUpdate() {
    lafJarsConfigurer.fireUpdate();
    lafConfigurer.fireUpdate();
  }
  
  @Override
  public void setValue(String s) {
    setValue((Object)s);
  }

  @Override
  public String getValueString() {
    return (String)value;
  }
  
  /**
   * Test of this configurer
   */
  public static void main(String[] args) {
    final JFrame frame = new JFrame();
    final JPanel content = new JPanel(new MigLayout("fill"));
    final LookAndFeelConfigurer config
      = new LookAndFeelConfigurer("Look and Feel",
                                  "a", "b", 
                                  "Current", "Jar"); //NON-NLS
    content.add(config.getControls(), "growx");
    config.addPropertyChangeListener(evt -> {
      config.fireUpdate();
      frame.pack();
    });
    frame.setContentPane(content);
    frame.pack();
    frame.setVisible(true);
  }

}
//
// EOF
//

  
    
    
      
    
