/*
 *
 * Copyright (c) 2008-2009 Brent Easton
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
package VASSAL.tools.icon;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import net.miginfocom.swing.MigLayout;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.Configurable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ImageConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.filechooser.FileChooser;
import VASSAL.tools.filechooser.ImageFileFilter;
import VASSAL.tools.image.ImageUtils;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.OpIcon;
import VASSAL.tools.swing.Dialogs;

/**
 * An IconFamily is a named set of Icons in the four standard Tango sizes.
 *
 * Each IconFamily consists of at least a Scalable Icon, plus zero or more
 * specifically sized icons.
 *
 * If a specific sized Icon is missing, the IconFamily will supply a scaled icon
 * based on the Scalable icon.
 *
 * Icons are created as lazily as possible.
 *
 * IconFamilys are created in two ways: - For Vassal inbuilt Icons by
 * IconFactory when it scans the Vengine for inbuilt Icons - For Modules,
 * IconFamilys can be added to IconFamilyContainer by the module designer.
 *
 * Each IconFamily consists of at least a Scalable Icon, plus zero or more
 * specifically sized icons. If an
 */
public class IconFamily extends AbstractConfigurable {

  public static final String SCALABLE_ICON = "scalableIcon"; //$NON-NLS-1$
  public static final String ICON0 = "icon0"; //$NON-NLS-1$
  public static final String ICON1 = "icon1"; //$NON-NLS-1$
  public static final String ICON2 = "icon2"; //$NON-NLS-1$
  public static final String ICON3 = "icon3"; //$NON-NLS-1$

  private final PropertyChangeSupport propSupport = new PropertyChangeSupport(
      this);

  // Tango Icon sizes
  public static final int XSMALL = 0;
  public static final int SMALL = 1;
  public static final int MEDIUM = 2;
  public static final int LARGE = 3;

  static final int MAX_SIZE = LARGE;
  static final int SIZE_COUNT = MAX_SIZE + 1;

  // Pixel size of each Tango Size
  static final int[] SIZES = new int[] { 16, 22, 32, 48 };

  // Directories within the icons directory to locate each Tango Size
  static final String[] SIZE_DIRS = new String[] {
    "16x16/", //$NON-NLS-1$
    "22x22/", //$NON-NLS-1$
    "32x32/", //$NON-NLS-1$
    "48x48/"  //$NON-NLS-1$
  };

  // Names of sizes in local language
  static final String[] SIZE_NAMES = new String[SIZE_COUNT];;

  // Directory within the icons directory holding the Scalable versions of the
  // icons
  static final String SCALABLE_DIR = "scalable/"; //$NON-NLS-1$

  // Cache of the icons in this family
  protected OpIcon[] icons;
  protected OpIcon scalableIcon;

  // Paths to the source of the icons in this family
  protected String scalablePath;
  protected String[] sizePaths = new String[SIZE_COUNT];

  /**
   * Return list of Icon Size names in local language
   *
   * @return
   */
  public static String[] getIconSizeNames() {
    synchronized (SIZE_NAMES) {
      if (SIZE_NAMES[0] == null) {
        SIZE_NAMES[XSMALL] = Resources.getString("Icon.extra_small"); //$NON-NLS-1$
        SIZE_NAMES[SMALL] = Resources.getString("Icon.small"); //$NON-NLS-1$
        SIZE_NAMES[MEDIUM] = Resources.getString("Icon.medium"); //$NON-NLS-1$
        SIZE_NAMES[LARGE] = Resources.getString("Icon.large"); //$NON-NLS-1$
      }
    }
    return SIZE_NAMES;
  }

  /**
   * Return an Icon Size based on the local language name
   */
  public static int getIconSize(String name) {
    int size = SMALL;
    final String[] options = getIconSizeNames();
    for (int i = 0; i < options.length; i++) {
      if (options[i].equals(name)) {
        return i;
      }
    }
    return size;
  }

  public static int getIconHeight(int size) {
    if (size < 0 || size > MAX_SIZE) {
      return 0;
    }
    return SIZES[size];
  }

  /**
   * Create a new IconFamily with the given name. The name supplied will
   * normally be the name of an IconFamily, with no suffix.
   *
   * These constructors are used by IconFactory to create IconFamilys for the
   * Vassal inbuilt Icons
   *
   * FIXME: Write this bit...Will be needed once Toolbar Icon support is added
   * Backward Compatibility: If the name supplied does have a file type suffix,
   * then it is a specific Icon name from a pre-IconFamily module. By throwing
   * away the suffix, IconFamily will use the supplied icon as a base icon to
   * create the full IconFamily.
   *
   * @param familyName
   *          IconFamily name or Icon name
   * @param scalableName
   *          Name of the scalable icon
   * @param sizeNames
   *          Names of the sized Icons
   */
  public IconFamily(String familyName, String scalableName, String[] sizeName) {
    this(familyName);
    setScalableIconPath(scalableName);
    for (int i = 0; i < MAX_SIZE; i++) {
      setSizeIconPath(i, sizeName[i]);
    }
  }

  public IconFamily(String familyName) {
    this();
    setConfigureName(familyName);
  }

  public IconFamily() {
    icons = new OpIcon[SIZE_COUNT];
    setConfigureName(""); //$NON-NLS-1$
  }

  public void setScalableIconPath(String s) {
    scalablePath = s;
    scalableIcon = null;
  }

  public void setSizeIconPath(int size, String path) {
    sizePaths[size] = path;
    icons[size] = null;
  }

  public boolean isLegacy() {
    return getName().contains("."); //$NON-NLS-1$
  }

  /**
   * Return a particular sized icon. If it can't be found, then build it by
   * scaling the base Icon.
   *
   * @param size
   *          Icon size
   * @return Icon
   */
  public Icon getIcon(int size) {
    if (size < 0 || size > MAX_SIZE) {
      return null;
    }

    synchronized (this) {
      if (icons[size] == null) {
        icons[size] = buildIcon(size);
      }
    }
    return icons[size];
  }

  /**
   * Return a particular sized Icon, but do not build one from the scalable Icon
   * if it is not found.
   *
   * @param size
   * @return
   */
  public Icon getRawIcon(int size) {
    if (size < 0 || size > MAX_SIZE || sizePaths[size] == null) {
      return null;
    }
    return getIcon(size);
  }

  /**
   * Return the scalable icon directly (used by {@link IconImageConfigurer})
   *
   * @return
   */
  public Icon getScalableIcon() {
    synchronized (this) {
      buildScalableIcon();
    }
    return scalableIcon;
  }

  public BufferedImage getImage(int size) {
    if (size < 0 || size > MAX_SIZE) {
      return null;
    }
    getIcon(size);
    return (BufferedImage) (icons[size] == null ? null : icons[size].getImage());
  }

  protected OpIcon buildIcon(int size) {
    // Do we have it ready to go?
    if (icons[size] != null) {
      return icons[size];
    }

    // This size exists?
    if (sizePaths[size] != null) {
      icons[size] = new OpIcon(Op.load(sizePaths[size]));
      icons[size].getImage();
      return icons[size];
    }

    // No, So we need to build it from the Scalable version
    buildScalableIcon();

    icons[size] = scaleIcon(scalableIcon, SIZES[size]);
    icons[size].getImage();
    return icons[size];
  }

  protected void buildScalableIcon() {
    if (scalableIcon == null) {
      if (scalablePath != null) {
        scalableIcon = new OpIcon(Op.load(scalablePath));
      }
    }
  }

  /**
   * Scale an Icon to desired size
   *
   * @param base
   *          Base Icon
   * @param toSizePixels
   *          Required Size in Pixels
   * @return Scaled Icon
   */
  protected OpIcon scaleIcon(OpIcon base, int toSizePixels) {

    if (base == null) {
      return null;
    }

    final int baseHeight = base.getIconHeight();
    if (baseHeight == toSizePixels) {
      return base;
    }

    return new OpIcon(Op.scale(base.getOp(), ((double) toSizePixels)
        / base.getIconHeight()));
  }

  public String getName() {
    return getConfigureName();
  }

  @Override
  public void addPropertyChangeListener(PropertyChangeListener l) {
    propSupport.addPropertyChangeListener(l);
  }

  @Override
  public void setConfigureName(String s) {
    String oldName = name;
    this.name = s;
    propSupport.firePropertyChange(NAME_PROPERTY, oldName, name);
  }

  // Note: Custom Configurer
  public static String getConfigureTypeName() {
    return Resources.getString("Editor.IconFamily.component_type"); //$NON-NLS-1$
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[0];
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[0];
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] { NAME_PROPERTY, SCALABLE_ICON, ICON0, ICON1, ICON2, ICON3 };
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME_PROPERTY.equals(key)) {
      return getConfigureName();
    }
    else if (SCALABLE_ICON.equals(key)) {
      return scalablePath;
    }
    else if (ICON0.equals(key)) {
      return sizePaths[0];
    }
    else if (ICON1.equals(key)) {
      return sizePaths[1];
    }
    else if (ICON2.equals(key)) {
      return sizePaths[2];
    }
    else if (ICON3.equals(key)) {
      return sizePaths[3];
    }
    return null;
  }

  @Override
  public void setAttribute(String key, Object value) {
    if (NAME_PROPERTY.equals(key)) {
      setConfigureName((String) value);
    }
    else if (SCALABLE_ICON.equals(key)) {
      setScalableIconPath((String) value);
    }
    else if (ICON0.equals(key)) {
      setSizeIconPath(0, (String) value);
    }
    else if (ICON1.equals(key)) {
      setSizeIconPath(1, (String) value);
    }
    else if (ICON2.equals(key)) {
      setSizeIconPath(2, (String) value);
    }
    else if (ICON3.equals(key)) {
      setSizeIconPath(3, (String) value);
    }
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public HelpFile getHelpFile() {
    return null;
  }

  @Override
  public void removeFrom(Buildable parent) {

  }

  @Override
  public void addTo(Buildable parent) {

  }

  @Override
  public Configurer getConfigurer() {
    return new IconFamilyConfig(this);
  }

  /*******************************************************
   * Custom Configurer for Icon Family
   *
   */
  static class IconFamilyConfig extends Configurer {
    protected IconFamily family;
    protected JPanel controls;
    protected StringConfigurer title;
    protected JLabel errorLabel;
    protected ImageConfigurer scalableConfig;

    public IconFamilyConfig(IconFamily f) {
      super(null, null);
      family = f;

      controls = new JPanel();
      controls.setLayout(new BoxLayout(controls, BoxLayout.Y_AXIS));

      final JPanel mig = new JPanel(new MigLayout("inset 5")); //$NON-NLS-1$

      title = new StringConfigurer(null, "", family.getConfigureName()); //$NON-NLS-1$
      title.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          if (evt.getNewValue() != null) {
            family.setConfigureName((String) evt.getNewValue());
          }
        }
      });

      mig.add(new JLabel(Resources.getString("Editor.IconFamily.family_name"))); //$NON-NLS-1$
      mig.add(title.getControls(), "wrap"); //$NON-NLS-1$

      errorLabel = new JLabel(Resources.getString("Editor.IconFamily.name_taken")); //$NON-NLS-1$
      errorLabel.setForeground(Color.red);
      errorLabel.setVisible(false);
      family.addPropertyChangeListener(new PropertyChangeListener() {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          if (Configurable.NAME_PROPERTY.equals(evt.getPropertyName())) {
            final IconFamily savedFamily = IconFactory.getIconFamily(family
                .getName());
            errorLabel.setVisible(savedFamily != null && savedFamily != family);
          }
        }
      });
      mig.add(errorLabel, "span 2,wrap"); //$NON-NLS-1$

      final IconImageConfigurer scalableConfig = new IconImageConfigurer(family);
      mig.add(new JLabel(Resources.getString("Editor.IconFamily.scalable_icon_label"))); //$NON-NLS-1$
      mig.add(scalableConfig.getControls(), "wrap"); //$NON-NLS-1$

      final IconImageConfigurer[] sizeConfig = new IconImageConfigurer[IconFamily.SIZE_COUNT];
      for (int size = 0; size < IconFamily.SIZE_COUNT; size++) {
        sizeConfig[size] = new IconImageConfigurer(family, size);
        final String px = String.valueOf(IconFamily.SIZES[size]);
        mig.add(new JLabel(Resources.getString("Editor.IconFamily.icon_label", IconFamily.getIconSizeNames()[size], px))); //$NON-NLS-1$
        mig.add(sizeConfig[size].getControls(), "wrap"); //$NON-NLS-1$
      }

      controls.add(mig);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    @Override
    public String getValueString() {
      return null;
    }

    @Override
    public void setValue(String s) {

    }

  }

  /**************************************************
   * Configure an individual Icon Image
   */
  static class IconImageConfigurer extends Configurer {

    protected int size;
    protected JPanel controls;
    protected IconFamily family;
    protected int px;
    protected JLabel warningLabel;

    public IconImageConfigurer(IconFamily family, int size) {
      super(null, null);
      this.size = size;
      this.family = family;
      if (size < 0) {
        px = IconFamily.SIZES[IconFamily.LARGE];
      }
      else {
        px = IconFamily.SIZES[size];
      }
    }

    /**
     * Constructor to Configure the scalable icon;
     *
     * @param key
     * @param name
     * @param testElementName
     */
    public IconImageConfigurer(IconFamily family) {
      this(family, -1);
    }

    @Override
    public Component getControls() {
      if (controls == null) {
        controls = new JPanel(new MigLayout());
        controls.add(new JLabel(getName()));

        final JPanel p = new JPanel() {
          private static final long serialVersionUID = 1L;

          @Override
          public void paint(Graphics g) {
            g.clearRect(0, 0, getSize().width, getSize().height);
            final Icon i = getIconValue();
            if (i != null) {
              i.paintIcon(this, g, getSize().width / 2 - i.getIconWidth() / 2,
                  getSize().height / 2 - i.getIconHeight() / 2);
            }
          }
        };
        p.setPreferredSize(new Dimension(px, px));
        controls.add(p);

        final JButton select = new JButton(Resources.getString(Resources.SELECT));
        select.addActionListener(new ActionListener() {
          @Override
          public void actionPerformed(ActionEvent e) {
            selectImage();
            p.repaint();
          }
        });
        controls.add(select, "wrap"); //$NON-NLS-1$

        warningLabel = new JLabel();
        warningLabel.setForeground(Color.red);
        warningLabel.setVisible(false);
        checkIconSize();
        controls.add(warningLabel, "span,wrap"); //$NON-NLS-1$

      }
      return controls;
    }

    @Override
    public String getValueString() {
      if (size < 0) {
        return family.scalablePath;
      }
      else {
        return family.sizePaths[size];
      }
    }

    public Icon getIconValue() {
      Icon icon = null;
      if (family != null) {
        if (size < 0) {
          if (family.scalablePath != null) {
            icon = family.getScalableIcon();
          }
        }
        else {
          if (family.sizePaths[size] != null) {
            icon = family.getIcon(size);
          }
        }
      }
      return icon;
    }

    @Override
    public void setValue(String s) {
      if (size < 0) {
        family.setScalableIconPath(buildPath(s));
      }
      else {
        family.setSizeIconPath(size, buildPath(s));
        checkIconSize();
      }
    }

    protected void checkIconSize() {
      final Icon check = family.getRawIcon(size);
      if (check != null) {
        if (check.getIconHeight() != IconFamily.SIZES[size]) {
          setWarning(Resources.getString("Editor.IconFamily.size_warning", IconFamily.SIZES[size], check.getIconHeight())); //$NON-NLS-1$
        }
      }
    }

    protected String buildPath(String s) {
      if (s == null || s.length() == 0) {
        return null;
      }

      if (size < 0) {
        return ArchiveWriter.ICON_DIR + IconFamily.SCALABLE_DIR + s; //$NON-NLS-1$
      }
      else {
        return ArchiveWriter.ICON_DIR + IconFamily.SIZE_DIRS[size] + s; //$NON-NLS-1$
      }
    }

    protected void setWarning(String warning) {
      warningLabel.setText(warning);
      warningLabel.setVisible(warning != null && warning.length() > 0);
      repack();
    }

    protected void repack() {
      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }

    protected void selectImage() {
      final FileChooser fc = GameModule.getGameModule().getFileChooser();
      fc.setFileFilter(new FamilyImageFilter(family.getName()));
      fc.setSelectedFile(new File(family.getName() + ".*")); //$NON-NLS-1$
      if (fc.showOpenDialog(getControls()) != FileChooser.APPROVE_OPTION) {
        setWarning(""); //$NON-NLS-1$
        setValue(null);
      }
      else {
        final File f = fc.getSelectedFile();
        if (f != null && f.exists()) {
          final String name = f.getName();
          if (name.split("\\.").length != 2) { //$NON-NLS-1$
            showError(Resources.getString("Editor.IconFamily.illegal_icon_name")); //$NON-NLS-1$
          }
          else if (!name.startsWith(family.getName())) {
            showError(Resources.getString("Editor.IconFamily.bad_icon_name", family.getName())); //$NON-NLS-1$
          }
          else if (!ImageUtils.hasImageSuffix(name)) {
            showError(Resources.getString("Editor.IconFamily.bad_icon_file")); //$NON-NLS-1$
          }
          else {
            GameModule.getGameModule().getArchiveWriter().addImage(f.getPath(),
                buildPath(f.getName()));
            setWarning(""); //$NON-NLS-1$
            setValue(name);
          }
        }
        else {
          setWarning(""); //$NON-NLS-1$
          setValue(null);
        }
      }
    }

    protected void showError(String message) {
      Dialogs.showMessageDialog(SwingUtilities.getWindowAncestor(controls),
          Resources.getString("Editor.IconFamily.icon_load_error"),  //$NON-NLS-1$
          Resources.getString("Editor.IconFamily.cannot_load_icon"), message, //$NON-NLS-1$
          JOptionPane.ERROR_MESSAGE);
    }

  }

  /**
   * Filter Icon files matching this family
   *
   */
  static class FamilyImageFilter extends ImageFileFilter {
    private String familyName;

    public FamilyImageFilter(String family) {
      super();
      familyName = family;
    }

    @Override
    public boolean accept(File f) {
      if (super.accept(f)) {
        final String s = f.getName().split("\\.")[0]; //$NON-NLS-1$
        return s.equals(familyName);
      }
      return false;
    }
  }
}
