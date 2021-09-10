/*
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
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

package VASSAL.build.module.gamepieceimage;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.VisibilityCondition;
import VASSAL.i18n.Resources;
import VASSAL.tools.ArchiveWriter;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.imageop.Op;
import VASSAL.tools.imageop.SourceOp;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.imageio.ImageIO;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.DocumentFilter;

import org.w3c.dom.Element;

/**
 *
 */
public class GamePieceImage extends AbstractConfigurable implements Visualizable, Cloneable, UniqueIdManager.Identifyable {

  protected static final String NAME = "name"; //$NON-NLS-1$
  protected static final String PROPS = "props"; //$NON-NLS-1$

  public static final String PART_SIZE = "Size"; //$NON-NLS-1$
  public static final String PART_SYMBOL1 = "Symbol1"; //$NON-NLS-1$
  public static final String PART_SYMBOL2 = "Symbol2"; //$NON-NLS-1$

  public static final String BG_COLOR = "bgColor"; //$NON-NLS-1$
  public static final String BORDER_COLOR = "borderColor"; //$NON-NLS-1$
  public static final String VERSION = "version";
  public static final String VERSION_0 = "0";
  public static final String VERSION_1 = "1";
  private static final String PNG_SUFFIX = ".png";

  protected List<ItemInstance> instances = new ArrayList<>();
  protected InstanceConfigurer defnConfig = null;
  protected GamePieceLayout layout;
  protected ColorSwatch bgColor = ColorSwatch.getWhite();
  protected ColorSwatch borderColor = ColorSwatch.getBlack();
  protected String id;

  protected static final UniqueIdManager idMgr = new UniqueIdManager("GamePieceImage"); //$NON-NLS-1$
  protected String nameInUse;
  protected Image visImage = null;

  protected SourceOp srcOp;
  /** version number. Existing GPI's loaded from a module will be defaulted to version 0 */
  protected String version = VERSION_0;

  public GamePieceImage() {
    super();
    setConfigureName(""); //$NON-NLS-1$
  }

  public GamePieceImage(String s) {
    instances = InstanceConfigurer.StringToProperties(s, this);
  }

  public GamePieceImage(GamePieceLayout l) {
    this();
    setConfigureName(l.getConfigureName());
    layout = l;
  }

  public GamePieceImage(GamePieceImage defn) {
    this();
    this.setConfigureName(defn.getConfigureName());
    this.layout = defn.getLayout();
    this.bgColor = defn.getBgColor();
    this.borderColor = defn.getBorderColor();
    this.instances.addAll(defn.getInstances());
  }

  @Override
  public void build(Element e) {
    super.build(e);
    // Newly created GPI, force to version 1
    if (e == null) {
      version = VERSION_1;
    }
  }

  /*
   * The Generic trait needs a deep copy of the Image Definition
   */
  @Override
  public Object clone() {
    return new GamePieceImage(this);
  }

  public List<ItemInstance> getInstances() {
    return instances;
  }

  @Override
  public String[] getAttributeDescriptions() {
    return new String[] {
      Resources.getString("Editor.name_label"),
      Resources.getString("Editor.background_color"),
      Resources.getString("Editor.border_color"),
      "",
      ""
    };
  }

  @Override
  public Class<?>[] getAttributeTypes() {
    return new Class<?>[] {
      ImageNameConfig.class,
      BgColorSwatchConfig.class,
      BorderColorSwatchConfig.class,
      DefnConfig.class
    };
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((GamePieceImage) c).getBgColor());
    }
  }

  public static class BorderColorSwatchConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((GamePieceImage) c).getBorderColor());
    }
  }

  public static class DefnConfig implements ConfigurerFactory {
    static GamePieceImage id;

    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      id = (GamePieceImage) c;
      id.defnConfig = new InstanceConfigurer(key, name, id);
      id.rebuildInstances();
      return id.defnConfig;
    }

    public static void refresh() {
      if (id.defnConfig != null) {
        id.defnConfig.repack();
      }
    }
  }

  @Override
  public String[] getAttributeNames() {
    return new String[] {NAME, BG_COLOR, BORDER_COLOR, PROPS, VERSION};
  }

  public ColorSwatch getBgColor() {
    return bgColor;
  }

  public ColorSwatch getBorderColor() {
    return borderColor;
  }

  public String getVersion() {
    return version;
  }

  @Override
  @SuppressWarnings("unchecked")
  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      final String newName = (String) value;
      final String oldName = getConfigureName();
      if (!oldName.equals(newName) && oldName.length() > 0) {
        final ArchiveWriter w = GameModule.getGameModule().getArchiveWriter();
        w.removeImage(oldName);
        w.addImage(newName, getEncodedImage((BufferedImage) visImage));
      }
      setConfigureName(newName);
      // If the user manages to type in a proper V1 image name, flip it over to Version 1
      if (! isVersion1() && isVersion1ImageName(newName)) {
        version = VERSION_1;
      }
    }
    else if (BG_COLOR.equals(key)) {
      if (value instanceof String) {
        value = new ColorSwatch((String) value);
      }
      bgColor = (ColorSwatch) value;
      if (defnConfig != null) {
        defnConfig.visualizer.rebuild();
      }
    }
    else if (BORDER_COLOR.equals(key)) {
      if (value instanceof String) {
        value = new ColorSwatch((String) value);
      }
      borderColor = (ColorSwatch) value;
      if (defnConfig != null) {
        defnConfig.visualizer.rebuild();
      }
    }
    else if (PROPS.equals(key)) {
      if (value instanceof String) {
        value = InstanceConfigurer.StringToProperties((String) value, this);
      }
      if (instances instanceof List<?>) {
        instances = (List<ItemInstance>) value;
      }
      if (defnConfig != null) {
        rebuildInstances();
        defnConfig.visualizer.rebuild();
        defnConfig.repack();
      }
    }
    else if (VERSION.equals(key)) {
      version = (String) value;
    }
    if (defnConfig != null) {
      rebuildVisualizerImage();
    }
  }

  @Override
  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (BG_COLOR.equals(key)) {
      return bgColor.encode();
    }
    else if (BORDER_COLOR.equals(key)) {
      return borderColor.encode();
    }
    else if (PROPS.equals(key)) {
      return InstanceConfigurer.PropertiesToString(instances);
    }
    else if (VERSION.equals(key)) {
      return version;
    }
    else
      return null;
  }

  @Override
  public VisibilityCondition getAttributeVisibility(String name) {
    if (BORDER_COLOR.equals(name)) {
      return borderCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private final VisibilityCondition borderCond = () -> {
    if (getLayout() == null) {
      return false;
    }
    else {
      return getLayout().isColoredBorder();
    }
  };

  @Override
  public void addLocalImageNames(Collection<String> s) {
    if (getConfigureName() != null) s.add(getConfigureName());
  }

  @Override
  public void removeFrom(Buildable parent) {
    idMgr.remove(this);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceImage.html"); //$NON-NLS-1$
  }

  @Override
  public Class<?>[] getAllowableConfigureComponents() {
    return new Class<?>[0];
  }

  @Override
  public void addTo(Buildable parent) {
    layout = (GamePieceLayout) parent;
    idMgr.add(this);
    validator = idMgr;
    setAllAttributesUntranslatable();
    rebuildInstances();
  }

  @Override
  public String getId() {
    return id;
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  public static String getConfigureTypeName() {
    return Resources.getString("Editor.GamePieceImage.component_type");
  }

  public void refreshConfig() {
    rebuildVisualizerImage();
  }

  public GamePieceLayout getLayout() {
    return layout;
  }

  @Override
  public int getVisualizerHeight() {
    return getLayout().getVisualizerHeight();
  }

  @Override
  public int getVisualizerWidth() {
    return getLayout().getVisualizerWidth();
  }

  @Override
  public Image getVisualizerImage() {
    if (visImage == null) {
      rebuildVisualizerImage();
    }
    return visImage;
  }

  // Build the new image and add to the archive
  @Override
  public void rebuildVisualizerImage() {
    if (layout != null) {
      visImage = layout.buildImage(this);

      final ArchiveWriter w = GameModule.getGameModule().getArchiveWriter();
      if (w != null) {
        if (getConfigureName() != null && getConfigureName().length() > 0) {
          w.addImage(getConfigureName(),
                     getEncodedImage((BufferedImage) visImage));
          final SourceOp op = Op.load(getConfigureName());
          op.update();
        }
      }
    }
  }

  public byte[] getEncodedImage(BufferedImage bufferedImage) {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    try {
      ImageIO.write(bufferedImage, "png", out); //$NON-NLS-1$
    }
    catch (IOException e) {
      ErrorDialog.bug(e);
// FIXME: why byte[1] instead of byte[0]?
      return new byte[1];
    }
    return out.toByteArray();
  }

  public ItemInstance getInstance(String name) { //NOPMD
    for (final ItemInstance instance : instances) {
      if (name.equals(instance.getName())) {
        return instance;
      }
    }
    return null;
  }

  public TextItemInstance getTextInstance(String name) {
    for (final ItemInstance instance : instances) {
      if (instance instanceof TextItemInstance) {
        if (name.equals(instance.getName())) {
          return (TextItemInstance) instance;
        }
      }
    }
    return null;
  }

  public TextBoxItemInstance getTextBoxInstance(String name) {
    for (final ItemInstance instance : instances) {
      if (instance instanceof TextBoxItemInstance) {
        if (name.equals(instance.getName())) {
          return (TextBoxItemInstance) instance;
        }
      }
    }
    return null;
  }

  public SymbolItemInstance getSymbolInstance(String name) {
    for (final ItemInstance instance : instances) {
      if (instance instanceof SymbolItemInstance) {
        if (name.equals(instance.getName())) {
          return (SymbolItemInstance) instance;
        }
      }
    }
    return null;
  }

  public ShapeItemInstance getShapeInstance(String name) {
    for (final ItemInstance instance : instances) {
      if (instance instanceof ShapeItemInstance) {
        if (name.equals(instance.getName())) {
          return (ShapeItemInstance) instance;
        }
      }
    }
    return null;
  }

  public ImageItemInstance getImageInstance(String name) {
    for (final ItemInstance instance : instances) {
      if (instance instanceof ImageItemInstance) {
        if (name.equals(instance.getName())) {
          return (ImageItemInstance) instance;
        }
      }
    }
    return null;
  }

  /*
   * Reconcile our current elements with the elements in the owning scheme.
   */
  protected void rebuildInstances() {
    final ArrayList<ItemInstance> newInstances = new ArrayList<>();

    for (final ItemInstance prop : instances) {
      final Item item = layout.getItem(prop.getName());
      if (item != null && item.getType().equals(prop.getType())) {
        prop.setLocation(item.getLocation());
        newInstances.add(prop);
      }
    }

    if (layout != null) {
      for (final Item item : layout.getItems()) {
        final String name = item.getConfigureName();
        final String type = item.getType();
        final String location = item.getLocation();

        boolean found = false;
        for (final ItemInstance prop : instances) {
          found = name.equals(prop.getName());
          if (found) break;
        }

        if (!found) {
          final ItemInstance instance =
            ItemInstance.newDefaultInstance(name, type, location);
          instance.addTo(this);
          newInstances.add(instance);
        }
      }
    }

    instances = newInstances;
    if (defnConfig != null) {
      defnConfig.setValue(instances);
    }
    rebuildVisualizerImage();
  }

  public boolean isVersion1() {
    return VERSION_1.equals(version);
  }

  private boolean isVersion1ImageName(String name) {
    return name != null && !name.isEmpty() && name.matches("^\\w+\\.png$");
  }

  public static class ImageNameConfig implements ConfigurerFactory {
    @Override
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new PngImageNameConfigurer(key, name, (GamePieceImage) c);
    }
  }

  static class PngImageNameConfigurer extends StringConfigurer {

    private final GamePieceImage gpi;

    public PngImageNameConfigurer(String key, String name, GamePieceImage gpi) {
      super(key, name, gpi.getConfigureName());
      this.gpi = gpi;
      setHintKey("Editor.GamePieceImage.png_image_name");
      getControls();
      ((AbstractDocument) nameField.getDocument()).setDocumentFilter(new ImageNameFilter(this));
    }

    public boolean isGpiVersion1() {
      return gpi.isVersion1();
    }

    public void setCaretPosition(int pos) {
      nameField.setCaretPosition(pos);
    }
  }

  /**
   * ImageNameFilter that controls how the user can change the Image Name.
   * If the GPI is still version 0, then no controls. Note we can't force change an image
   * name because other components may reference that image.
   * Once the GPI is version 1, then enforce a .png suffix
   */
  private static class ImageNameFilter extends DocumentFilter {
    private final PngImageNameConfigurer config;

    public ImageNameFilter(PngImageNameConfigurer config) {
      this.config = config;
    }

    /** Return the current value of the text */
    private String getText(FilterBypass fb) {
      final Document doc = fb.getDocument();
      if (doc == null || doc.getLength() == 0) {
        return "";
      }

      String currentValue;
      try {
        currentValue = doc.getText(0, doc.getLength());
      }
      catch (BadLocationException ignored) {
        currentValue = "";
      }

      return currentValue;
    }

    /** Does the current text end in '.png'? */
    private boolean isPng(FilterBypass fb) {
      return getText(fb).toLowerCase().endsWith(PNG_SUFFIX);
    }

    /** Ensure the current value ends in '.png' */
    private void fixPng(FilterBypass fb, int caretPos) throws BadLocationException {
      if (!isPng(fb)) {
        super.replace(fb, getText(fb).length(), 0, PNG_SUFFIX, null);
        config.setCaretPosition(caretPos);
      }
    }

    private String clean(String string) {
      return string.replaceAll("\\W+", "");
    }

    /** Don't let any of the '.png' at the end of the string be removed and clean unwanted characters */
    @Override
    public void remove(FilterBypass fb, int offset, int length) throws BadLocationException {
      if (!config.isGpiVersion1()) {
        super.remove(fb, offset, length);
        return;
      }

      if (isPng(fb)) {
        if ((offset + length) > (getText(fb).length() - PNG_SUFFIX.length())) {
          return;
        }
      }
      super.remove(fb, offset, length);
      fixPng(fb, offset);
    }

    /** Nothing to be inserted within the '.png' at the end and clean unwanted characters */
    @Override
    public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr) throws BadLocationException {
      if (!config.isGpiVersion1()) {
        super.insertString(fb, offset, string, attr);
        return;
      }

      if (isPng(fb)) {
        if ((offset) >= (getText(fb).length() - PNG_SUFFIX.length())) {
          return;
        }
      }
      super.insertString(fb, offset, clean(string), attr);
      fixPng(fb, offset + clean(string).length());
    }

    /** No part of '.png' at then end of the current text to be replaced and clean unwanted characters */
    @Override
    public void replace(FilterBypass fb, int offset, int length, String text, AttributeSet attrs) throws BadLocationException {
      if (!config.isGpiVersion1()) {
        super.replace(fb, offset, length, text, attrs);
        return;
      }

      if (isPng(fb)) {
        if ((offset + length) > (getText(fb).length() - PNG_SUFFIX.length())) {
          return;
        }
      }
      super.replace(fb, offset, length, clean(text), attrs);
      fixPng(fb, length + clean(text).length());
    }
  }
}
