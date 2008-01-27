/*
 * $Id$
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

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import javax.imageio.ImageIO;

import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.VisibilityCondition;
import VASSAL.tools.ImageSource;
import VASSAL.tools.UniqueIdManager;
import VASSAL.tools.imageop.SourceOp;

/**
 * 
 */
public class GamePieceImage extends AbstractConfigurable implements Visualizable, Cloneable, ImageSource, UniqueIdManager.Identifyable {

  protected static final String NAME = "name"; //$NON-NLS-1$
  protected static final String PROPS = "props"; //$NON-NLS-1$

  public static final String PART_SIZE = "Size"; //$NON-NLS-1$
  public static final String PART_SYMBOL1 = "Symbol1"; //$NON-NLS-1$
  public static final String PART_SYMBOL2 = "Symbol2"; //$NON-NLS-1$

  public static final String BG_COLOR = "bgColor"; //$NON-NLS-1$
  public static final String BORDER_COLOR = "borderColor"; //$NON-NLS-1$

// FIXME: Why size 5? Comment this to indicate the reason.
  protected List<ItemInstance> instances = new ArrayList<ItemInstance>(5);
  protected InstanceConfigurer defnConfig = null;
  protected GamePieceLayout layout;
  protected ColorSwatch bgColor = ColorSwatch.getWhite();
  protected ColorSwatch borderColor = ColorSwatch.getBlack();
  protected String id;

  protected static UniqueIdManager idMgr = new UniqueIdManager("GamePieceImage"); //$NON-NLS-1$
  protected String nameInUse;
  
  protected static Image NULL_IMAGE = new BufferedImage(1, 1, BufferedImage.TYPE_4BYTE_ABGR); // empty image for images scaled to zero size

  protected SourceOp srcOp;

  // empty image for images scaled to zero size
  protected static final Image NULL_IMAGE =
    new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB);

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
    rebuildInstances();
  }

  public GamePieceImage(GamePieceImage defn) {
    this();
    this.setConfigureName(defn.getConfigureName());
    this.layout = defn.getLayout();
    this.bgColor = defn.getBgColor();
    this.borderColor = defn.getBorderColor();
    this.instances.addAll(defn.getInstances());
  }

  /*
   * The Generic trait needs a deep copy of the Image Definition
   */
  public Object clone() {
    return new GamePieceImage(this);
  }

  public List<ItemInstance> getInstances() {
    return instances;
  }

  public String[] getAttributeDescriptions() {
    return new String[] {
      "Name:  ",
      "Background Color:  ",
      "Border Color:  ",
      "" //$NON-NLS-1$
    };
  }

  public Class[] getAttributeTypes() {
    return new Class[] {
      String.class,
      BgColorSwatchConfig.class,
      BorderColorSwatchConfig.class,
      DefnConfig.class
    };
  }

  public static class BgColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((GamePieceImage) c).getBgColor());
    }
  }

  public static class BorderColorSwatchConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new ColorSwatchConfigurer(key, name, ((GamePieceImage) c).getBorderColor());
    }
  }

  public static class DefnConfig implements ConfigurerFactory {
    static GamePieceImage id;

    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      id = (GamePieceImage) c;
      id.defnConfig = new InstanceConfigurer(key, name, id);
      return id.defnConfig;
    }

    public static void refresh() {
      if (id.defnConfig != null) {
        id.defnConfig.repack();
      }
    }
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, BG_COLOR, BORDER_COLOR, PROPS};
  }

  public ColorSwatch getBgColor() {
    return bgColor;
  }

  public ColorSwatch getBorderColor() {
    return borderColor;
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      final String newName = (String) value;
      setConfigureName(newName);
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
      instances = (List<ItemInstance>) value;
      if (defnConfig != null) {
        rebuildInstances();
        defnConfig.visualizer.rebuild();
        defnConfig.repack();
      }
    }
    rebuildVisualizerImage();
  }

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
    else
      return null;
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (BORDER_COLOR.equals(name)) {
      return borderCond;
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  private VisibilityCondition borderCond = new VisibilityCondition() {
    public boolean shouldBeVisible() {
      if (getLayout() == null) {
        return false;
      }
      else {
        return getLayout().isColoredBorder();
      }
    }
  };

  public void removeFrom(Buildable parent) {
    GameModule.getGameModule().getDataArchive().removeImageSource(getConfigureName());
    idMgr.remove(this);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("GamePieceImage.htm"); //$NON-NLS-1$
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    layout = (GamePieceLayout) parent;
    idMgr.add(this);
    validator = idMgr;
    rebuildInstances();
    setAllAttributesUntranslatable();
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public static String getConfigureTypeName() {
    return "Game Piece Image";
  }

  public void refreshConfig() {
    rebuildVisualizerImage();
  }

  public GamePieceLayout getLayout() {
    return layout;
  }

  public int getVisualizerHeight() {
    return getLayout().getVisualizerHeight();
  }

  public int getVisualizerWidth() {
    return getLayout().getVisualizerWidth();
  }

  // Let the DataArchive cache the image
  public Image getVisualizerImage() {
/*
    try {
      return GameModule.getGameModule().getDataArchive().getCachedImage(getConfigureName());
    }
    catch (IOException e) {
      return NULL_IMAGE;
    }
*/
    srcOp = new SourceOp(getConfigureName());

    try {
      return srcOp.getImage(null);
    }
    catch (CancellationException e) {
        e.printStackTrace();
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
    catch (ExecutionException e) {
      e.printStackTrace();
    }
    return NULL_IMAGE;
  }

  // Called by the DataArchive only when the image is needed
  // This only happens in edit mode, so we add the image data to the archive
  // here
  public Image getImage() {
    final Image i = layout.buildImage(this);
    GameModule.getGameModule()
              .getArchiveWriter()
              .addImage(getConfigureName(), getEncodedImage((BufferedImage) i));
    return i;
  }

  // Invalidate the image data in the Data Archive
  // If we've changed names, remove the data under the old name
  // This only happens in edit mode
  public void rebuildVisualizerImage() {
    if (GameModule.getGameModule().getArchiveWriter() != null) {
      if (nameInUse != null) {
//        GameModule.getGameModule().getDataArchive().unCacheImage(nameInUse);
// FIXME: this is probably not right at all---we need to do something here!
//        ImageCache.remove(new SourceOp(nameInUse));
        if (!nameInUse.equals(getConfigureName())) {
          GameModule.getGameModule().getDataArchive().removeImageSource(nameInUse);
          GameModule.getGameModule().getArchiveWriter().removeImage(nameInUse);
          nameInUse = null;
        }
      }
      if (nameInUse == null &&
          getConfigureName() != null &&
          getConfigureName().length() > 0 &&
          GameModule.getGameModule()
                    .getDataArchive()
                    .addImageSource(getConfigureName(), this)) {
        nameInUse = getConfigureName();
      }
    }
  }

  public byte[] getEncodedImage(BufferedImage bufferedImage) {
    final ByteArrayOutputStream out = new ByteArrayOutputStream();
    try {
      ImageIO.write(bufferedImage,"png", out); //$NON-NLS-1$
    }
    catch (IOException e) {
      e.printStackTrace();
      return new byte[1];
    }
    return out.toByteArray();
  }

  public ItemInstance getInstance(String name) {
    for (ItemInstance instance : instances) {
      if (name.equals(instance.getName())) {
        return instance;
      }
    }
    return null;
  }

  public TextItemInstance getTextInstance(String name) {
    for (ItemInstance instance : instances) {
      if (instance instanceof TextItemInstance) {
        if (name.equals(instance.getName())) {
          return (TextItemInstance) instance;
        }
      }
    }
    return null;
  }

  public TextBoxItemInstance getTextBoxInstance(String name) {
    for (ItemInstance instance : instances) {
      if (instance instanceof TextBoxItemInstance) {
        if (name.equals(instance.getName())) {
          return (TextBoxItemInstance) instance;
        }
      }
    }
    return null;
  }

  public SymbolItemInstance getSymbolInstance(String name) {
    for (ItemInstance instance : instances) {
      if (instance instanceof SymbolItemInstance) {
        if (name.equals(instance.getName())) {
          return (SymbolItemInstance) instance;
        }
      }
    }
    return null;
  }

  public ShapeItemInstance getShapeInstance(String name) {
    for (ItemInstance instance : instances) {
      if (instance instanceof ShapeItemInstance) {
        if (name.equals(instance.getName())) {
          return (ShapeItemInstance) instance;
        }
      }
    }
    return null;
  }

  public ImageItemInstance getImageInstance(String name) {
    for (ItemInstance instance : instances) {
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
    final ArrayList<ItemInstance> newInstances = new ArrayList<ItemInstance>();

    for (ItemInstance prop : instances) {
      final Item item = layout.getItem(prop.getName());
      if (item != null && item.getType().equals(prop.getType())) {
        prop.setLocation(item.getLocation());
        newInstances.add(prop);
      }
    }

    for (Item item : layout.getItems()) {
      final String name = item.getConfigureName();
      final String type = item.getType();
      final String location = item.getLocation();

      boolean found = false;
      for (Iterator i = instances.iterator(); i.hasNext() && !found; ) {
        final ItemInstance prop = (ItemInstance) i.next();
        found = name.equals(prop.getName());
      }

      if (!found) {
        final ItemInstance instance =
          ItemInstance.newDefaultInstance(name, type, location);
        instance.addTo(this);
        newInstances.add(instance);
      }
    }

    instances = newInstances;
    if (defnConfig != null) {
      defnConfig.setValue(instances);
    }
    rebuildVisualizerImage();
  }
}
