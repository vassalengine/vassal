package VASSAL.build.module.map;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import VASSAL.build.AbstractConfigurable;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.IconConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.VisibilityCondition;
import VASSAL.counters.GamePiece;
import VASSAL.counters.Highlighter;

public class SelectionHighlighter extends AbstractConfigurable implements Highlighter {

  public static final String NAME = "name";
  public static final String MATCH = "match";
  public static final String COLOR = "color";
  public static final String THICKNESS = "thickness";
  public static final String USE_IMAGE = "useImage";
  public static final String IMAGE = "image";
  public static final String X_OFFSET = "xoffset";
  public static final String Y_OFFSET = "yoffset";

  protected PropertyExpression matchProperties = new PropertyExpression();
  protected Color color = Color.RED;
  protected int thickness = 3;
  protected boolean useImage = false;
  protected String imageName = "";
  protected int x = 0;
  protected int y = 0;

  protected VisibilityCondition visibilityCondition;
  protected Image image;

  public void draw(GamePiece p, Graphics g, int x, int y, Component obs, double zoom) {
    Graphics2D g2d = (Graphics2D) g;
    if (accept(p)) {
      if (useImage) {
        if (image != null) {
          int x1 = x - (int) (image.getWidth(null) * zoom / 2);
          int y1 = y - (int) (image.getHeight(null) * zoom / 2);
          if (zoom == 1.0) {
            g2d.drawImage(image, x1, y1, null);
          }
          else {
            g2d.drawImage(GameModule.getGameModule().getDataArchive().getScaledImage(image, zoom), x1, y1, null);
          }
        }
      }
      else {
        if (color == null || thickness <= 0) {
          return;
        }
        Shape s = p.getShape();
        Stroke str = g2d.getStroke();
        g2d.setStroke(new BasicStroke(Math.max(1,Math.round(zoom*thickness))));
        g2d.setColor(color);
        AffineTransform t = AffineTransform.getScaleInstance(zoom,zoom);
        t.translate(x/zoom,y/zoom);
        g2d.draw(t.createTransformedShape(s));
        g2d.setStroke(str);
      }
    }
  }

  public Rectangle boundingBox(GamePiece p) {
    Rectangle r = p.getShape().getBounds();

    if (accept(p)) {
      if (useImage) {
        if (image != null) {
          int width = image.getWidth(null);
          int height = image.getHeight(null);
          r = r.union(new Rectangle(-width/2, -height/2, width, height));
        }
      }
      else {
        r.translate(-thickness, -thickness);
        r.setSize(r.width + 2 * thickness, r.height + 2 * thickness);
      }
    }
    return r;
  }

  protected boolean accept(GamePiece p) {
    if (matchProperties.isNull()) {
      return true;
    }
    
    return matchProperties.accept(p);
  }
  
  public static String getConfigureTypeName() {
    return "Highlighter";
  }
  
  public String[] getAttributeDescriptions() {
    return new String[] {"Name:  ", "Active if Properties Match:  ", "Use Image",
        "Border Color:  ", "Border Thickness:  ", "Image:  ", "X Offset:  ", "Y Offset:  "};
  }

  public Class[] getAttributeTypes() {
    return new Class[] {String.class, PropertyExpression.class, Boolean.class, Color.class, Integer.class,
        IconConfig.class, Integer.class, Integer.class};
  }

  public static class IconConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new IconConfigurer(key, name, ((SelectionHighlighter) c).imageName);
    }
  }

  public String[] getAttributeNames() {
    return new String[] {NAME, MATCH, USE_IMAGE, COLOR, THICKNESS, IMAGE, X_OFFSET, Y_OFFSET};
  }

  public VisibilityCondition getAttributeVisibility(String name) {
    if (COLOR.equals(name) || THICKNESS.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return !useImage;
        }
      };
    }
    else if (IMAGE.equals(name) || X_OFFSET.equals(name) || Y_OFFSET.equals(name)) {
      return new VisibilityCondition() {
        public boolean shouldBeVisible() {
          return useImage;
        }
      };
    }
    else {
      return super.getAttributeVisibility(name);
    }
  }

  public void setAttribute(String key, Object value) {
    if (key.equals(NAME)) {
      setConfigureName((String) value);
    }
    else if (key.equals(MATCH)) {
      matchProperties.setExpression((String) value);
    }
    else if (key.equals(USE_IMAGE)) {
      if (value instanceof String) {
        value = new Boolean((String) value);
      }
      useImage = ((Boolean) value).booleanValue();

    }
    else if (key.equals(COLOR)) {
      if (value instanceof String) {
        value = ColorConfigurer.stringToColor((String) value);
      }
      if (value != null) {
        color = ((Color) value);
      }
    }
    else if (key.equals(THICKNESS)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      try {
        thickness = ((Integer) value).intValue();
      }
      catch (NumberFormatException ex) {
      }
    }
    else if (key.equals(IMAGE)) {
      imageName = (String) value;
      if (imageName.equals("")) {
        image = null;
      }
      else {        
        try {
          image = GameModule.getGameModule().getDataArchive().getCachedImage(imageName);
        }
        catch (Exception ex) {
          image = null;
        }
      }
    }
    else if (key.equals(X_OFFSET)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      try {
        x = ((Integer) value).intValue();
      }
      catch (NumberFormatException ex) {
      }
    }
    else if (key.equals(Y_OFFSET)) {
      if (value instanceof String) {
        value = new Integer((String) value);
      }
      try {
        y = ((Integer) value).intValue();
      }
      catch (NumberFormatException ex) {
      }
    }

  }

  public String getAttributeValueString(String key) {
    if (key.equals(NAME)) {
      return getConfigureName();
    }
    else if (key.equals(MATCH)) {
      return matchProperties.getExpression();
    }
    else if (key.equals(USE_IMAGE)) {
      return String.valueOf(useImage);
    }
    else if (key.equals(COLOR)) {
      return ColorConfigurer.colorToString(color);
    }
    else if (key.equals(THICKNESS)) {
      return String.valueOf(thickness);
    }
    else if (key.equals(IMAGE)) {
      return imageName;
    }
    else if (key.equals(X_OFFSET)) {
      return String.valueOf(x);
    }
    else if (key.equals(Y_OFFSET)) {
      return String.valueOf(y);
    }
    return null;
  }

  public void removeFrom(Buildable parent) {
    ((SelectionHighlighters) parent).removeHighlighter(this);
  }

  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Map.htm","SelectionHighlighter");
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[0];
  }

  public void addTo(Buildable parent) {
    ((SelectionHighlighters) parent).addHighlighter(this);
  }

}
