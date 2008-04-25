
package VASSAL.tools;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import javax.swing.Box;
import javax.swing.JComponent;
import javax.swing.LayoutStyle;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

public class UIUtils {
  private UIUtils() { }

  // Figure 26
  private static final Border dialogPadding = new EmptyBorder(12,12,11,11);

  public Border dialogPadding() {
    return dialogPadding; 
  }

  // Figure 29
  private static final Dimension columnSeparator =
    new Dimension(11,0);

  public static Component columnSeparator() {
    return Box.createRigidArea(columnSeparator);
  }

  // Figure 33
  private static final Dimension extraGroupRowSeparator = new Dimension(0,11);

  public static Component extraGroupRowSeparator() {
    return Box.createRigidArea(extraGroupRowSeparator);
  }

  // Figure 33
  private static final Dimension intraGroupRowSeparator = new Dimension(0,5);

  public static Component intraGroupRowSeparator() {
    return Box.createRigidArea(intraGroupRowSeparator);
  }

  // Figure 27
  private static final Dimension commandButtonRowSeparator =
    new Dimension(0,17);

  public static Component commandButtonRowSeparator() {
    return Box.createRigidArea(commandButtonRowSeparator);
  }

  // Figure 31
  private static final Dimension labelGutter = new Dimension(12,0);
  
  public static Component labelGutter() {
    return Box.createRigidArea(labelGutter);
  }  

  // Figure 34
  private static final Dimension intraGroupToolbarButtonGutter =
    new Dimension(2,0);
  
  public static Component intraGroupToolbarButtonGutter() {
    return Box.createRigidArea(intraGroupToolbarButtonGutter);
  }

  // Figure 34
  private static final Dimension extraGroupToolbarButtonGutter =
    new Dimension(11,0);

  public static Component extraGroupToolbarButtonGutter() {
    return Box.createRigidArea(extraGroupToolbarButtonGutter);
  }

// toolbar button margins: 3px above, 2px below
// no gutter when responding to mouse-over events

  // Figure 36
  private static final Dimension exclusiveToggleButtonGutter =
    new Dimension(2,0);

  public static Component exclusiveToggleButtonGutter() {
    return Box.createRigidArea(exclusiveToggleButtonGutter);
  }

  // Figure 38
  private static final Dimension commandButtonGutter = new Dimension(5,0);

  public static Component commandButtonGutter() {
    return Box.createRigidArea(commandButtonGutter);
  }
  
   // Figure 41
  private static final Border titledPadding = new EmptyBorder(12,12,11,11);

  public static Border titledPadding() {
    return titledPadding; 
  }

  

  public static void maximizeWidths(Component... c) {
    final Dimension[] d = new Dimension[c.length];

    // find widest component
    int maxwidth = 0;
    for (int i = 0; i < c.length; i++) {
      d[i] = c[i].getPreferredSize();
      if (d[i].width > maxwidth) maxwidth = d[i].width;
    }

    // set all components to the width of the widest
    for (int i = 0; i < c.length; i++) {
      d[i].width = maxwidth;
      c[i].setPreferredSize(d[i]);
    }
  }

  public static void maxmizeHeights(Component... c) {
    final Dimension[] d = new Dimension[c.length];

    // find tallest component
    int maxheight = 0;
    for (int i = 0; i < c.length; i++) {
      d[i] = c[i].getPreferredSize();
      if (d[i].height > maxheight) maxheight = d[i].height;
    }

    // set all components to the height of the tallests
    for (int i = 0; i < c.length; i++) {
      d[i].height = maxheight;
      c[i].setPreferredSize(d[i]);
    }
  }

/*
  public static Box createControlButtonBox() {
    return new Box(BoxLayout.X_AXIS) {
      public Component add(JButton b) {
        
      }
    };

    final Box box = Box.createHorizontalBox();
    box.add(Box.createHorizontalGlue());
    
  }
*/

  private static final LayoutStyle style = LayoutStyle.getInstance();

  public static Border createPadding(JComponent component, Container parent) {
    return new EmptyBorder(
      style.getContainerGap(component, SwingConstants.NORTH, parent),
      style.getContainerGap(component, SwingConstants.WEST, parent),
      style.getContainerGap(component, SwingConstants.SOUTH, parent),
      style.getContainerGap(component, SwingConstants.EAST, parent)
    );
  }

  public static Component createRelatedHGap(JComponent a, JComponent b) {
    return Box.createRigidArea(new Dimension(
      style.getPreferredGap(a, b,
        LayoutStyle.ComponentPlacement.RELATED,
        SwingConstants.EAST, null),
      0)
    );
  }

  public static Component createRelatedVGap(JComponent a, JComponent b) {
    return Box.createRigidArea(new Dimension(
      0,
      style.getPreferredGap(a, b,
        LayoutStyle.ComponentPlacement.RELATED,
        SwingConstants.SOUTH, null)
      )
    );
  }

  public static Component createUnrelatedHGap(JComponent a, JComponent b) {
    return Box.createRigidArea(new Dimension(
      style.getPreferredGap(a, b,
        LayoutStyle.ComponentPlacement.UNRELATED,
        SwingConstants.EAST, null),
      0)
    );
  }

  public static Component createUnrelatedVGap(JComponent a, JComponent b) {
    return Box.createRigidArea(new Dimension(
      0,
      style.getPreferredGap(a, b,
        LayoutStyle.ComponentPlacement.UNRELATED,
        SwingConstants.SOUTH, null)
      )
    );
  }
}
