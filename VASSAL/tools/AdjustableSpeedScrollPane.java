/*
 * $Id$
 *
 * Copyright (c) 2006 by Joel Uckelman
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
package VASSAL.tools;

import java.awt.Component;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import VASSAL.build.GameModule;
import VASSAL.configure.IntConfigurer;
import VASSAL.preferences.Prefs;
import VASSAL.tools.ScrollPane;

/**
   AdjustableSpeedScrollPane extends {@link ScrollPane} by making the
   scroll speed user-configurable. Use AdjustableScrollPane instead
   of ScrollPane wherever a scrollpane for large images is needed.

   @author Joel Uckelman
   @see VASSAL.tools.ScrollPane
   @see javax.swing.JScrollPane
*/
public class AdjustableSpeedScrollPane extends ScrollPane {
   private static final String SCROLL_SPEED = "scrollSpeed";
   private static final int defaultSpeed = 50;

   /**
     Creates an AdjustableSpeedScrollPane that displays the contents of the
     specified component, where both horizontal and vertical scrollbars
     appear whenever the component's contents are larger than the view.

     @param view the component to display in the scrollpane's viewport
   */
   public AdjustableSpeedScrollPane(Component view) {
      this(view, VERTICAL_SCROLLBAR_AS_NEEDED, 
                 HORIZONTAL_SCROLLBAR_AS_NEEDED);
   }

   /**
     Creates an AdjustableSpeedScrollPane that displays the view component
     in a viewport with the specified scrollbar policies. The available
     policy settings are listed at
     {@link JScrollPane#setVerticalScrollBarPolicy} and
     {@link JScrollPane#setHorizontalScrollBarPolicy}.

     @param view the component to display in the scrollpane's viewport
     @param vsbPolicy an integer that specifies the vertical scrollbar policy
     @param hsbPolicy an integer that specifies the horizontal scrollbar policy    */
   public AdjustableSpeedScrollPane(Component view, int vsbPolicy,
      int hsbPolicy) {

      super(view, vsbPolicy, hsbPolicy);

      // set configurer      
      IntConfigurer config = new IntConfigurer(SCROLL_SPEED,
         "Scroll increment (pixels)", new Integer(defaultSpeed));
      config.addPropertyChangeListener(new PropertyChangeListener() {
         public void propertyChange(PropertyChangeEvent e) {
            if (SCROLL_SPEED.equals(e.getPropertyName()))
               setSpeed(((Integer) e.getNewValue()).intValue());
         }
      });

      Prefs prefs = GameModule.getGameModule().getPrefs();
      prefs.addOption("General", config);
      setSpeed(((Integer) prefs.getValue(SCROLL_SPEED)).intValue());
   }

   private void setSpeed(int speed) {
      verticalScrollBar.setUnitIncrement(speed);
      horizontalScrollBar.setUnitIncrement(speed);
   }
}
