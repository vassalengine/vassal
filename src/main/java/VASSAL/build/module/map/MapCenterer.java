/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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

import java.awt.Point;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import VASSAL.build.AbstractBuildable;
import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.Deck;
import VASSAL.counters.EventFilter;
import VASSAL.counters.GamePiece;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.Properties;
import VASSAL.tools.swing.SwingUtils;

/**
 * Centers the map when user right-clicks on an empty hex
 */
public class MapCenterer extends AbstractBuildable implements MouseListener {
  private Map map;
  private PieceFinder finder;

  @Override
  public void addTo(Buildable b) {
    finder = createPieceFinder();
    map = (Map) b;
    map.addLocalMouseListener(this);
  }

  /**
   * When the user right-clicks on the Map, the view will center on the location
   * of the click unless this {@link PieceFinder} locates a piece there.
   * @return
   */
  protected PieceFinder createPieceFinder() {
    return new PieceFinder.PieceInStack() {
      @Override
      public Object visitDeck(Deck d) {
        Point pos = d.getPosition();
        Point p = new Point(pt.x - pos.x, pt.y - pos.y);
        return d.getShape().contains(p) ? d : null;
      }
    };
  }

  @Override
  public String[] getAttributeNames() {
    return new String[0];
  }

  @Override
  public String getAttributeValueString(String attName) {
    return null;
  }

  @Override
  public void setAttribute(String attName, Object value) {
  }

  @Override
  public void mouseReleased(MouseEvent e) {
    if (SwingUtils.isRightMouseButton(e)) {
      GamePiece found = map.findPiece(e.getPoint(), finder);
      if (found != null) {
        EventFilter filter = (EventFilter) found.getProperty(Properties.SELECT_EVENT_FILTER);
        if (filter != null
            && filter.rejectEvent(e)) {
          found = null;
        }
      }
      if (found == null) {
        Map.View m = (Map.View) e.getSource();
        m.getMap().centerAt(e.getPoint());
      }
    }
  }

  @Override
  public void mousePressed(MouseEvent e) {
  }

  @Override
  public void mouseEntered(MouseEvent e) {
  }

  @Override
  public void mouseExited(MouseEvent e) {
  }

  @Override
  public void mouseClicked(MouseEvent e) {
  }
}
