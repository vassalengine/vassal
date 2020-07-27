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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import VASSAL.build.Buildable;
import VASSAL.build.module.Map;
import VASSAL.counters.GamePiece;
import VASSAL.counters.KeyBuffer;
import VASSAL.counters.PieceFinder;
import VASSAL.counters.Stack;
import VASSAL.tools.swing.SwingUtils;

public class StackExpander extends MouseAdapter implements Buildable {
  protected Map map;

  @Override
  public void addTo(Buildable b) {
    map = (Map) b;
    map.addLocalMouseListener(this);
  }

  @Override
  public void add(Buildable b) {
  }

  @Override
  public Element getBuildElement(Document doc) {
    return doc.createElement(getClass().getName());
  }

  @Override
  public void build(Element e) {
  }

// FIXME: should be mouseClicked()?
  @Override
  public void mouseReleased(MouseEvent e) {
    if (!e.isConsumed() && e.getClickCount() == 2
                        && SwingUtils.isLeftMouseButton(e)) {
      final GamePiece p = map.findPiece(e.getPoint(), PieceFinder.STACK_ONLY);
      if (p != null) {
        KeyBuffer.getBuffer().clear();
        ((Stack) p).setExpanded(!((Stack) p).isExpanded());
        KeyBuffer.getBuffer().add(((Stack) p).topPiece());
      }
      e.consume();
    }
  }
}
