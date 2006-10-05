/*
 * $Id$
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
package VASSAL.counters;

import java.util.Enumeration;
import java.util.Vector;

import VASSAL.build.Configurable;
import VASSAL.build.widget.PieceSlot;

/**
 * Edits an entire set of GamePieces at once
 */
public class MassPieceDefiner extends PieceDefiner {
  private Vector definers;

  public MassPieceDefiner(Configurable top) {
    super();
    definers = new Vector();
    init(top);
    if (definers.size() > 0) {
      setPiece(((Entry) definers.firstElement()).slot.getPiece());
      Vector template = getTemplate();
      for (int i = 0; i < definers.size(); ++i) {
        GamePiece p = ((Entry) definers.elementAt(i)).definer.getPiece();
        if (!matchesTemplate(p, template)) {
          definers.removeElementAt(i--);
        }
      }
    }
  }

  private void init(Configurable c) {
    if (c instanceof PieceSlot) {
      PieceDefiner def = new Def();
      def.setPiece(((PieceSlot) c).getPiece());
      definers.addElement(new Entry((PieceSlot) c, def));
    }
    Configurable[] child = c.getConfigureComponents();
    for (int i = 0; i < child.length; ++i) {
      init(child[i]);
    }
  }

  private Vector getTemplate() {
    GamePiece p = ((Entry) definers.firstElement()).definer.getPiece();
    Vector types = new Vector();
    while (p instanceof Decorator) {
      types.addElement(p.getClass());
      p = ((Decorator) p).piece;
    }
    types.addElement(p.getClass());
    return types;
  }

  private boolean matchesTemplate(GamePiece p, Vector template) {
    Enumeration e = template.elements();
    while (p instanceof Decorator
      && e.hasMoreElements()) {
      if (p.getClass() != e.nextElement()) {
        return false;
      }
      p = ((Decorator) p).piece;
    }
    return e.hasMoreElements() ?
      p.getClass() == e.nextElement() && !e.hasMoreElements()
      : false;
  }

  protected void addTrait(Decorator c) {
    super.addTrait(c);
    for (Enumeration e = definers.elements();
         e.hasMoreElements();) {
      ((Entry) e.nextElement()).definer.addTrait(c);
    }
  }

  protected void removeTrait(int index) {
    super.removeTrait(index);
    for (Enumeration e = definers.elements();
         e.hasMoreElements();) {
      ((Entry) e.nextElement()).definer.removeTrait(index);
    }
  }

  protected void moveDecoratorUp(int index) {
    super.moveDecoratorUp(index);
    for (Enumeration e = definers.elements();
         e.hasMoreElements();) {
      ((Entry) e.nextElement()).definer.moveDecoratorUp(index);
    }
  }

  protected void moveDecoratorDown(int index) {
    super.moveDecoratorDown(index);
    for (Enumeration e = definers.elements();
         e.hasMoreElements();) {
      ((Entry) e.nextElement()).definer.moveDecoratorDown(index);
    }
  }

  protected boolean edit(int index) {
    boolean result = super.edit(index);
    for (Enumeration e = definers.elements();
         e.hasMoreElements();) {
      ((Entry) e.nextElement()).definer.edit(index);
    }
    return result;
  }

  public void save() {
    for (Enumeration e = definers.elements();
         e.hasMoreElements();) {
      Entry entry = (Entry) e.nextElement();
      entry.slot.setPiece(entry.definer.getPiece());
    }
  }

  private static class Entry {
    private PieceSlot slot;
    private PieceDefiner definer;

    private Entry(PieceSlot slot, PieceDefiner definer) {
      this.slot = slot;
      this.definer = definer;
    }
  }

  private class Def extends PieceDefiner {
    protected boolean edit(int index) {
      Object o = MassPieceDefiner.this.inUseModel.elementAt(index);
      if (!(o instanceof EditablePiece)) {
        return false;
      }
      PieceEditor template = ((EditablePiece) o).getEditor();
      EditablePiece myPiece = (EditablePiece) this.inUseModel.elementAt(index);
      myPiece.mySetType(template.getType());
      if (myPiece instanceof Decorator) {
        ((Decorator) myPiece).mySetState(template.getState());
      }
      else {
        myPiece.setState(template.getState());
      }
      return true;
    }
  }
}
