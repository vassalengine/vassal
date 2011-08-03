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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import VASSAL.build.Configurable;
import VASSAL.build.widget.PieceSlot;

/**
 * Edits an entire set of GamePieces at once
 */
public class MassPieceDefiner extends PieceDefiner {
  private static final long serialVersionUID = 1L;

  protected List<Entry> definers;

  public MassPieceDefiner(Configurable top) {
    super();
    definers = new ArrayList<Entry>();
    changed = false;
    init(top);
    if (!definers.isEmpty()) {
      setPiece(definers.get(0).slot.getPiece());
      List<Class<? extends GamePiece>> template = getTemplate();

      for (int i = 0; i < definers.size(); ++i) {
        GamePiece p = definers.get(i).definer.getPiece();
        if (!matchesTemplate(p, template)) {
          definers.remove(i--);
        }
      }
    }
  }

  private void init(Configurable c) {
    if (c instanceof PieceSlot) {
      PieceDefiner def = new Def();
      def.setPiece(((PieceSlot) c).getPiece());
      definers.add(new Entry((PieceSlot) c, def));
    }
    Configurable[] child = c.getConfigureComponents();
    for (int i = 0; i < child.length; ++i) {
      init(child[i]);
    }
  }

  private List<Class<? extends GamePiece>> getTemplate() {
    GamePiece p = definers.get(0).definer.getPiece();
    ArrayList<Class<? extends GamePiece>> types =
      new ArrayList<Class<? extends GamePiece>>();
    while (p instanceof Decorator) {
      types.add(p.getClass());
      p = ((Decorator) p).piece;
    }
    types.add(p.getClass());
    return types;
  }

  private boolean matchesTemplate(GamePiece p,
                                  List<Class<? extends GamePiece>> template) {
    Iterator<Class<? extends GamePiece>> i = template.iterator();
    while (p instanceof Decorator && i.hasNext()) {
      if (p.getClass() != i.next()) {
        return false;
      }
      p = ((Decorator) p).piece;
    }
    return i.hasNext() ? p.getClass() == i.next() && !i.hasNext() : false;
  }

  @Override
  protected void addTrait(Decorator c) {
    super.addTrait(c);
    for (Entry e : definers) {
      e.definer.addTrait(c);
    }
  }

  @Override
  protected void removeTrait(int index) {
    super.removeTrait(index);
    for (Entry e : definers) {
      e.definer.removeTrait(index);
    }
  }

  @Override
  protected void moveDecoratorUp(int index) {
    super.moveDecoratorUp(index);
    for (Entry e : definers) {
      e.definer.moveDecoratorUp(index);
    }
  }

  @Override
  protected void moveDecoratorDown(int index) {
    super.moveDecoratorDown(index);
    for (Entry e : definers) {
      e.definer.moveDecoratorDown(index);
    }
  }

  @Override
  protected void paste() {
    super.paste();
    for (Entry e : definers) {
      e.definer.paste();
    }
  }

  @Override
  protected boolean edit(int index) {
    boolean result = super.edit(index);
    for (Entry e : definers) {
      e.definer.edit(index);
    }
    return result;
  }

  public void save() {
    for (Entry e : definers) {
      e.slot.setPiece(e.definer.getPiece());
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
    private static final long serialVersionUID = 1L;

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
