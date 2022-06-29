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
    definers = new ArrayList<>();
    changed = false;
    init(top);
    if (!definers.isEmpty()) {
      setPiece(PieceCloner.getInstance().clonePieceUnexpanded(definers.get(0).slot.getPiece()));
      final List<Class<? extends GamePiece>> template = getTemplate();

      for (int i = 0; i < definers.size(); ++i) {
        final GamePiece p = definers.get(i).definer.getPiece();
        if (!matchesTemplate(p, template)) {
          definers.remove(i--);
        }
      }
    }
  }

  private void init(Configurable c) {
    if (c instanceof PieceSlot) {
      final PieceSlot slot = (PieceSlot) c;
      final PieceDefiner def = new Def();
      def.setPiece(PieceCloner.getInstance().clonePieceUnexpanded(slot.getPiece()));
      definers.add(new Entry(slot, def));
    }
    final Configurable[] child = c.getConfigureComponents();
    for (final Configurable configurable : child) {
      init(configurable);
    }
  }

  private List<Class<? extends GamePiece>> getTemplate() {
    GamePiece p = PieceCloner.getInstance().clonePieceUnexpanded(definers.get(0).definer.getPiece());
    final ArrayList<Class<? extends GamePiece>> types = new ArrayList<>();
    while (p instanceof Decorator) {
      types.add(p.getClass());
      p = ((Decorator) p).piece;
    }
    types.add(p.getClass());
    return types;
  }

  private boolean matchesTemplate(GamePiece p,
                                  List<Class<? extends GamePiece>> template) {
    final Iterator<Class<? extends GamePiece>> i = template.iterator();
    while (p instanceof Decorator && i.hasNext()) {
      if (p.getClass() != i.next()) {
        return false;
      }
      p = ((Decorator) p).piece;
    }
    return i.hasNext() && p.getClass() == i.next() && !i.hasNext();
  }

  @Override
  protected void addTrait(Decorator c) {
    super.addTrait(c);
    for (final Entry e : definers) {
      e.definer.addTrait(c);
    }
  }

  @Override
  protected void addTrait(Decorator c, int insertIndex) {
    super.addTrait(c, insertIndex);
    for (final Entry e : definers) {
      e.definer.addTrait(c, insertIndex);
    }
  }

  @Override
  protected void removeTrait(int index) {
    super.removeTrait(index);
    for (final Entry e : definers) {
      e.definer.removeTrait(index);
    }
  }

  @Override
  protected void moveDecoratorUp(int index) {
    super.moveDecoratorUp(index);
    for (final Entry e : definers) {
      e.definer.moveDecoratorUp(index);
    }
  }

  @Override
  protected void moveDecoratorDown(int index) {
    super.moveDecoratorDown(index);
    for (final Entry e : definers) {
      e.definer.moveDecoratorDown(index);
    }
  }

  @Override
  protected void paste() {
    super.paste();
    for (final Entry e : definers) {
      e.definer.paste();
    }
  }

  @Override
  protected boolean edit(int index) {
    final boolean result = super.edit(index);
    for (final Entry e : definers) {
      e.definer.edit(index);
    }
    return result;
  }

  public void save() {
    for (final Entry e : definers) {
      e.slot.setPiece(e.definer.getPiece());
    }
  }

  private static class Entry {
    private final PieceSlot slot;
    private final PieceDefiner definer;

    private Entry(PieceSlot slot, PieceDefiner definer) {
      this.slot = slot;
      this.definer = definer;
    }
  }

  private class Def extends PieceDefiner {
    private static final long serialVersionUID = 1L;

    @Override
    protected boolean edit(int index) {
      final Object o = MassPieceDefiner.this.inUseModel.elementAt(index);
      if (!(o instanceof EditablePiece)) {
        return false;
      }
      final PieceEditor template = ((EditablePiece) o).getEditor();
      final EditablePiece myPiece = (EditablePiece) this.inUseModel.elementAt(index);
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
