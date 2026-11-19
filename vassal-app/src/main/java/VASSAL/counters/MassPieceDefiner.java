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

import VASSAL.configure.ConfigureTree;
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
  protected ConfigureTree tree;

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

  public MassPieceDefiner(Configurable top, ConfigureTree tree) {
    this(top);
    this.tree = tree;
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
    final List<Class<? extends GamePiece>> types = new ArrayList<>();
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
  protected void moveDecoratorTop(int index) {
    super.moveDecoratorTop(index);
    for (final Entry e : definers) {
      e.definer.moveDecoratorTop(index);
    }
  }

  @Override
  protected void moveDecoratorBottom(int index) {
    super.moveDecoratorBottom(index);
    for (final Entry e : definers) {
      e.definer.moveDecoratorBottom(index);
    }
  }

  @Override
  protected void paste() {
    // Resolve where the trait goes once, from the list the user can see, and give that
    // position to every piece. Calling the no-argument paste() on the others made each of
    // them read its own hidden list instead, whose selection is still wherever setPiece()
    // left it, so the pasted trait landed at a different depth in every piece and the
    // trait lists --- which the whole dialog assumes are identical --- drifted apart.
    final int insertIndex = getSelectedTraitIndex();
    super.paste(insertIndex);
    for (final Entry e : definers) {
      e.definer.paste(insertIndex);
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
      tree.nodeEdited(e.slot);
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

      // Every piece being edited together starts with the same list of traits, and each
      // structural change is applied to all of them at the same position, so the trait at
      // this position is the same kind of trait in every piece. If that has nonetheless
      // stopped being true, feeding one trait's type string to a different kind of trait
      // would either corrupt the piece or --- more usually --- throw out of the receiving
      // trait's own decoder, which is what vassalengine/vassal#15027, #13607 and #12377
      // report. Leave a piece that does not match alone rather than damage it.
      if (index >= this.inUseModel.size()) {
        return false;
      }
      final Object mine = this.inUseModel.elementAt(index);
      if (mine.getClass() != o.getClass()) {
        return false;
      }

      final PieceEditor template = ((EditablePiece) o).getEditor();
      final EditablePiece myPiece = (EditablePiece) mine;
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
