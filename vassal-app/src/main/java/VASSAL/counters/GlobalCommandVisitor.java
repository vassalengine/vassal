/*
 *
 * Copyright (c) 2020-2023 by vassalengine.org, Brian Reynolds
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

import VASSAL.command.Command;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;

import javax.swing.KeyStroke;
import java.util.HashSet;
import java.util.Set;

/**
 * When processing a GlobalCommand (either a Global Key Command or an Attachment command), this applies the
 * "additional properties filter"
 */
public class GlobalCommandVisitor implements DeckVisitor {
  protected Command command;
  private final BoundsTracker tracker;
  protected final PieceFilter filter;
  private final KeyStroke stroke;
  protected int selectedCount;
  private Auditable owner;
  protected final AuditTrail auditSoFar;
  private int selectFromDeck;
  private GlobalCommand globalCommand;

  // Keep track of the ID's of all pieces processed by this GKC.
  private final Set<String> seen = new HashSet<>();


  public GlobalCommandVisitor(Command command, PieceFilter filter, KeyStroke stroke) {
    this(command, filter, stroke, null);
  }

  public GlobalCommandVisitor(Command command, PieceFilter filter, KeyStroke stroke, AuditTrail audit) {
    this.command = command;
    tracker = new BoundsTracker();
    this.filter = filter;
    this.stroke = stroke;
    auditSoFar = audit;
  }

  public GlobalCommandVisitor(Command command, PieceFilter filter, KeyStroke stroke, AuditTrail audit, Auditable owner, int selectFromDeck) {
    this(command, filter, stroke, audit);
    this.owner = owner;
    this.selectFromDeck = selectFromDeck;
  }

  public GlobalCommandVisitor(Command command, PieceFilter filter, KeyStroke stroke, AuditTrail audit, Auditable owner, int selectFromDeck, GlobalCommand globalCommand) {
    this(command, filter, stroke, audit, owner, selectFromDeck);
    this.globalCommand = globalCommand;
  }

  public void setOwner(Auditable val) {
    owner = val;
  }

  public void setSelectFromDeck(int val) {
    selectFromDeck = val;
  }

  public int getSelectFromDeck() {
    return selectFromDeck;
  }

  public void setSelectedCount(int selectedCount) {
    this.selectedCount = selectedCount;
  }

  public int getSelectedCount() {
    return selectedCount;
  }

  @Override
  public Object visitDeck(Deck d) {
    if (!d.isAccessible()) {
      return null;
    }
    if (getSelectFromDeck() != 0) {

      // selectFromDeck = -1 means process all cards in Deck
      // selectFromDeck > 0 means select that many cards from the Deck

      // Ask for all cards to be drawn.
      d.setDragCount(d.getPieceCount());

      // Keep drawing until required select count met or all cards in Deck have been processed
      selectedCount = 0;
      for (final PieceIterator it = d.drawCards(); it.hasMoreElements() && (getSelectFromDeck() < 0 || getSelectFromDeck() > selectedCount);) {
        apply(it.nextPiece(), true);
      }
    }
    return null;
  }

  @Override
  public Object visitStack(Stack s) {
    s.asList().forEach(this::apply);
    return null;
  }

  @Override
  public Object visitDefault(GamePiece p) {
    apply(p);
    return null;
  }

  protected void apply(GamePiece p) {
    apply(p, false);
  }

  protected void apply(GamePiece p, boolean visitingDeck) {

    /*
      Pieces can change Stacks as a result of the GKC being executed and due to the way GlobalCommand
      has been coded, this can result in a piece being sent the GKC a second time. Rather than hack into
      GlobalCommand code and potentially change the order that things are being executed, just maintain a
      Set of the piece IDs that have been sent the GKC so far and prevent duplicates. This will have the
      minimal impact possible on existing functionality.
     */
    final String uid = (String) p.getProperty(BasicPiece.PIECE_UID);
    if (seen.contains(uid)) {
      return;
    }
    seen.add(uid);

    /*
      If an AuditTrail has been supplied for the evaulation history of the filter up to this point,
      then clone it for applying to each individual piece.
     */
    AuditTrail audit = null;
    if (auditSoFar != null) {
      audit = new AuditTrail(auditSoFar);
      audit.addMessage(Resources.getString("Audit.gkc_applied_to", p.getComponentName()));
    }

    if (filter == null || filter.accept(p, owner, audit)) {
      if (visitingDeck) {
        p.setProperty(Properties.OBSCURED_BY, p.getProperty(Properties.OBSCURED_BY_PRE_DRAW));  // Bug 13433 restore correct OBSCURED_BY after checking filter
      }
      tracker.addPiece(p);
      p.setProperty(Properties.SNAPSHOT, ((PropertyExporter) p).getProperties());

      // Set any Parameters into the piece prior to issuing the Key Command
      command = command.append(Decorator.setDynamicProperties(
        globalCommand.getParameters(),
        p,
        globalCommand.getPropertySource(),
        owner,
        auditSoFar
      ));

      command = command.append(p.keyEvent(stroke));
      tracker.addPiece(p);
      selectedCount++;
    }
    else {
      if (visitingDeck) {
        p.setProperty(Properties.OBSCURED_BY, p.getProperty(Properties.OBSCURED_BY_PRE_DRAW));  // Bug 13433 restore correct OBSCURED_BY
      }
    }
  }

  public Command getCommand() {
    return command;
  }

  public BoundsTracker getTracker() {
    return tracker;
  }
}
