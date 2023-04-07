package VASSAL.counters;

import VASSAL.command.Command;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;

import javax.swing.KeyStroke;

public class GlobalCommandVisitor implements DeckVisitor {
  private final Command command;
  private final BoundsTracker tracker;
  protected final PieceFilter filter;
  private final KeyStroke stroke;
  protected int selectedCount;
  private Auditable owner;
  protected final AuditTrail auditSoFar;
  private int selectFromDeck;


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

  private void apply(GamePiece p) {
    apply(p, false);
  }

  private void apply(GamePiece p, boolean visitingDeck) {

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
      command.append(p.keyEvent(stroke));
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
