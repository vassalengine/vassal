package VASSAL.counters;

import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;
import VASSAL.tools.RecursionLimiter;

import javax.swing.KeyStroke;

public class GlobalAttach extends GlobalCommand {
  public GlobalAttach(RecursionLimiter.Loopable l) {
    this (l, null);
  }

  public GlobalAttach(RecursionLimiter.Loopable l, PropertySource p) {
    super(l, p);
  }

  public GlobalCommandVisitor getVisitor(Command command, PieceFilter filter, KeyStroke keyStroke, AuditTrail audit, Auditable owner, int selectFromDeck) {
    return new AttachVisitor(command, filter, keyStroke, audit, owner, selectFromDeck);
  }

  protected class AttachVisitor extends GlobalCommandVisitor {
    public AttachVisitor(Command command, PieceFilter filter, KeyStroke stroke, AuditTrail audit, Auditable owner, int selectFromDeck) {
      super(command, filter, stroke, audit, owner, selectFromDeck);
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

        //command.append(p.keyEvent(stroke));

        selectedCount++;
      }
      else {
        if (visitingDeck) {
          p.setProperty(Properties.OBSCURED_BY, p.getProperty(Properties.OBSCURED_BY_PRE_DRAW));  // Bug 13433 restore correct OBSCURED_BY
        }
      }
    }
  }
}
