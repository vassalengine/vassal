/*
 *
 * Copyright (c) 2023 by vassalengine.org, Brian Reynolds
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

import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.Command;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.script.expression.Auditable;

import javax.swing.KeyStroke;

/**
 * Variant on GlobalCommand for use with Attachment -- instead of sending a key, finds pieces and attaches them
 */
public class GlobalAttach extends GlobalCommand {
  public Attachment attach;

  public GlobalAttach(Attachment attach) {
    this (attach, null);
  }

  public GlobalAttach(Attachment attach, PropertySource p) {
    super(attach, p);
    this.attach = attach;
  }

  @Override
  public boolean isAbortIfNoCommand() {
    return false; // Unlike a normal GKC, we aren't actually sending a key, so it's okay if the key command doesn't exist
  }

  public GlobalCommandVisitor getVisitor(Command command, PieceFilter filter, KeyStroke keyStroke, AuditTrail audit, Auditable owner, int selectFromDeck) {
    return new AttachVisitor(command, filter, keyStroke, audit, owner, selectFromDeck);
  }

  protected class AttachVisitor extends GlobalCommandVisitor {
    public AttachVisitor(Command command, PieceFilter filter, KeyStroke stroke, AuditTrail audit, Auditable owner, int selectFromDeck) {
      super(command, filter, stroke, audit, owner, selectFromDeck);
    }

    @Override
    protected void apply(GamePiece p, boolean visitingDeck) {

    /*
      If an AuditTrail has been supplied for the evaluation history of the filter up to this point,
      then clone it for applying to each individual piece.
     */
      AuditTrail audit = null;
      if (auditSoFar != null) {
        audit = new AuditTrail(auditSoFar);
        audit.addMessage(Resources.getString("Audit.attach_applied_to", p.getComponentName()));
      }

      if (filter == null || filter.accept(p, owner, audit)) {
        if (visitingDeck) {
          p.setProperty(Properties.OBSCURED_BY, p.getProperty(Properties.OBSCURED_BY_PRE_DRAW));  // Bug 13433 restore correct OBSCURED_BY after checking filter
        }

        command.append(attach.makeAddTargetCommand(p));

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
