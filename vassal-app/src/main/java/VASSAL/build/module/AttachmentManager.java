/*
 * Copyright (c) 2023 by The VASSAL Development Team
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
package VASSAL.build.module;

import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.counters.Attachment;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Maintains a list of pieces with attachment traits
 */
public class AttachmentManager {

  /**
   * Map of the attachment traits with the same attachment names
   */
  private final Map<String, List<Attachment>> attachments = new HashMap<>();
  private final List<GamePiece> newPiecesWithAttachments = new ArrayList<>();

  public void clearAll() {
    attachments.clear();
  }

  public void clearNew() {
    newPiecesWithAttachments.clear();
  }

  /**
   * Check our list of new pieces created w/ attachments, and for any that are "auto attach" go ahead
   * and attach them to all of their siblings.
   *
   * @return a command to reproduce the action on other instances
   */
  public Command doAutoAttachments() {
    Command c = new NullCommand();

    for (final GamePiece p : newPiecesWithAttachments) {
      final List<GamePiece> myAttachments = Decorator.getDecorators(p, Attachment.class);
      for (final GamePiece a : myAttachments) {
        final Attachment attach = (Attachment)a;
        if (!attach.isAutoAttach()) continue;
        if (attach.getMap() == null) continue;

        final String attachName = attach.getAttachName();
        final List<Attachment> siblings = attachments.get(attachName);
        if (siblings == null) continue;

        for (final Attachment sibling : siblings) {
          final GamePiece outer = Decorator.getOutermost(sibling);
          if (p.equals(outer) && !attach.isAllowSelfAttach()) continue;
          if (outer.getMap() == null) continue;

          c = c.append(attach.makeAddTargetCommand(outer));
        }
      }
    }

    newPiecesWithAttachments.clear();

    return c;
  }

  /**
   * A piece w/ attachment traits has been added to the game state. Make note of its attachment traits, and
   * add the piece to the "new pieces" list for later.
   * @param p
   */
  public void pieceAdded(GamePiece p) {
    if (!(p instanceof Decorator)) {
      return;
    }

    final List<GamePiece> myAttachments = Decorator.getDecorators(p, Attachment.class);
    if (!myAttachments.isEmpty()) {
      newPiecesWithAttachments.add(p);
    }

    for (final GamePiece piece : myAttachments) {
      final Attachment attachment = (Attachment) piece;

      final String attachName = attachment.getAttachName();
      List<Attachment> currentAttachments = attachments.get(attachName);
      if (currentAttachments == null) {
        currentAttachments = new ArrayList<>();
      }
      currentAttachments.add(attachment);
      attachments.put(attachName, currentAttachments);
    }
  }

  /**
   * A piece with attachment traits has been removed from the game state. Clean up the attachments index.
   * @param p
   */
  public void pieceRemoved(GamePiece p) {
    if (!(p instanceof Decorator)) {
      return;
    }
    for (final GamePiece piece : Decorator.getDecorators(p, Attachment.class)) {
      final Attachment attachment = (Attachment) piece;
      // if (attachment.isAutoAttach()) {
      final String attachId = attachment.getAttachName();

      // Clean the attachments array
      final List<Attachment> currentAttachments = attachments.get(attachId);
      if (currentAttachments != null) {
        currentAttachments.remove(attachment);
      }
      attachments.put(attachId, currentAttachments);
    }
  }
}
