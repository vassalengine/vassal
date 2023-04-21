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

import VASSAL.counters.Attachment;
import VASSAL.counters.Decorator;
import VASSAL.counters.GamePiece;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class AttachmentManager {

  /**
   * Map of the attachment traits with the same attachment names
   */
  final HashMap<String, List<Attachment>> attachments = new HashMap<>();

  public AttachmentManager() {

  }

  public void clearAll() {
    attachments.clear();
  }

  /**
   * A piece has been added to the game state. Process any auto-attachments it has defined
   * @param p
   */
  public void pieceAdded(GamePiece p) {
    if (!(p instanceof Decorator)) {
      return;
    }
    for (GamePiece piece : Decorator.getDecorators(p, Attachment.class)) {
      final Attachment attachment = (Attachment) piece;
      // if (attachment.isAutoAttach()) {
      final String attachId = attachment.getAttachName();
      List<Attachment> currentAttachments = attachments.get(attachId);
      if (currentAttachments == null) {
        currentAttachments = new ArrayList<>();
      }
      for (Attachment target : currentAttachments) {
        // TODO Do attachy stuff betweem attachment and target
      }
      currentAttachments.add(attachment);
      attachments.put(attachId, currentAttachments);
      // }
    }
  }

  /**
   * A piece has been removed from the game state. Remove any auto-attachments it has defined
   * @param p
   */
  public void pieceRemoved(GamePiece p) {
    if (!(p instanceof Decorator)) {
      return;
    }
    for (GamePiece piece : Decorator.getDecorators(p, Attachment.class)) {
      final Attachment attachment = (Attachment) piece;
      // if (attachment.isAutoAttach()) {
      final String attachId = attachment.getAttachName();

      // Remove any attachments
      // TODO - Add code to detach this piece from all parents

      // Clean the attachments array
      final List<Attachment> currentAttachments = attachments.get(attachId);
      if (currentAttachments != null) {
        currentAttachments.remove(attachment);
      }
      attachments.put(attachId, currentAttachments);
      // }
    }
  }
}
