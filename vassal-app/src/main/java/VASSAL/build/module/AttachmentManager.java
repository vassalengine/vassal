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
import VASSAL.counters.Stack;
import VASSAL.tools.lang.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Maintains a list of pieces with attachment traits
 */
public class AttachmentManager {

  /**
   * A Map of the set of Attachment traits that are using the same Attachment name
   */
  private final java.util.Map<String, Set<Attachment>> attachments;

  /**
   * A List of attachments that could not be finalised during game load due to pieces not being loaded yet
   */
  private final List<Pair<Attachment, String>> pendingAttachments;

  public AttachmentManager() {
    attachments = new HashMap<>();
    pendingAttachments = new ArrayList<>();
  }

  /**
   * New game starting, clean all attachment info
   */
  public void clearAll() {
    attachments.clear();
    pendingAttachments.clear();
  }

  public void pieceAdded(GamePiece piece) {
    if (piece instanceof Stack) {
      for (final GamePiece p : ((Stack) piece).asList()) {
        addPiece(p);
      }
    }
    else {
      addPiece(piece);
    }
  }

  /**
   * A piece has been added to the game.
   * 1. If it has as Auto-attach, attach all other Attachments with same name to it.
   * 2. Tell all other Auto-attach traits with same name to attach to it.
   * <p>
   * Create the self attachment if required
   * @param piece Piece moved or added
   */
  public void addPiece(GamePiece piece) {
    // Check each Attachment trait in the piece
    for (final GamePiece gp : Decorator.getDecorators(piece, Attachment.class)) {
      final Attachment attach = (Attachment) gp;
      final String attachName = attach.getAttachName();

      // Find all Attachment traits (not pieces) that use that attachment name and loop through them
      final Set<Attachment> currentAttachments = attachments.computeIfAbsent(attachName, k -> new HashSet<>());

      if (attach.isAllowSelfAttach()) {
        // Self attach
        attach.autoAttach(attach);
      }

      for (final Attachment targetAttachment : currentAttachments) {
        // Auto-attach the traits to each other. The traits will do nothing if they are not auto-attach, and will prevent double attaches.
        if (!targetAttachment.equals(attach)) {
          attach.autoAttach(targetAttachment);
          targetAttachment.autoAttach(attach);
        }
      }

      // Add this trait to the current Set
      currentAttachments.add(attach);
      attachments.put(attachName, currentAttachments);
    }
  }

  public void pieceRemoved(GamePiece piece) {
    // Ignore Stacks, Stacks can be removed from the GameState leaving their pieces behind.
    // So do NOT clean up the attachments of the Stack contents. Wait until pieceRemoved is explicitly called with the Stacks children
    if (!(piece instanceof Stack)) {
      removePiece(piece);
    }
  }

  /**
   * A piece has been removed from a Map. If it contains any Attachments, tell all other
   * auto-attachments to that Attachment to forget this piece
   */
  public void removePiece(GamePiece piece) {
    for (final GamePiece gp : Decorator.getDecorators(piece, Attachment.class)) {

      final Attachment attach = (Attachment) gp;
      final String attachName = attach.getAttachName();

      // Find all attachments using this Attach name and process each one
      final Set<Attachment> currentAttachments = attachments.computeIfAbsent(attachName, k -> new HashSet<>());

      for (final Attachment attachment : currentAttachments) {
        if (attachment.isAutoAttach()) {
          attachment.removeTarget(piece);
        }
      }
      currentAttachments.remove(attach);
      attachments.put(attachName, currentAttachments);
      attach.clearAll();
    }
  }

  /**
   * A piece is being removed by a Delete or Replace Command.
   * Undo any attachments to or from any non-auto-attach Attachment traits it has
   * Auto-attach traits will be cleaned up locally by the client when the piece is removed from the GameState
   *
   * @param   piece Piece to clean up Attachments for.
   * @return        Command to undo attachments.
   */
  public Command removeAttachments(GamePiece piece) {
    Command command = new NullCommand();

    for (final GamePiece gp : Decorator.getDecorators(piece, Attachment.class)) {

      final Attachment attach = (Attachment) gp;
      if (!attach.isAutoAttach()) {

        final String attachName = attach.getAttachName();

        // Find all attachments using this Attach name and process each one
        final Set<Attachment> currentAttachments = attachments.computeIfAbsent(attachName, k -> new HashSet<>());

        for (final Attachment targetAttachment : currentAttachments) {
          // If the target attachment is not auto-attachable, remove any attachment back to us
          if (!targetAttachment.isAutoAttach()) {
            command = command.append(targetAttachment.makeRemoveTargetCommand(piece));
          }
          // Remove any attachment we have to the target piece.
          command = command.append(attach.makeRemoveTargetCommand(Decorator.getOutermost(targetAttachment)));
        }

        // Remove the Attachment trait being processed from the index
        currentAttachments.remove(attach);
        attachments.put(attachName, currentAttachments);
      }

    }

    return command;
  }

  public Set<String> getAttachmentList() {
    return attachments.keySet();
  }

  public Set<Attachment> getAttachmentList(String attachName) {
    return attachments.get(attachName);
  }

  /**
   * Record an Attachment that must be resolved later, the target piece has not been loaded yet.
   * @param attachnent  Attachment Trait
   * @param target      Id of target piece
   */
  public void addPendingAttachment(Attachment attachnent, String target) {
    pendingAttachments.add(Pair.of(attachnent, target));
  }

  /**
   * Callback from AttachmentManager aftet end of game load to add Attachments to pieces
   * that had not yet been loaded.
   */
  public void resolvePendingAttachments() {
    for (final Pair<Attachment, String> p : pendingAttachments) {
      p.first.resolvePendingAtttachment(p.second);
    }
    pendingAttachments.clear();
  }
}
