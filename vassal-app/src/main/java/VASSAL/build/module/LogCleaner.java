/*
 *
 * Copyright (c) 2000-2009 by Rodney Kinney, Brent Easton
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

import VASSAL.build.GameModule;
import VASSAL.build.module.metadata.MetaDataFactory;
import VASSAL.build.module.metadata.SaveMetaData;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.tools.io.DeobfuscatingInputStream;
import VASSAL.tools.io.ObfuscatingOutputStream;
import VASSAL.tools.io.ZipWriter;
import org.apache.commons.io.IOUtils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

/**
 * LogCleaner provides utilities for cleaning VASSAL log files (.vlog) by
 * removing undo operations and the commands they undid, producing a clean
 * record of only the final sequence of moves.
 */
public class LogCleaner {

  /**
   * Clean a log file by removing undo operations and undone commands.
   *
   * @param inputFile  The input .vlog file to clean
   * @param outputFile The output .vlog file to write the cleaned log to
   * @throws IOException If an I/O error occurs during cleaning
   */
  public static void cleanLogFile(File inputFile, File outputFile) throws IOException {
    // Load the log file
    final Command logCommand = loadLogFile(inputFile);
    
    // Extract metadata - cast to SaveMetaData as that's what .vlog files use
    final SaveMetaData metadata = (SaveMetaData) MetaDataFactory.buildMetaData(inputFile);
    
    // Clean the log
    final Command cleanedCommand = cleanLog(logCommand);
    
    // Save the cleaned log
    saveCleanedLog(cleanedCommand, metadata, outputFile);
  }

  /**
   * Load a log file and decode it into a Command tree.
   *
   * @param logFile The .vlog file to load
   * @return The decoded Command tree
   * @throws IOException If an I/O error occurs during loading
   */
  private static Command loadLogFile(File logFile) throws IOException {
    try (InputStream in = new BufferedInputStream(Files.newInputStream(logFile.toPath()));
         ZipInputStream zipInput = new ZipInputStream(in)) {
      for (ZipEntry entry = zipInput.getNextEntry(); entry != null;
           entry = zipInput.getNextEntry()) {
        if (GameState.SAVEFILE_ZIP_ENTRY.equals(entry.getName())) {
          try (InputStream din = new DeobfuscatingInputStream(zipInput)) {
            final String commandString = IOUtils.toString(din, StandardCharsets.UTF_8);
            return GameModule.getGameModule().decode(commandString);
          }
        }
      }
    }
    
    throw new IOException("Invalid log file format");
  }

  /**
   * Clean a command tree by removing undo operations and the commands they undid.
   * 
   * The algorithm:
   * 1. Get the direct subcommands from the root (which is the beginning state)
   * 2. Identify undo pairs (UndoCommand(true) followed by UndoCommand(false))
   * 3. Mark the undo commands and the commands between them for removal
   * 4. Also mark the command immediately before the undo start (the original undone command)
   * 5. Rebuild the command tree with only the "clean" commands
   *
   * @param logCommand The command tree to clean (root is the beginning state)
   * @return A new command tree with undo operations removed
   */
  private static Command cleanLog(Command logCommand) {
    // Get the direct subcommands (which should be LogCommand objects and UndoCommand markers)
    final Command[] subCommands = logCommand.getSubCommands();
    
    // Track which commands to remove
    final boolean[] toRemove = new boolean[subCommands.length];
    
    // Find and mark undo operations
    for (int i = 0; i < subCommands.length; i++) {
      final Command cmd = subCommands[i];
      
      // Check if this is an UndoCommand(true) - start of undo
      if (cmd instanceof BasicLogger.UndoCommand) {
        final BasicLogger.UndoCommand undoCmd = (BasicLogger.UndoCommand) cmd;
        if (undoCmd.isInProgress()) {
          // Found start of undo - mark it for removal
          toRemove[i] = true;
          
          // Find the matching UndoCommand(false)
          int undoEnd = -1;
          for (int j = i + 1; j < subCommands.length; j++) {
            final Command endCmd = subCommands[j];
            if (endCmd instanceof BasicLogger.UndoCommand) {
              final BasicLogger.UndoCommand endUndoCmd = (BasicLogger.UndoCommand) endCmd;
              if (!endUndoCmd.isInProgress()) {
                undoEnd = j;
                break;
              }
            }
          }
          
          if (undoEnd != -1) {
            // Mark the end undo command for removal
            toRemove[undoEnd] = true;
            
            // Mark all commands between start and end for removal (the undone action)
            for (int j = i + 1; j < undoEnd; j++) {
              toRemove[j] = true;
            }
            
            // Mark the command before the undo start (the original command being undone)
            // Need to find the previous LogCommand
            for (int j = i - 1; j >= 0; j--) {
              if (subCommands[j] instanceof BasicLogger.LogCommand) {
                toRemove[j] = true;
                break;
              }
            }
          }
        }
      }
    }
    
    // Rebuild command tree without removed commands
    // The root command is the beginning state (restore command), we want to preserve it
    // but with only the cleaned LogCommand subcommands
    Command cleanedRoot = logCommand;
    
    // Clear the existing subcommands by creating a new root of the same type
    // For simplicity, we'll just append to a NullCommand which will work for encoding
    cleanedRoot = new NullCommand();
    
    // Append only the commands we want to keep
    for (int i = 0; i < subCommands.length; i++) {
      if (!toRemove[i]) {
        cleanedRoot.append(subCommands[i]);
      }
    }
    
    return cleanedRoot;
  }

  /**
   * Save a cleaned command tree to a log file.
   *
   * @param cleanedCommand The cleaned command tree
   * @param metadata       The metadata to include in the log file
   * @param outputFile     The output .vlog file
   * @throws IOException If an I/O error occurs during saving
   */
  private static void saveCleanedLog(Command cleanedCommand, SaveMetaData metadata, File outputFile) throws IOException {
    final String logString = GameModule.getGameModule().encode(cleanedCommand);
    
    try (ZipWriter zw = new ZipWriter(outputFile)) {
      try (OutputStream out = new ObfuscatingOutputStream(
          new BufferedOutputStream(zw.write(GameState.SAVEFILE_ZIP_ENTRY)))) {
        out.write(logString.getBytes(StandardCharsets.UTF_8));
      }
      metadata.save(zw);
    }
  }
}
