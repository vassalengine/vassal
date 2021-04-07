/*
 *
 * Copyright (c) 2020 by Brian Reynolds, VASSAL
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

import VASSAL.Info;
import VASSAL.build.GameModule;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.command.Logger;
import VASSAL.tools.BugUtils;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Iterator;
import java.util.regex.Pattern;

import org.slf4j.LoggerFactory;

/**
 * Expandable "Console" to allow entering commands into the Chatter.
 */
public class Console {
  Iterator<String> tok;
  String commandLine;

  private static final org.slf4j.Logger log =
    LoggerFactory.getLogger(Console.class);

  private void show(String s) {
    GameModule.getGameModule().warn(s);
  }


  private boolean matches(String s1, String s2) {
    return matches(s1, s2, 2);
  }

  private boolean matches(String s1, String s2, int min) {
    if (s2.isEmpty() || (s2.length() > s1.length()) || ((s2.length() < min) && (s1.length() > s2.length()))) {
      return false;
    }
    if (s2.length() < min) {
      return s1.equalsIgnoreCase(s2);
    }
    return s1.substring(0, s2.length()).equalsIgnoreCase(s2);
  }

  private String nextString(String def) {
    return tok.hasNext() ? tok.next() : def;
  }

  private int nextInt(int def) {
    try {
      return tok.hasNext() ? Integer.parseInt(tok.next()) : def;
    }
    catch (NumberFormatException e) {
      return def;
    }
  }

  /**
   * Opens the specified file (or folder) using the Desktop's default method
   * @param file file or folder to open
   */
  private void browseFileOrFolder(File file) {
    final Desktop desktop = Desktop.getDesktop();
    try {
      desktop.open(file);
    }
    catch (IOException e) {
      log.error("File Not Found", e); //NON-NLS
    }
    catch (IllegalArgumentException iae) {
      log.error("Illegal argument", iae); //NON-NLS
    }
  }


  private boolean doErrorLog() {
    final String option = nextString("");
    if (matches("show", option)) { //NON-NLS
      final String errorLog = BugUtils.getErrorLog();
      final String delims2 = "[\n]+";
      final String[] lines = errorLog.split(delims2);

      if (lines.length > 0) {
        final int end = lines.length - 1;
        final int linesToShow = nextInt(0);
        final int start = (linesToShow <= 0) ? 0 : Math.max(0, end - linesToShow + 1);

        for (int line = start; line <= end; line++) {
          show(lines[line]);
        }
      }
    }
    else if (matches("write", option)) { //NON-NLS
      final int where = commandLine.toLowerCase().indexOf("write"); //NON-NLS
      if ((where > 0) && commandLine.length() > where + 6) {
        log.info(commandLine.substring(where + 6));
      }
    }
    else if (matches("folder", option)) { //NON-NLS
      browseFileOrFolder(Info.getConfDir());
    }
    else if (matches("noecho", option)) { //NON-NLS
      GameModule.setErrorLogToChat(false);
      show("Errorlog Echo: OFF"); //NON-NLS
    }
    else if (matches("echo", option)) { //NON-NLS
      GameModule.setErrorLogToChat(true);
      show("Errorlog Echo: ON"); //NON-NLS
    }
    else if (matches("open", option)) { //NON-NLS
      browseFileOrFolder(Info.getErrorLogPath());
    }
    else if (matches("wipe", option)) { //NON-NLS
      final File errorLog = Info.getErrorLogPath();
      try {
        Files.newOutputStream(errorLog.toPath()).close();
        show("Wiped errorlog"); //NON-NLS
      }
      catch (IOException e) {
        show("Failed to wipe errorlog"); //NON-NLS
      }
    }
    else if (matches("?", option) || matches("help", option) || ("".equals(option))) { //NON-NLS
      show("Usage:"); //NON-NLS
      show("  /errorlog echo         - Echoes new errorlog info in chat log"); //NON-NLS
      show("  /errorlog folder       - Opens folder containing errorlog"); //NON-NLS
      show("  /errorlog noecho       - Disables echoing of errorlog info in chat log"); //NON-NLS
      show("  /errorlog open         - Opens errorlog in OS"); //NON-NLS
      show("  /errorlog show [n]     - Show last n lines of errorlog"); //NON-NLS
      show("  /errorlog wipe         - Wipe the errorlog file"); //NON-NLS
      show("  /errorlog write [text] - Write text into the errorlog file"); //NON-NLS
    }
    else {
      show("Unknown command."); //NON-NLS
      show("Use '/errorlog help' for usage info."); //NON-NLS
    }

    return true;
  }


  private boolean doProperty() {
    final String option   = nextString("");
    final String property = nextString("");

    if (matches("?", option) || matches("help", option)) { //NON-NLS
      show("Usage:"); //NON-NLS
      show("  /property show [property]        - show global property value"); //NON-NLS
      show("  /property set [property] [value] - set global property new value"); //NON-NLS
    }
    else if (matches("show", option)) { //NON-NLS
      final MutableProperty.Impl propValue = (MutableProperty.Impl) GameModule.getGameModule().getMutableProperty(property);
      if (propValue != null) {
        show("[" + property + "]: " + propValue.getPropertyValue());
      }
      else {
        final String propVal = (String)GameModule.getGameModule().getProperty(property);
        if (propVal != null) {
          show("[" + property + "]: " + propVal);
        }
      }
    }
    else if (matches("set", option)) { //NON-NLS
      final MutableProperty.Impl propValue = (MutableProperty.Impl) GameModule.getGameModule().getMutableProperty(property);
      if (propValue != null) {
        propValue.setPropertyValue(nextString(""));
        show("[" + property + "]: " + propValue.getPropertyValue());
      }
    }

    return true;
  }


  private boolean doHelp() {
    final String topic = nextString("");

    if (topic.isEmpty()) {
      show("VASSAL console commands:"); //NON-NLS
      show("  /errorlog - commands for opening/clearing/altering errorlog"); //NON-NLS
      show("  /help     - shows list of commands"); //NON-NLS
      show("  /property - commands for reading/writing global properties"); //NON-NLS
    }
    else {
      tok = Pattern.compile(" +").splitAsStream("help").iterator(); //NON-NLS // Fake up a help subcommand
      if (matches("errorlog", topic)) { //NON-NLS
        return doErrorLog();
      }
      else if (matches("property", topic)) { //NON-NLS
        return doProperty();
      }

      show("Unknown help topic"); //NON-NLS
    }

    return true;
  }


  public boolean consoleHook(String s, String commandLine, Iterator<String> tok, String command) {
    // Hook for console subclasses, etc.
    return false;
  }


  public boolean exec(String s, String style, boolean html_allowed) {
    if (s.isEmpty() || (s.charAt(0) != '/')) {
      return false;
    }
    commandLine = s.substring(1);

    // If this has EVER been a multiplayer game (has ever been connected to Server, or has ever had two player slots filled simultaneously), then
    // it will not accept console commands.
    final Logger log = GameModule.getGameModule().getLogger();
    if (log instanceof BasicLogger) {
      if (((BasicLogger)log).isMultiPlayer() || GameModule.getGameModule().isMultiPlayer()) {
        show("|<b>Console commands not allowed in multiplayer games.</b>"); //NON-NLS
        return false;
      }
    }

    show(s);

    // First get rid of any extra spaces between things
    tok = Pattern.compile(" +").splitAsStream(commandLine).iterator();

    final String command = nextString("");

    if (matches("help", command) || matches("?", command)) { //NON-NLS
      return doHelp();
    }

    if (matches("errorlog", command)) { //NON-NLS
      return doErrorLog();
    }

    if (matches("property", command)) { //NON-NLS
      return doProperty();
    }

    if (!consoleHook(s, commandLine, tok, command)) {
      show("Unknown command. Use /help for list of commands."); //NON-NLS
      return false;
    }

    return true;
  }
}
