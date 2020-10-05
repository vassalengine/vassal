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
import VASSAL.tools.SequenceEncoder;
import org.slf4j.LoggerFactory;

import java.awt.Desktop;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * Expandable "Console" to allow entering commands into the Chatter.
 */
public class Console {
  SequenceEncoder.Decoder decode;
  String commandLine;

  private static final org.slf4j.Logger log =
    LoggerFactory.getLogger(Console.class);

  private void show (String s) {
    GameModule.getGameModule().warn(s);
  }


  private Boolean matches (String s1, String s2) {
    return matches(s1, s2, 2);
  }

  private Boolean matches (String s1, String s2, int min) {
    if (s2.isEmpty() || (s2.length() > s1.length()) || ((s2.length() < min) && (s1.length() > s2.length()))) {
      return false;
    }
    if (s2.length() < min) {
      return s1.toLowerCase().equals(s2.toLowerCase());
    }
    return s1.toLowerCase().substring(0, s2.length()).equals(s2.toLowerCase());
  }


  private Boolean doErrorLog () {
    String option = decode.nextToken("");
    if (matches("show", option)) {
      String errorLog = BugUtils.getErrorLog();
      String delims2 = "[\n]+";
      String[] lines = errorLog.split(delims2);

      if (lines.length > 0) {
        int end         = lines.length - 1;
        int linesToShow = decode.nextInt(0);
        int start       = (linesToShow <= 0) ? 0 : Math.max(0, end - linesToShow + 1);

        for (int line = start; line <= end; line++) {
          show(lines[line]);
        }
      }
    }
    else if (matches("write", option)) {
      int where = commandLine.toLowerCase().indexOf("write");
      if ((where > 0) && commandLine.length() > where + 6) {
        log.info(commandLine.substring(where + 6));
      }
    }
    else if (matches("folder", option)) {
      Desktop desktop = Desktop.getDesktop();
      File dirToOpen = null;
      try {
        dirToOpen = Info.getHomeDir();
        desktop.open(dirToOpen);
      }
      catch (IOException e) {
        System.out.println("File Not Found");
      }
      catch (IllegalArgumentException iae) {
        System.out.println("File Not Found");
      }
    }
    else if (matches("open", option)) {
      Desktop desktop = Desktop.getDesktop();
      File dirToOpen = null;
      try {
        dirToOpen = Info.getErrorLogPath();
        desktop.open(dirToOpen);
      }
      catch (IOException e) {
        System.out.println("File Not Found");
      }
      catch (IllegalArgumentException iae) {
        System.out.println("File Not Found");
      }
    }
    else if (matches("wipe", option)) {
      final File errorLog = Info.getErrorLogPath();
      try {
        new FileOutputStream(errorLog).close();
        show("Wiped errorlog");
      }
      catch (IOException e) {
        show("Failed to wipe errorlog");
      }
    }
    else if (matches("?", option) || matches("help", option)) {
      show("Usage:");
      show("  /errorlog folder       - Opens folder containing errorlog");
      show("  /errorlog open         - Opens errorlog in OS");
      show("  /errorlog show [n]     - Show last n lines of errorlog");
      show("  /errorlog wipe         - Wipe the errorlog file");
      show("  /errorlog write [text] - Write text into the errorlog file");
    }

    return true;
  }


  private Boolean doProperty() {
    String option   = decode.nextToken("");
    String property = decode.nextToken("");

    if (matches("?", option) || matches("help", option)) {
      show ("Usage:");
      show ("  /property show [property]        - show global property value");
      show ("  /property set [property] [value] - set global property new value");
    }
    else if (matches("show", option)) {
      MutableProperty.Impl propValue = (MutableProperty.Impl) GameModule.getGameModule().getMutableProperty(property);
      if (propValue != null) {
        show("[" + property + "]: " + propValue.getPropertyValue());
      }
    }
    else if (matches("set", option)) {
      MutableProperty.Impl propValue = (MutableProperty.Impl) GameModule.getGameModule().getMutableProperty(property);
      if (propValue != null) {
        propValue.setPropertyValue(decode.nextToken(""));
        show("[" + property + "]: " + propValue.getPropertyValue());
      }
    }

    return true;
  }


  public Boolean consoleHook(String s, String commandLine, String cl, String command, SequenceEncoder.Decoder decode) {
    // Hook for console subclasses, etc.
    return false;
  }


  public Boolean exec(String s, String style, boolean html_allowed) {
    if (s.charAt(0) != '/') {
      return false;
    }
    commandLine = s.substring(1);

    // If this has EVER been a multiplayer game (has ever been connected to Server, or has ever had two player slots filled simultaneously), then
    // it will not accept console commands.
    Logger log = GameModule.getGameModule().getLogger();
    if (log instanceof BasicLogger) {
      if (((BasicLogger)log).isMultiPlayer() || GameModule.getGameModule().getPlayerRoster().isMultiPlayer()) {
        show ("<b>Console commands not allowed in multiplayer games.</b>");
        return false;
      }
    }

    show(s);

    // First get rid of any extra spaces between things
    String delims = "[ ]+";
    String[] tokens = commandLine.split(delims);
    StringBuilder sb = new StringBuilder("");
    Boolean first = true;
    for (String t : tokens) {
      if (first) {
        first = false;
      }
      else {
        sb.append(" ");
      }
      sb.append(t);
    }
    String cl = sb.toString(); // Our command line with things separated by no more than one space

    decode = new SequenceEncoder.Decoder(cl, ' ');

    String command = decode.nextToken("");

    if (matches("errorlog", command)) {
      return doErrorLog();
    }

    if (matches("property", command)) {
      return doProperty();
    }

    if (!consoleHook(s, commandLine, cl, command, decode)) {
      show ("Unknown command.");
      return false;
    }

    return true;
  }
}
