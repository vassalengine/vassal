/*
 *
 * Copyright (c) 2020 by Brian Reynolds
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License (LGPL) as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, copies are available
 * at http://www.opensource.org.
 */
package VASSAL.tools;

import org.apache.commons.lang3.StringUtils;

/**
 * Allows certain special characters at the beginning of a string to control a color styling, as an
 * easier shortcut than putting style="color:#00ffff;" tags in every message.
 */
public class QuickColors {
  public static final String QUICK_COLOR_CODES = "|!?~`"; // Our quick color code characters
  public static final int QUICK_COLOR_NONE = -1; // If no code

  public static final String[] QUICK_COLOR_REGEX = { "\\|", "!", "\\?", "~", "`" }; // Regex searchers for each of our codes

  /**
   * Checks if a character is a Quick Color code
   * @param c Character to check if it's a Quick Color code
   * @return Index of Quick Color code, or -1 if none
   */
  public static int getQuickColor(char c) {
    return QUICK_COLOR_CODES.indexOf(c);
  }

  /**
   * Checks if the string begins with a Quick Color code
   * @param s String to check first non-space character of
   * @return Index of Quick Color code, or -1 if none
   */
  public static int getQuickColor(String s) {
    if (StringUtils.isEmpty(s)) return QUICK_COLOR_NONE;
    final String s2 = s.trim();
    if (s2.isEmpty()) return QUICK_COLOR_NONE;
    return getQuickColor(s2.charAt(0));
  }

  /**
   * Checks if the string begins with a Quick Color code (possibly after a prefix)
   * @param s String to check first non-space-character-after-the-prefix
   * @param prefix Prefix to ignore (e.g. "*" at the beginning of Game Messages sent to the Chatter)
   * @return Index of Quick Color code, or -1 if none
   */
  public static int getQuickColor(String s, String prefix) {
    if (StringUtils.isEmpty(s) || s.isBlank()) return QUICK_COLOR_NONE;
    if (StringUtils.isEmpty(prefix)) {
      return getQuickColor(s);
    }
    if (!s.startsWith(prefix)) return QUICK_COLOR_NONE;
    final String s2 = s.substring(prefix.length()).trim();
    if (s2.isEmpty()) return QUICK_COLOR_NONE;
    return getQuickColor(s2.charAt(0));
  }

  /**
   * Strips the Quick Color tag from a string if any, readying it for display
   * @param s String to strip Quick Color tag from
   * @return the string minus the Quick Color tag, if one was present
   */
  public static String stripQuickColorTag(String s) {
    return stripQuickColorTag(s, "");
  }

  /**
   * Strips the Quick Color tag from a string if any, readying it for display
   * @param s String to strip Quick Color tag from
   * @param prefix Prefix to ignore (e.g. "*" at the beginning of Game Messages sent to the Chatter)
   * @return the string minus the Quick Color tag, if one was present
   */
  public static String stripQuickColorTag(String s, String prefix) {
    final int quickIndex = getQuickColor(s, prefix);
    if (quickIndex < 0) return s;
    return s.replaceFirst(QUICK_COLOR_REGEX[quickIndex], "");
  }

  /**
   * Returns the proper CSS class name to be used to display the string, based on Quick Color (or lack thereof)
   * @param s String to be checked
   * @return CSS class name to be used to display it
   */
  public static String getQuickColorHTMLStyle(String s) {
    return getQuickColorHTMLStyle(s, "");
  }

  /**
   * Returns the proper CSS class name to be used to display the string, based on Quick Color (or lack thereof)
   * @param s String to be checked
   * @param prefix Prefix to ignore (e.g. "*" at the beginning of Game Messages sent to the Chatter)
   * @return CSS class name to be used to display it
   */
  public static String getQuickColorHTMLStyle(String s, String prefix) {
    final int quickIndex = getQuickColor(s, prefix);
    return "msg" + ((quickIndex <= 0) ? "" : quickIndex + 1); //NON-NLS
  }
}
