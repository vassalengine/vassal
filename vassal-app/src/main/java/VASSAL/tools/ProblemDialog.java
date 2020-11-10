/*
 *
 * Copyright (c) 2008 by Joel Uckelman
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

import java.awt.Component;
import java.awt.Frame;
import java.nio.file.FileSystemException;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Future;

import java.util.stream.Collectors;
import javax.swing.JOptionPane;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.swing.DetailsDialog;
import VASSAL.tools.swing.Dialogs;

/**
 * @author Joel Uckelman
 * @since 3.1.0
 */
public class ProblemDialog {
  private ProblemDialog() {
  }

  private static final Logger logger =
    LoggerFactory.getLogger(ProblemDialog.class);

  public static Future<?> show(
    int messageType,
    String messageKey,
    Object... args) {

    return show(messageType, getFrame(), null, messageKey, args);
  }

  public static Future<?> show(
    int messageType,
    Component parent,
    String messageKey,
    Object... args) {

    return show(messageType, parent, null, messageKey, args);
  }

  public static Future<?> show(
    int messageType,
    Throwable thrown,
    String messageKey,
    Object... args) {

    return show(messageType, getFrame(), thrown, messageKey, args);
  }

  public static Future<?> show(
    int messageType,
    Component parent,
    Throwable thrown,
    String messageKey,
    Object... args) {

    return show(
      messageType, parent, thrown,
      Resources.getString(messageKey + "_title"), // NON-NLS
      Resources.getString(messageKey + "_heading"), // NON-NLS
      Resources.getString(messageKey + "_message", args) // NON-NLS
    );
  }

  public static Future<?> show(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final String title,
    final String heading,
    final String message) {

    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(() -> Dialogs.showMessageDialog(
      parent,
      title,
      heading,
      message,
      messageType
    ));
  }

  public static Future<?> showDisableable(
    int messageType,
    Object key,
    String messageKey,
    Object... args) {

    return showDisableable(
      messageType, getFrame(), null, key, messageKey, args
    );
  }

  public static Future<?> showDisableable(
    int messageType,
    Component parent,
    Object key,
    String messageKey,
    Object... args) {

    return showDisableable(messageType, parent, null, key, messageKey, args);
  }

  public static Future<?> showDisableable(
    int messageType,
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args) {

    return showDisableable(
      messageType, getFrame(), thrown, key, messageKey, args
    );
  }

  public static Future<?> showDisableable(
    int messageType,
    Component parent,
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args) {

    return showDisableable(
      messageType, parent, thrown, key,
      Resources.getString(messageKey + "_title"), // NON-NLS
      Resources.getString(messageKey + "_heading"), // NON-NLS
      Resources.getString(messageKey + "_message", args) // NON-NLS
    );
  }

  public static Future<?> showDisableable(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final Object key,
    final String title,
    final String heading,
    final String message) {

    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(() -> Dialogs.showMessageDialog(
      parent,
      title,
      heading,
      message,
      messageType,
      key,
      Resources.getString("Dialogs.disable")
    ));
  }

  public static Future<?> showDisableableNoI18N(
    final int messageType,
    final Throwable thrown,
    final Object key,
    final String title,
    final String heading,
    final String message) {

    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(() -> Dialogs.showMessageDialog(
      getFrame(),
      title,
      heading,
      message,
      messageType,
      key,
      Resources.getString("Dialogs.dont_show_again")
    ));
  }

  public static Future<?> showDetails(
    int messageType,
    String details,
    String messageKey,
    Object... args) {

    return showDetails(
      messageType, getFrame(), null, details, messageKey, args
    );
  }

  public static Future<?> showDetails(
    int messageType,
    Component parent,
    String details,
    String messageKey,
    Object... args) {

    return showDetails(messageType, parent, null, details, messageKey, args);
  }

  public static Future<?> showDetails(
    int messageType,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args) {

    return showDetails(
      messageType, getFrame(), thrown, details, messageKey, args
    );
  }

  public static Future<?> showFileOverwriteFailure(
    int messageType,
    FileSystemException thrown,
    String details,
    String messageKey,
    Object... args) {

    return showFileOverwriteFailure(
      messageType, getFrame(), thrown, details, messageKey, args
    );
  }

  public static Future<?> showDetails(
    int messageType,
    Component parent,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args) {

    return showDetails(
      messageType, parent, thrown, details,
      Resources.getString(messageKey + "_title"), // NON-NLS
      Resources.getString(messageKey + "_heading"), // NON-NLS
      Resources.getString(messageKey + "_message", args) // NON-NLS
    );
  }

  public static Future<?> showFileOverwriteFailure(
    int messageType,
    Component parent,
    FileSystemException thrown,
    String details,
    String messageKey,
    Object... args) {

    return showDetails(
      messageType, parent, thrown, details,
      Resources.getString(messageKey + "_title", thrown.getFile(), thrown.getOtherFile()), // NON-NLS
      Resources.getString(messageKey + "_heading", thrown.getFile(), thrown.getOtherFile()), // NON-NLS
      Resources.getString(messageKey + "_message", thrown.getFile(), thrown.getOtherFile(), args) // NON-NLS
    );
  }

  public static Future<?> showDetails(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final String details,
    final String title,
    final String heading,
    final String message) {

    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(() -> DetailsDialog.showDialog(
      parent,
      title,
      heading,
      message,
      details,
      Resources.getString("Dialogs.disable"),
      Resources.getString("Dialogs.show_details"),
      Resources.getString("Dialogs.hide_details"),
      messageType,
      null
    ));
  }

  public static Future<?> showDetailsDisableable(
    int messageType,
    String details,
    Object key,
    String messageKey,
    Object... args) {

    return showDetailsDisableable(
      messageType, getFrame(), null, details, key, messageKey, args
    );
  }

  public static Future<?> showDetailsDisableable(
    int messageType,
    Component parent,
    String details,
    Object key,
    String messageKey,
    Object... args) {

    return showDetailsDisableable(
      messageType, parent, null, details, key, messageKey, args
    );
  }

  public static Future<?> showDetailsDisableable(
    int messageType,
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args) {

    return showDetailsDisableable(
      messageType, getFrame(), thrown, details, key, messageKey, args
    );
  }

  public static Future<?> showDetailsDisableable(
    int messageType,
    Component parent,
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args) {

    return showDetailsDisableable(
      messageType, parent, thrown, details, key,
      Resources.getString(messageKey + "_title"), // NON-NLS
      Resources.getString(messageKey + "_heading"), // NON-NLS
      Resources.getString(messageKey + "_message", args) // NON-NLS
    );
  }

  public static Future<?> showDetailsDisableable(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final String details,
    final Object key,
    final String title,
    final String heading,
    final String message) {

    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(() -> DetailsDialog.showDialog(
      parent,
      title,
      heading,
      message,
      details,
      Resources.getString("Dialogs.disable"),
      Resources.getString("Dialogs.show_details"),
      Resources.getString("Dialogs.hide_details"),
      messageType,
      key
    ));
  }

  private static Frame getFrame() {
    return GameModule.getGameModule() == null
      ? null : GameModule.getGameModule().getPlayerWindow();
  }

  private static final List<String> deprecatedMethods = new ArrayList<>();
  /**
   * Display a warning about the use of deprecated code in custom modules
   *
   * showDeprecated is called once for each use of a Deprecated Vassal method that has been
   * marked for removal by the VASSAL development team.
   *
   * We handle the warnings as follows:
   * 1. The Player will only see the first Deprecated warning message. It is displayed in the Chatter
   *    and does not contain details, it refers to the Error log for more detail
   * 2. The first warning message for EACH deprecated method is logged in error log with full details.
   * 3. After the initial warning period (6 months after deprecation), the initial user message will
   *    be displayed as a Disable-able dialog instead of in the Chatter.
   *
   * @param date YYYYMMDD date the method was deprecated
   * @return A Future to allow closing of the Dialog box to be tracked.
   */
  public static Future<?> showDeprecated(String date) {

    // Grab the Stack Frames above the one that called us
    final List<StackWalker.StackFrame> frames =
      StackWalker.getInstance(Set.of(StackWalker.Option.RETAIN_CLASS_REFERENCE), 3)
        .walk(f -> f.skip(1).limit(2).collect(Collectors.toList()));

    final String method = frames.get(0).getClassName() + "." + frames.get(0).getMethodName();

    // Ignore all but the first instance of each method
    if (deprecatedMethods.contains(method)) {
      return null;
    }
    deprecatedMethods.add(method);

    final String callingMethod = frames.get(1).getClassName() + "." + frames.get(1).getMethodName();
    final LocalDate deprecateFrom = LocalDate.parse(date, DateTimeFormatter.ofPattern("uuuu-M-d")); // NON-NLS
    final LocalDate warnUntil = deprecateFrom.plusMonths(9);
    final String expiry = deprecateFrom.plusYears(1).format(DateTimeFormatter.ofPattern("d-MMM-uuuu", Resources.getLocale())); // NON-NLS
    final String expiryDetails = Resources.getString("Dialogs.deprecated.message",  expiry);

    // Log the developer details to the Error log
    logger.warn(Resources.getString("Dialogs.deprecated.detailed_message", method, callingMethod, expiry));

    // Warn the user with a simplified message on the very first deprecated message only
    if (deprecatedMethods.size() == 1) {
      if (LocalDate.now().isBefore(warnUntil)) {
        GameModule.getGameModule().warn("[" + Resources.getString("Dialogs.out_of_date") + "] " + expiryDetails);
        GameModule.getGameModule().warn("[" + Resources.getString("Dialogs.out_of_date") + "] " + Resources.getString("Dialogs.check_for_updated_module"));
        DialogUtils.setDisabled(expiryDetails, true);
        return null;
      }
      else {
        return showDisableable(JOptionPane.WARNING_MESSAGE,
          null, null, method,
          Resources.getString("Dialogs.out_of_date"),
          Resources.getString("Dialogs.out_of_date"),
          expiryDetails + "\n\n" + Resources.getString("Dialogs.check_for_updated_module")
        );
      }
    }
    return null;
  }

  public static Future<?> showOutdatedUsage(String usage) {

    return showDisableable(JOptionPane.WARNING_MESSAGE,
      null, null, usage,
      Resources.getString("Dialogs.out_of_date"),
      Resources.getString("Dialogs.out_of_date"),
      Resources.getString("Dialogs.out_dated_usage", usage) + "\n\n"
        + Resources.getString("Dialogs.check_for_updated_module")
    );
  }


  /**
   * Used when SequenceEncoder#next_token runs out of data when it was expecting more.
   * @param usage - information about what was being sequenced
   * @return Future<?> - Call the get() method of the return value to wait for dialog to close.
   */
  public static Future<?> showOutdatedModule(String usage) {
    return showDisableable(JOptionPane.WARNING_MESSAGE,
      null, null, usage,
      Resources.getString("Dialogs.module_version_incompatible"),
      Resources.getString("Dialogs.module_version_incompatible"),
      Resources.getString("Dialogs.module_version_incompatible_sequence", usage) + "\n\n"
        + Resources.getString("Dialogs.check_for_updated_vmod")
    );
  }
}
