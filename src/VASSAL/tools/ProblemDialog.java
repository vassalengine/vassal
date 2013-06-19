/*
 * $Id$
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
import java.util.concurrent.Future;

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
  private ProblemDialog() {}

  private static final Logger logger =
    LoggerFactory.getLogger(ProblemDialog.class);

  public static Future<?> show(
    int messageType,
    String messageKey,
    Object... args)
  {
    return show(messageType, getFrame(), null, messageKey, args);
  }

  public static Future<?> show(
    int messageType,
    Component parent,
    String messageKey,
    Object... args)
  {
    return show(messageType, parent, null, messageKey, args);
  }

  public static Future<?> show(
    int messageType,
    Throwable thrown,
    String messageKey,
    Object... args)
  {
    return show(messageType, getFrame(), thrown, messageKey, args);
  }

  public static Future<?> show(
    int messageType,
    Component parent,
    Throwable thrown,
    String messageKey,
    Object... args)
  {
    return show(
      messageType, parent, thrown,
      Resources.getString(messageKey + "_title"),
      Resources.getString(messageKey + "_heading"),
      Resources.getString(messageKey + "_message", args)
    );
  }

  public static Future<?> show(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final String title,
    final String heading,
    final String message)
  {
    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(new Runnable() {
      public void run() {
        Dialogs.showMessageDialog(
          parent,
          title,
          heading,
          message,
          messageType
        );
      }
    });
  }

  public static Future<?> showDisableable(
    int messageType,
    Object key,
    String messageKey,
    Object... args)
  {
    return showDisableable(
      messageType, getFrame(), null, key, messageKey, args
    );
  }

  public static Future<?> showDisableable(
    int messageType,
    Component parent,
    Object key,
    String messageKey,
    Object... args)
  {
    return showDisableable(messageType, parent, null, key, messageKey, args);
  }

  public static Future<?> showDisableable(
    int messageType,
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
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
    Object... args)
  {
    return showDisableable(
      messageType, parent, thrown, key,
      Resources.getString(messageKey + "_title"),
      Resources.getString(messageKey + "_heading"),
      Resources.getString(messageKey + "_message", args)
    );
  }

  public static Future<?> showDisableable(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final Object key,
    final String title,
    final String heading,
    final String message)
  {
    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(new Runnable() {
      public void run() {
        Dialogs.showMessageDialog(
          parent,
          title,
          heading,
          message,
          messageType,
          key,
          Resources.getString("Dialogs.disable")
        );
      }
    });
  }

  public static Future<?> showDisableableNoI18N(
    final int messageType,
    final Throwable thrown,
    final Object key,
    final String title,
    final String heading,
    final String message)
  {
    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(new Runnable() {
      public void run() {
        Dialogs.showMessageDialog(
          getFrame(),
          title,
          heading,
          message,
          messageType,
          key,
          "Don't show this again"
        );
      }
    });
  }

  public static Future<?> showDetails(
    int messageType,
    String details,
    String messageKey,
    Object... args)
  {
    return showDetails(
      messageType, getFrame(), null, details, messageKey, args
    );
  }

  public static Future<?> showDetails(
    int messageType,
    Component parent,
    String details,
    String messageKey,
    Object... args)
  {
    return showDetails(messageType, parent, null, details, messageKey, args);
  }

  public static Future<?> showDetails(
    int messageType,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    return showDetails(
      messageType, getFrame(), thrown, details, messageKey, args
    );
  }

  public static Future<?> showDetails(
    int messageType,
    Component parent,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    return showDetails(
      messageType, parent, thrown, details,
      Resources.getString(messageKey + "_title"),
      Resources.getString(messageKey + "_heading"),
      Resources.getString(messageKey + "_message", args)
    );
  }

  public static Future<?> showDetails(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final String details,
    final String title,
    final String heading,
    final String message)
  {
    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(new Runnable() {
      public void run() {
        DetailsDialog.showDialog(
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
        );
      }
    });
  }

  public static Future<?> showDetailsDisableable(
    int messageType,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
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
    Object... args)
  {
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
    Object... args)
  {
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
    Object... args)
  {
    return showDetailsDisableable(
      messageType, parent, thrown, details, key,
      Resources.getString(messageKey + "_title"),
      Resources.getString(messageKey + "_heading"),
      Resources.getString(messageKey + "_message", args)
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
    final String message)
  {
    if (thrown != null) logger.error("", thrown);

    return DialogUtils.enqueue(new Runnable() {
      public void run() {
        DetailsDialog.showDialog(
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
        );
      }
    });
  }

  private static Frame getFrame() {
    return GameModule.getGameModule() == null
      ? null : GameModule.getGameModule().getFrame();
  }
}
