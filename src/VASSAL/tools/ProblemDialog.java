package VASSAL.tools;

import java.awt.Component;
import java.awt.Frame;

import VASSAL.build.GameModule;
import VASSAL.i18n.Resources;
import VASSAL.tools.logging.Logger;
import VASSAL.tools.swing.Dialogs;
import VASSAL.tools.swing.DetailsDialog;

public class ProblemDialog {
  private ProblemDialog() {}

  public static void show(
    int messageType,
    String messageKey,
    Object... args)
  {
    show(messageType, getFrame(), null, messageKey, args);
  }

  public static void show(
    int messageType,
    Component parent,
    String messageKey,
    Object... args)
  {
    show(messageType, parent, null, messageKey, args);
  }

  public static void show(
    int messageType,
    Throwable thrown,
    String messageKey,
    Object... args)
  {
    show(messageType, getFrame(), thrown, messageKey, args);
  }

  public static void show(
    int messageType,
    Component parent,
    Throwable thrown,
    String messageKey,
    Object... args)
  {
    show(messageType, parent, thrown,
         Resources.getString(messageKey + "_title"),
         Resources.getString(messageKey + "_heading"),
         Resources.getString(messageKey + "_message", args));
  }

  public static void show(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final String title,
    final String heading,
    final String message)
  {
    if (thrown != null) Logger.log(thrown);

    DialogUtils.enqueue(new Runnable() {
      public void run() {
        Dialogs.showMessageDialog(
          parent,
          title,
          heading,
          message,
          messageType, 
          null
        );
      }
    });
  } 
 
  public static void showDisableable(
    int messageType,
    Object key,
    String messageKey,
    Object... args)
  {
    showDisableable(messageType, getFrame(), null, key, messageKey, args);
  }

  public static void showDisableable(
    int messageType,
    Component parent,
    Object key,
    String messageKey,
    Object... args)
  {
    showDisableable(messageType, parent, null, key, messageKey, args);
  }
  
  public static void showDisableable(
    int messageType,
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
    showDisableable(messageType, getFrame(), thrown, key, messageKey, args);
  }

  public static void showDisableable(
    int messageType,
    Component parent,
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
    showDisableable(messageType, parent, thrown, key,
      Resources.getString(messageKey + "_title"),
      Resources.getString(messageKey + "_heading"),
      Resources.getString(messageKey + "_message", args));
  }

  public static void showDisableable(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final Object key,
    final String title,
    final String heading,
    final String message)
  {
    if (thrown != null) Logger.log(thrown);

    DialogUtils.enqueue(new Runnable() {
      public void run() {
        Dialogs.showMessageDialog(
          parent,
          title,
          heading,
          message,
          messageType, 
          key 
        );
      }
    });
  }

  public static void showDetails(
    int messageType,
    String details,
    String messageKey,
    Object... args)
  {
    showDetails(messageType, getFrame(), null, details, messageKey, args);
  }

  public static void showDetails(
    int messageType,
    Component parent,
    String details,
    String messageKey,
    Object... args)
  {
    showDetails(messageType, parent, null, details, messageKey, args);
  }
  
  public static void showDetails(
    int messageType,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    showDetails(messageType, getFrame(), thrown, details, messageKey, args);
  }

  public static void showDetails(
    int messageType,
    Component parent,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    showDetails(messageType, parent, thrown, details,
      Resources.getString(messageKey + "_title"),
      Resources.getString(messageKey + "_heading"),
      Resources.getString(messageKey + "_message", args));
  }

  public static void showDetails(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final String details,
    final String title,
    final String heading,
    final String message)
  {
    if (thrown != null) Logger.log(thrown);

    DialogUtils.enqueue(new Runnable() {
      public void run() {
        DetailsDialog.showDialog(
          parent,
          title,
          heading,
          message,
          details,
          messageType, 
          null 
        );
      }
    });
  }

  public static void showDetailsDisableable(
    int messageType,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    showDetailsDisableable(messageType, getFrame(), null,
                           details, key, messageKey, args);
  }

  public static void showDetailsDisableable(
    int messageType,
    Component parent,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    showDetailsDisableable(messageType, parent, null,
                           details, key, messageKey, args);
  }
  
  public static void showDetailsDisableable(
    int messageType,
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    showDetailsDisableable(messageType, getFrame(), thrown,
                           details, key, messageKey, args);
  }

  public static void showDetailsDisableable(
    int messageType,
    Component parent,
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    showDetailsDisableable(messageType, parent, thrown, details, key,
      Resources.getString(messageKey + "_title"),
      Resources.getString(messageKey + "_heading"),
      Resources.getString(messageKey + "_message", args));
  }

  public static void showDetailsDisableable(
    final int messageType,
    final Component parent,
    final Throwable thrown,
    final String details,
    final Object key,
    final String title,
    final String heading,
    final String message)
  {
    if (thrown != null) Logger.log(thrown);

    DialogUtils.enqueue(new Runnable() {
      public void run() {
        DetailsDialog.showDialog(
          parent,
          title,
          heading,
          message,
          details,
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
