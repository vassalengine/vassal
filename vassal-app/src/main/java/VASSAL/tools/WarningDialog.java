
package VASSAL.tools;

import java.awt.Component;
import java.util.concurrent.Future;

import javax.swing.JOptionPane;


public class WarningDialog {
  private WarningDialog() {}

  public static Future<?> show(
    String messageKey,
    Object... args)
  {
    return ProblemDialog.show(JOptionPane.WARNING_MESSAGE, messageKey, args);
  }

  public static Future<?> show(
    Component parent,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.show(
      JOptionPane.WARNING_MESSAGE, parent, messageKey, args
    );
  }

  public static Future<?> show(
    Throwable thrown,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.show(
      JOptionPane.WARNING_MESSAGE, thrown, messageKey, args
    );
  }

  public static Future<?> show(
    final Component parent,
    final Throwable thrown,
    final String messageKey,
    final Object... args)
  {
    return ProblemDialog.show(
      JOptionPane.WARNING_MESSAGE, parent, thrown, messageKey, args
    );
  }

  public static Future<?> show(
    final Component parent,
    final Throwable thrown,
    final String title,
    final String heading,
    final String message)
  {
    return ProblemDialog.show(
      JOptionPane.WARNING_MESSAGE, parent, thrown, title, heading, message
    );
  }

  public static Future<?> showDisableable(
    Object key,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDisableable(
      JOptionPane.WARNING_MESSAGE, key, messageKey, args
    );
  }

  public static Future<?> showDisableable(
    Component parent,
    Object key,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDisableable(
      JOptionPane.WARNING_MESSAGE, parent, key, messageKey, args
    );
  }

  public static Future<?> showDisableable(
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDisableable(
      JOptionPane.WARNING_MESSAGE, thrown, key, messageKey, args
    );
  }

  public static Future<?> showDisableable(
    Component parent,
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDisableable(
      JOptionPane.WARNING_MESSAGE, parent, thrown, key, messageKey, args
    );
  }

  public static Future<?> showDisableable(
    Component parent,
    Throwable thrown,
    Object key,
    String title,
    String heading,
    String message)
  {
    return ProblemDialog.showDisableable(
      JOptionPane.WARNING_MESSAGE, parent,
      thrown, key, title, heading, message
    );
  }

  public static Future<?> showDetails(
    String details,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDetails(
      JOptionPane.WARNING_MESSAGE, details, messageKey, args
    );
  }

  public static Future<?> showDetails(
    Component parent,
    String details,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDetails(
      JOptionPane.WARNING_MESSAGE, parent, details, messageKey, args
    );
  }

  public static Future<?> showDetails(
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDetails(
      JOptionPane.WARNING_MESSAGE, thrown, details, messageKey, args
    );
  }

  public static Future<?> showDetails(
    Component parent,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDetails(
      JOptionPane.WARNING_MESSAGE, parent, thrown, details, messageKey, args
    );
  }

  public static Future<?> showDetails(
    Component parent,
    Throwable thrown,
    String details,
    String title,
    String heading,
    String message)
  {
    return ProblemDialog.showDetails(
      JOptionPane.WARNING_MESSAGE, parent,
      thrown, details, title, heading, message
    );
  }

  public static Future<?> showDetailsDisableable(
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDetailsDisableable(
      JOptionPane.WARNING_MESSAGE, details, key, messageKey, args
    );
  }

  public static Future<?> showDetailsDisableable(
    Component parent,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDetailsDisableable(
      JOptionPane.WARNING_MESSAGE, parent, details, key, messageKey, args
    );
  }

  public static Future<?> showDetailsDisableable(
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDetailsDisableable(
      JOptionPane.WARNING_MESSAGE, thrown, details, key, messageKey, args
    );
  }

  public static Future<?> showDetailsDisableable(
    Component parent,
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    return ProblemDialog.showDetailsDisableable(
      JOptionPane.WARNING_MESSAGE, parent,
      thrown, details, key, messageKey, args
    );
  }

  public static Future<?> showDetailsDisableable(
    Component parent,
    Throwable thrown,
    String details,
    Object key,
    String title,
    String heading,
    String message)
  {
    return ProblemDialog.showDetailsDisableable(
      JOptionPane.WARNING_MESSAGE, parent, thrown,
      details, key, title, heading, message
    );
  }
}
