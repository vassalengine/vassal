
package VASSAL.tools;

import java.awt.Component;
import javax.swing.JOptionPane;


public class WarningDialog {
  private WarningDialog() {}

  public static void show(
    String messageKey,
    Object... args)
  {
    ProblemDialog.show(JOptionPane.WARNING_MESSAGE, messageKey, args);
  }

  public static void show(
    Component parent,
    String messageKey,
    Object... args)
  {
    ProblemDialog.show(JOptionPane.WARNING_MESSAGE, parent, messageKey, args);
  }

  public static void show(
    Throwable thrown,
    String messageKey,
    Object... args)
  {
    ProblemDialog.show(JOptionPane.WARNING_MESSAGE, thrown, messageKey, args);
  }

  public static void show(
    final Component parent,
    final Throwable thrown,
    final String messageKey,
    final Object... args)
  {
    ProblemDialog.show(JOptionPane.WARNING_MESSAGE, parent,
                       thrown, messageKey, args);
  }

  public static void show(
    final Component parent,
    final Throwable thrown,
    final String title,
    final String heading,
    final String message)
  {
    ProblemDialog.show(JOptionPane.WARNING_MESSAGE, parent,
                       thrown, title, heading, message);
  }

  public static void showDisableable(
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDisableable(JOptionPane.WARNING_MESSAGE,
                                  key, messageKey, args);
  }

  public static void showDisableable(
    Component parent,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDisableable(JOptionPane.WARNING_MESSAGE,
                                  parent, key, messageKey, args);
  }

  public static void showDisableable(
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDisableable(JOptionPane.WARNING_MESSAGE,
                                  thrown, key, messageKey, args);
  }

  public static void showDisableable(
    Component parent,
    Throwable thrown,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDisableable(JOptionPane.WARNING_MESSAGE, parent,
                                  thrown, key, messageKey, args);
  }

  public static void showDisableable(
    Component parent,
    Throwable thrown,
    Object key,
    String title,
    String heading,
    String message)
  {
    ProblemDialog.showDisableable(JOptionPane.WARNING_MESSAGE, parent,
                                  thrown, key, title, heading, message);
  }

  public static void showDetails(
    String details,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetails(JOptionPane.WARNING_MESSAGE,
                              details, messageKey, args);
  }

  public static void showDetails(
    Component parent,
    String details,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetails(JOptionPane.WARNING_MESSAGE,
                              parent, details, messageKey, args);
  }
  
  public static void showDetails(
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetails(JOptionPane.WARNING_MESSAGE,
                              thrown, details, messageKey, args);
  }

  public static void showDetails(
    Component parent,
    Throwable thrown,
    String details,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetails(JOptionPane.WARNING_MESSAGE,
                              parent, thrown, details, messageKey, args);
  }

   public static void showDetails(
    Component parent,
    Throwable thrown,
    String details,
    String title,
    String heading,
    String message)
  {
    ProblemDialog.showDetails(JOptionPane.WARNING_MESSAGE,
                              parent, thrown, details, title, heading, message);
  }

    public static void showDetailsDisableable(
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.WARNING_MESSAGE,
                                         details, key, messageKey, args);
  }

  public static void showDetailsDisableable(
    Component parent,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.WARNING_MESSAGE,
      parent, details, key, messageKey, args);
  }
  
  public static void showDetailsDisableable(
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.WARNING_MESSAGE,
      thrown, details, key, messageKey, args);
  }

  public static void showDetailsDisableable(
    Component parent,
    Throwable thrown,
    String details,
    Object key,
    String messageKey,
    Object... args)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.WARNING_MESSAGE,
      parent, thrown, details, key, messageKey, args);
  }

  public static void showDetailsDisableable(
    Component parent,
    Throwable thrown,
    String details,
    Object key,
    String title,
    String heading,
    String message)
  {
    ProblemDialog.showDetailsDisableable(JOptionPane.WARNING_MESSAGE,
      parent, thrown, details, key, title, heading, message);
  }
}
