/*
 *
 * Copyright (c) 2020 by VASSAL Development Team
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

package VASSAL.counters;

import VASSAL.configure.ConfigurableList;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;

import java.awt.Component;
import javax.swing.JLabel;
import javax.swing.JPanel;



/**
 * A standardised Panel for use by Trait configurers
 */
public class TraitConfigPanel extends JPanel {
  private static final long serialVersionUID = 1L;

  /**
   * Create a new default Trait Config Panel
   */
  public TraitConfigPanel() {
    this(false);
  }

  /**
   * Create a new default Trait Config Panel and specify debug option
   *
   * @param debug Turn debug on?
   */
  public TraitConfigPanel(boolean debug) {
    super(new TraitLayout(debug));
  }

  /**
   * Create a new Trait Config Panel with a non-standard layout
   *
   * @param layout Non-standard layout
   */
  public TraitConfigPanel(TraitLayout layout) {
    super(layout);
  }

  /**
   * Add a label as a JLabel.
   * @param text text to wrap in the JLabel
   */
  public void addLabel(String text) {
    add(new JLabel(text));
  }

  /**
   * Add the Controls from a Configurer
   * @param c Configurer
   */
  public void addControls(Configurer c) {
    add(c.getControls());
  }

  public void addControls(Configurer c, String configurerConstraints) {
    add(c.getControls(), configurerConstraints);
  }

  /**
   * Add a component and its label with additional MigLayout constraints to this panel.
   *
   * NOTE: This is the base level add() method. All other add() overrides should eventually
   * call this method.
   *
   * @param label Label
   * @param c Configurer components
   * @param constraints MigLayout constraints
   */
  public void add(JLabel label, Component c, String constraints) {
    label.setLabelFor(c);
    add(label);
    add(c, constraints);
  }

  /**
   * Add a Configurer and its label with additional MigLayout constraints to this panel.
   *
   * @param label Label
   * @param c Configurer
   * @param configureConstraints Miglayout Constraints
   */
  public void add(JLabel label, Configurer c, String configureConstraints) {
    add(label, c.getControls(), configureConstraints);
  }

  /**
   * Add a Configurer and its label to this panel
   *
   * @param label JLabel
   * @param c Configurer
   */
  public void add(JLabel label, Configurer c) {
    add(label, c, ""); // NON-NLS
  }

  /**
   * Generate and add a label for an i18nKey and add Configurer controls
   *
   * @param i18nKey i18n Key
   * @param c Configurer
   */
  public void add(String i18nKey, Configurer c) {
    add(new JLabel(Resources.getString(i18nKey)), c);
  }

  /**
   * Generate and add a label for an i18nKey and add Configurer controls with MigLayout constraints
   *
   * @param i18nKey I18n key
   * @param c Configurer
   * @param configureConstraints MigLayout constraints
   */
  public void add(String i18nKey, Configurer c, String configureConstraints) {
    add(new JLabel(Resources.getString(i18nKey)), c, configureConstraints);
  }

  /**
   * Add an existing JLabel and existing JPanel containing controls.
   *
   * @param label JLabel
   * @param panel JPanel
   */
  public void add(JLabel label, JPanel panel) {
    add(label, panel, "");
  }
  /**
   * Add a label based on an i18Key and an existing JPanel containing controls
   * @param i18nKey i18n Key
   * @param p JPanel containing controls
   */
  public void add(String i18nKey, JPanel p) {
    add(new JLabel(Resources.getString(i18nKey)), p);
  }

  public void add(String i18nKey, JPanel p, String constraints) {
    add(new JLabel(Resources.getString(i18nKey)), p, constraints);
  }
}
