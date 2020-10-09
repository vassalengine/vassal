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

import VASSAL.configure.ColorConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.i18n.Resources;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * A standardised Panel for use by Trait configurers
 */
public class TraitConfigPanel extends JPanel {

  /**
   * Create a new default Trait Config Panel
   */
  public TraitConfigPanel() {
    super(new TraitLayout());
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
   * @param c Conigurer
   */
  public void addControls(Configurer c) {
    add(c.getControls());
  }

  public void addControls(Configurer c, String configurerConstraints) {
    add(c.getControls(), configurerConstraints);
  }

  /**
   * Generate and add a label for an i18nKey and add Configurer controls
   * @param i18nKey i18n Key
   * @param c Configurer
   */
  public void add(String i18nKey, Configurer c) {
    addLabel(Resources.getString(i18nKey));
    addControls(c);
  }

  public void add(String i18nKey, Configurer c, String configureConstraints) {
    addLabel(Resources.getString(i18nKey));
    addControls(c, configureConstraints);
  }

  /**
   * Generate and add a label for an i18nKey and add a ColorConfigurer controls
   * NOTE the addition of the grow 0 constraint to prevent the Color button stretching
   * @param i18nKey i18n Key
   * @param c ColorConfigurer
   */
  public void add(String i18nKey, ColorConfigurer c) {
    add(i18nKey, c, "grow 0"); // NON-NLS
  }
  /**
   * Add an existing JLabel and add Configurer Controls
   * @param label Jlabel
   * @param c Configurer
   */
  public void add(JLabel label, Configurer c) {
    add(label);
    addControls(c);
  }

  /**
   * Add a label based on an i18Key and an existing JPanel containing controls
   * @param i18nKey i18n Key
   * @param p Jpanel containing controls
   */
  public void add(String i18nKey, JPanel p) {
    addLabel(Resources.getString(i18nKey));
    add(p);
  }
}
