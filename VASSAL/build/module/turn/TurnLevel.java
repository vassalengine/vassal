/*
 * $Id: TurnLevel.java 919 2006-05-29 13:24:46 +0000 (Mon, 29 May 2006) swampwallaby $
 * 
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package VASSAL.build.module.turn;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JMenu;
import javax.swing.JPanel;
import VASSAL.build.AutoConfigurable;
import VASSAL.build.Buildable;
import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.configure.Configurer;
import VASSAL.configure.ConfigurerFactory;
import VASSAL.configure.FormattedStringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.tools.FormattedString;

public abstract class TurnLevel extends TurnComponent {

  protected static final String NAME = "name";
  protected static final String TURN_FORMAT = "turnFormat";

  protected static final String LEVEL_VALUE = "value";

  protected TurnTracker turn;
  protected JDialog setDialog;
  protected JPanel levelSetControls = null;
  protected Component childSetControls = null;

  protected int start = 0; // Counter Start value

  protected int current = 0; // Current counter pointer

  protected int currentSubLevel = 0; // sub-level pointer

  protected boolean subLevelRolledOver = false;
  protected boolean rolledOver = false;
  protected MutableProperty.Impl myValue = new MutableProperty.Impl("",this);

  protected FormattedString turnFormat;

  protected abstract String getState();

  protected abstract void setState(String code);

  protected abstract String getValueString();

  protected abstract String getLongestValueName();

  protected abstract Component getSetControl();

  public TurnLevel() {
    super();
    turnFormat = new FormattedString("$" + LEVEL_VALUE + "$");
  }

  protected boolean hasSubLevelRolledOver() {
    return subLevelRolledOver;
  }

  protected boolean isActive() {
    return true;
  }
  
  protected void reset() {
    for (int i = 0; i < getTurnLevelCount(); i++) {
      getTurnLevel(i).reset();
    }
    currentSubLevel = 0;
  }

  protected void advance() {
    rolledOver = false;
    subLevelRolledOver = false;
    if (getTurnLevelCount() > 0) {
      TurnLevel subLevel = getTurnLevel(currentSubLevel);
      subLevel.advance();
      if (subLevel.hasRolledOver()) {
        currentSubLevel++;
        if (currentSubLevel >= getTurnLevelCount()) {
          currentSubLevel = 0;
          subLevelRolledOver = true;
        }
        getTurnLevel(currentSubLevel).setLow();
      }
    }
  }

  protected void retreat() {
    rolledOver = false;
    subLevelRolledOver = false;
    if (getTurnLevelCount() > 0) {
      TurnLevel subLevel = getTurnLevel(currentSubLevel);
      subLevel.retreat();
      if (subLevel.hasRolledOver()) {
        currentSubLevel--;
        if (currentSubLevel < 0) {
          currentSubLevel = getTurnLevelCount() - 1;
          subLevelRolledOver = true;
        }
        getTurnLevel(currentSubLevel).setHigh();
      }
    }
  }

  // Return the description of this turn
  public String getTurnString() {
    turnFormat.setProperty(LEVEL_VALUE, getValueString());
    return turnFormat.getText();
  }

  public void getTurnStrings(ArrayList desc) {
    desc.add(getTurnString());
    if (getTurnLevelCount() > 0) {
      getTurnLevel(currentSubLevel).getTurnStrings(desc);
    }
  }
  
  public void getTurnValues(ArrayList desc) {
    desc.add(getValueString());
    if (getTurnLevelCount() > 0) {
      getTurnLevel(currentSubLevel).getTurnValues(desc);
    }
  }

  public void getTurnNames(ArrayList desc) {
    desc.add(getConfigureName());
    if (getTurnLevelCount() > 0) {
      getTurnLevel(currentSubLevel).getTurnNames(desc);
    }
  }
  
  protected void buildConfigMenu(JMenu menu) {
    JMenu m = getConfigMenu();
    if (m != null) {
      menu.add(m);
    }
  }

  protected JMenu getConfigMenu() {

    JMenu menu = new JMenu(getConfigureName());

    for (int i = 0; i < getTurnLevelCount(); i++) {
      getTurnLevel(i).buildConfigMenu(menu);
    }

    return menu.getItemCount() == 0 ? null : menu;

  }

  public boolean isConfigurable() {

    for (int i = 0; i < getTurnLevelCount(); i++) {
      if (getTurnLevel(i).isConfigurable()) {
        return true;
      }
    }

    return false;
  }

  private static final Dimension FILLER = new Dimension(0, 3);

  protected Component getSetControls(JDialog dialog, TurnTracker turn) {
    this.turn = turn;
    this.setDialog = dialog;

    levelSetControls = new JPanel();
    levelSetControls.setLayout(new BoxLayout(levelSetControls, BoxLayout.Y_AXIS));
    JPanel p = new JPanel();
    p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS));
    p.setBorder(BorderFactory.createLineBorder(Color.black));

    p.add(Box.createRigidArea(FILLER));
    p.add(getSetControl());
    p.add(Box.createRigidArea(FILLER));

    if (getTurnLevelCount() > 1) {
      String s[] = new String[getTurnLevelCount()];
      for (int i = 0; i < s.length; i++) {
        s[i] = getTurnLevel(i).getConfigureName();
      }
      StringEnumConfigurer e = new StringEnumConfigurer(null, " Select:  ", s);
      e.setValue(getTurnLevel(currentSubLevel).getConfigureName());
      e.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          String option = ((StringEnumConfigurer) e.getSource()).getValueString();
          for (int i = 0; i < getTurnLevelCount(); i++) {
            if (option.equals(getTurnLevel(i).getConfigureName())) {
              currentSubLevel = i;
              addChildControls();
            }
          }
        }
      });
      p.add(e.getControls());
      p.add(Box.createRigidArea(FILLER));
    }

    levelSetControls.add(p);
    levelSetControls.add(Box.createRigidArea(FILLER));

    addChildControls();

    return levelSetControls;
  }

  protected void addChildControls() {
    if (childSetControls != null) {
      levelSetControls.remove(childSetControls);
    }
    if (getTurnLevelCount() > 0) {
      childSetControls = getTurnLevel(currentSubLevel).getSetControls(setDialog, turn);
      levelSetControls.add(childSetControls);
      setDialog.pack();
    }
  }

  protected TurnTracker getTurn() {
    return turn;
  }

  protected void setRolledOver(boolean b) {
    rolledOver = b;
  }

  protected boolean hasRolledOver() {
    return rolledOver;
  }

  protected void setLow() {
    if (getTurnLevelCount() > 0) {
      currentSubLevel = 0;
      getTurnLevel(currentSubLevel).setLow();
    }
    myValue.setPropertyValue(getValueString());
  }

  protected void setHigh() {
    if (getTurnLevelCount() > 0) {
      currentSubLevel = getTurnLevelCount() - 1;
      getTurnLevel(currentSubLevel).setHigh();
    }
    myValue.setPropertyValue(getValueString());
  }

  public String[] getAttributeDescriptions() {
    return new String[] { "Name:  ", "Turn Format:  " };
  }

  public Class[] getAttributeTypes() {
    return new Class[] { String.class, TurnFormatConfig.class };
  }

  public String[] getAttributeNames() {
    return new String[] { NAME, TURN_FORMAT };
  }

  public void setAttribute(String key, Object value) {
    if (NAME.equals(key)) {
      setConfigureName((String) value);
      myValue.setPropertyName(getConfigureName());
    }
    else if (TURN_FORMAT.equals(key)) {
      turnFormat.setFormat((String) value);
    }
  }

  public String getAttributeValueString(String key) {
    if (NAME.equals(key)) {
      return getConfigureName();
    }
    else if (TURN_FORMAT.equals(key)) {
      return turnFormat.getFormat();
    }
    else {
      return "";
    }
  }

  public void addTo(Buildable parent) {
    ((TurnComponent) parent).addLevel(this);
    myValue.addTo(GameModule.getGameModule());
  }

  public void removeFrom(Buildable parent) {
    ((TurnComponent) parent).removeLevel(this);
    myValue.removeFromContainer();
  }

  public HelpFile getHelpFile() {
    return null;
  }

  public Class[] getAllowableConfigureComponents() {
    return new Class[] { CounterTurnLevel.class, ListTurnLevel.class };
  }

  public static class TurnFormatConfig implements ConfigurerFactory {
    public Configurer getConfigurer(AutoConfigurable c, String key, String name) {
      return new FormattedStringConfigurer(key, name, new String[] { LEVEL_VALUE });
    }
  }

}
