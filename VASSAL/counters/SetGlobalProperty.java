/*
 * $Id: SetGlobalProperty.java 965 2006-09-03 16:34:05 +0000 (Sun, 03 Sep 2006) rodneykinney $
 *
 * Copyright (c) 2000-2006 by Brent Easton, Rodney Kinney
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

import java.awt.Component;
import java.awt.Window;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.GlobalProperty;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.ListConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.StringEnumConfigurer;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * 
 * @author Brent Easton
 * 
 * A trait that allows counters to manipulate the value of Global properties. Uses the Property manipulation
 * functionality of DynamicPropert, but applies them to Global Properties.
 * 
 */
public class SetGlobalProperty extends DynamicProperty {
  protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);
  public static final String ID = "setprop;";
  public static final String CURRENT_ZONE = "Current Zone/Current Map/Module";
  public static final String NAMED_ZONE = "Named Zone";
  public static final String NAMED_MAP = "Named Map";
  protected String description;
  protected String propertyLevel;
  protected String searchName;
  protected Decorator dec;

  public SetGlobalProperty() {
    this(ID, null);
  }

  public SetGlobalProperty(String type, GamePiece p) {
    super(type, p);
  }

  public String getDescription() {
    String s = "Set Global Property";
    if (description.length() > 0) {
      s += " - " + description;
    }
    return s;
  }

  public void mySetType(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    sd.nextToken(); // Skip over command prefix
    key = sd.nextToken("name");
    decodeConstraints(sd.nextToken(""));
    keyCommandListConfig.setValue(sd.nextToken(""));
    keyCommands = (DynamicKeyCommand[]) keyCommandListConfig.getListValue().toArray(new DynamicKeyCommand[keyCommandListConfig.getListValue().size()]);
    List l = new ArrayList();
    for (int i = 0; i < keyCommands.length; i++) {
      if (keyCommands[i].getName() != null && keyCommands[i].getName().length() > 0) {
        l.add(keyCommands[i]);
      }
    }
    menuCommands = (DynamicKeyCommand[]) l.toArray(new DynamicKeyCommand[l.size()]);
    description = sd.nextToken("");
    propertyLevel = sd.nextToken(CURRENT_ZONE);
    searchName = sd.nextToken("");
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(key);
    se.append(encodeConstraints());
    se.append(keyCommandListConfig.getValueString());
    se.append(description);
    se.append(propertyLevel);
    se.append(searchName);
    return ID + se.getValue();
  }

  public String myGetState() {
    return "";
  }

  public void mySetState(String state) {
  }

  /*
   * Duplicate code from Decorator for setProperty(), getProperty() Do not call super.xxxProperty() as we no longer
   * contain a DynamicProperty that can be manipulated, but you cannot call super.super.xxxProperty().
   */
  public Object getProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getKeyCommands();
    }
    else if (Properties.INNER.equals(key)) {
      return piece;
    }
    else if (Properties.OUTER.equals(key)) {
      return dec;
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return myGetState() + piece.getProperty(key);
    }
    else {
      return piece.getProperty(key);
    }
  }

  public void setProperty(Object key, Object val) {
    if (Properties.INNER.equals(key)) {
      setInner((GamePiece) val);
    }
    else if (Properties.OUTER.equals(key)) {
      dec = (Decorator) val;
    }
    else {
      piece.setProperty(key, val);
    }
  }
  
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SetGlobalProperty.htm");
  }

  /*
   * Locate the correct Global Variable to adjust and update its value. The named global variables must already be
   * defined in the appropriate component before a counter can update them. $xxxx$ names are allowed in both the
   * property name and the target containing map/zone name.
   */
  public Command myKeyEvent(KeyStroke stroke) {
    Command comm = new NullCommand();
    for (int i = 0; i < keyCommands.length; i++) {
      if (keyCommands[i].matches(stroke)) {
        GlobalProperty prop = null;
        String propertyName = (new FormattedString(key)).getText(Decorator.getOutermost(this));
        
        List propertyContainers = new ArrayList();
        Map map = getMap();
        if (NAMED_MAP.equals(propertyLevel)) {
          String mapName = (new FormattedString(searchName)).getText(Decorator.getOutermost(this));
          map = Map.getMapById(mapName);
        }
        propertyContainers.add(0, map);
        Zone z = null;
        if (CURRENT_ZONE.equals(propertyLevel) && getMap() != null) {
          z = getMap().findZone(getPosition());
        }
        else if (NAMED_ZONE.equals(propertyLevel) && getMap() != null) {
          String zoneName = (new FormattedString(searchName)).getText(Decorator.getOutermost(this));
          z = getMap().findZone(zoneName);
        }
        propertyContainers.add(0, z);
        prop = GlobalProperty.findGlobalProperty(propertyName, propertyContainers);
        /*
         * Debugging could be painful, so print a useful message in the Chat Window if no property can be found to
         * update
         */
        if (prop == null) {
          String s = "Set Global Property (" + description + "): Unable to locate Global Property named " + propertyName;
          if (!propertyLevel.equals(CURRENT_ZONE)) {
            s += " in " + propertyLevel + " " + searchName;
          }
          GameModule.getGameModule().warn(s);
        }
        else {
          String oldValue = prop.getPropertyValue();
          String newValue = keyCommands[i].propChanger.getNewValue(oldValue);
          format.setFormat(newValue);
          newValue = format.getText(Decorator.getOutermost(this));
          comm = prop.setPropertyValue(newValue);
        }
      }
    }
    return comm;
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }
  protected static class Ed implements PieceEditor {
    protected StringConfigurer descConfig;
    protected StringConfigurer nameConfig;
    protected BooleanConfigurer numericConfig;
    protected IntConfigurer minConfig;
    protected IntConfigurer maxConfig;
    protected BooleanConfigurer wrapConfig;
    protected ListConfigurer keyCommandListConfig;
    protected StringEnumConfigurer levelConfig;
    protected StringConfigurer searchNameConfig;
    protected JLabel mapLabel = new JLabel("map");
    protected JLabel zoneLabel = new JLabel("zone");
    protected Box controls;
    protected Box nameBox;

    public Ed(final SetGlobalProperty m) {
      keyCommandListConfig = new ListConfigurer(null, "Key Commands") {
        protected Configurer buildChildConfigurer() {
          return new DynamicKeyCommandConfigurer(m);
        }
      };
      keyCommandListConfig.setValue(new ArrayList(Arrays.asList(m.keyCommands)));
      PropertyChangeListener l = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent evt) {
          boolean isNumeric = numericConfig.booleanValue().booleanValue();
          minConfig.getControls().setVisible(isNumeric);
          maxConfig.getControls().setVisible(isNumeric);
          wrapConfig.getControls().setVisible(isNumeric);
          keyCommandListConfig.repack();
        }
      };
      controls = Box.createVerticalBox();
      descConfig = new StringConfigurer(null, "Description:  ", m.description);
      controls.add(descConfig.getControls());
      nameConfig = new StringConfigurer(null, "Global Property Name:  ", m.getKey());
      controls.add(nameConfig.getControls());
      levelConfig = new StringEnumConfigurer(null, "", new String[]{CURRENT_ZONE, NAMED_ZONE, NAMED_MAP});
      levelConfig.setValue(m.propertyLevel);
      levelConfig.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateVisibility();
        }
      });
      Box box = Box.createHorizontalBox();
      box.add(new JLabel("Locate Property starting in the:   "));
      box.add(levelConfig.getControls());
      controls.add(box);
      nameBox = Box.createHorizontalBox();
      nameBox.add(new JLabel("Name of "));
      nameBox.add(mapLabel);
      nameBox.add(zoneLabel);
      nameBox.add(new JLabel(" containing property:  "));
      searchNameConfig = new StringConfigurer(null, "", m.searchName);
      nameBox.add(searchNameConfig.getControls());
      controls.add(nameBox);
      numericConfig = new BooleanConfigurer(null, "Is numeric: ", m.isNumeric());
      controls.add(numericConfig.getControls());
      minConfig = new IntConfigurer(null, "Minimum value: ", new Integer(m.getMinimumValue()));
      controls.add(minConfig.getControls());
      maxConfig = new IntConfigurer(null, "Maximum value: ", new Integer(m.getMaximumValue()));
      controls.add(maxConfig.getControls());
      wrapConfig = new BooleanConfigurer(null, "Wrap ", m.isWrap());
      controls.add(wrapConfig.getControls());
      controls.add(keyCommandListConfig.getControls());
      numericConfig.addPropertyChangeListener(l);
      numericConfig.fireUpdate();
      updateVisibility();
    }

    protected void updateVisibility() {
      mapLabel.setVisible(levelConfig.getValueString().equals(NAMED_MAP));
      zoneLabel.setVisible(levelConfig.getValueString().equals(NAMED_ZONE));
      nameBox.setVisible(!levelConfig.getValueString().equals(CURRENT_ZONE));
      Window w = SwingUtilities.getWindowAncestor(controls);
      if (w != null) {
        w.pack();
      }
    }

    public Component getControls() {
      return controls;
    }

    protected String encodeConstraints() {
      return new SequenceEncoder(',').append(numericConfig.getValueString()).append(minConfig.getValueString()).append(maxConfig.getValueString()).append(
          wrapConfig.getValueString()).getValue();
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameConfig.getValueString());
      se.append(encodeConstraints());
      se.append(keyCommandListConfig.getValueString());
      se.append(descConfig.getValueString());
      se.append(levelConfig.getValueString());
      se.append(searchNameConfig.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      return "";
    }
  }
}
