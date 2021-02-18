/*
 *
 * Copyright (c) 2000-2013 by Brent Easton, Rodney Kinney
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

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.boardPicker.board.mapgrid.Zone;
import VASSAL.build.module.properties.MutablePropertiesContainer;
import VASSAL.build.module.properties.MutableProperty;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.DynamicKeyCommandListConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Objects;

import javax.swing.JLabel;
import javax.swing.KeyStroke;

import org.apache.commons.lang3.StringUtils;

/**
 *
 * @author Brent Easton
 *
 * A trait that allows counters to manipulate the value of Global properties.
 * Uses the Property manipulation functionality of DynamicPropert, but
 * applies them to Global Properties.
 */
public class SetGlobalProperty extends DynamicProperty {
  protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);
  public static final String ID = "setprop;"; // NON-NLS
  public static final String CURRENT_ZONE = "Current Zone/Current Map/Module"; // NON-NLS
  public static final String NAMED_ZONE = "Named Zone"; // NON-NLS
  public static final String NAMED_MAP = "Named Map"; // NON-NLS
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

  @Override
  public String getDescription() {
    return buildDescription("Editor.SetGlobalProperty.trait_description", key, description);
  }

  @Override
  public void mySetType(String s) {
    final SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ';');
    sd.nextToken(); // Skip over command prefix
    key = sd.nextToken("name");
    decodeConstraints(sd.nextToken(""));
    keyCommandListConfig.setValue(sd.nextToken(""));
    keyCommands = keyCommandListConfig.getListValue().toArray(new DynamicKeyCommand[0]);

    menuCommands = Arrays.stream(keyCommands).filter(
      kc -> !StringUtils.isEmpty(kc.getName())
    ).toArray(KeyCommand[]::new);

    description = sd.nextToken("");
    propertyLevel = sd.nextToken(CURRENT_ZONE);
    searchName = sd.nextToken("");
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(key);
    se.append(encodeConstraints());
    se.append(keyCommandListConfig.getValueString());
    se.append(description);
    se.append(propertyLevel);
    se.append(searchName);
    return ID + se.getValue();
  }

  @Override
  public String myGetState() {
    return "";
  }

  @Override
  public void mySetState(String state) {
  }

  /*
   * Duplicate code from Decorator for setProperty(), getProperty() Do not call super.xxxProperty() as we no longer
   * contain a DynamicProperty that can be manipulated, but you cannot call super.super.xxxProperty().
   */
  @Override
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

  @Override
  public Object getLocalizedProperty(Object key) {
    if (Properties.KEY_COMMANDS.equals(key)) {
      return getProperty(key);
    }
    else if (Properties.INNER.equals(key)) {
      return getProperty(key);
    }
    else if (Properties.OUTER.equals(key)) {
      return getProperty(key);
    }
    else if (Properties.VISIBLE_STATE.equals(key)) {
      return getProperty(key);
    }
    else {
      return piece.getLocalizedProperty(key);
    }
  }

  @Override
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

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SetGlobalProperty.html"); // NON-NLS
  }

  /*
   * Locate the correct Global Variable to adjust and update its value. The named global variables must already be
   * defined in the appropriate component before a counter can update them. $xxxx$ names are allowed in both the
   * property name and the target containing map/zone name.
   */
  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    Command comm = new NullCommand();
    for (final DynamicKeyCommand keyCommand : keyCommands) {
      if (keyCommand.matches(stroke)) {
        MutableProperty prop;
        final String propertyName = (new FormattedString(key)).getText(Decorator.getOutermost(this));

        final ArrayList<MutablePropertiesContainer> propertyContainers =
          new ArrayList<>();
        propertyContainers.add(0, GameModule.getGameModule());
        Map map = getMap();
        if (NAMED_MAP.equals(propertyLevel)) {
          final String mapName = (new FormattedString(searchName)).getText(Decorator.getOutermost(this));
          map = Map.getMapById(mapName);
        }
        if (map != null) {
          propertyContainers.add(0, map);
        }
        Zone z = null;
        if (CURRENT_ZONE.equals(propertyLevel) && getMap() != null) {
          z = getMap().findZone(getPosition());
        }
        else if (NAMED_ZONE.equals(propertyLevel) && getMap() != null) {
          final String zoneName = (new FormattedString(searchName)).getText(Decorator.getOutermost(this));
          z = getMap().findZone(zoneName);
        }
        if (z != null) {
          propertyContainers.add(0, z);
        }
        prop = MutableProperty.Util.findMutableProperty(propertyName, propertyContainers);
        /*
         * Debugging could be painful, so print a useful message in the
         * Chat Window if no property can be found to update.
         */
        if (prop == null) {
          final String message = "Unable to locate Global Property in " + propertyLevel + "."; // NON-NLS
          final String data = "Property Expression=[" + key + "], Property Name=" + propertyName + "]."; // NON-NLS
          ErrorDialog.dataWarning(new BadDataReport(this, message, data));
        }
        else {
          final String oldValue = prop.getPropertyValue();
          String newValue = keyCommand.propChanger.getNewValue(oldValue);
          format.setFormat(newValue);
          newValue = format.getText(Decorator.getOutermost(this));
          comm = prop.setPropertyValue(newValue);
        }
      }
    }
    return comm;
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof SetGlobalProperty)) return false;
    final SetGlobalProperty c = (SetGlobalProperty) o;

    if (! Objects.equals(key, c.key)) return false;
    if (! Objects.equals(encodeConstraints(), c.encodeConstraints())) return false;
    if (! Objects.equals(keyCommandListConfig.getValueString(), c.keyCommandListConfig.getValueString())) return false;
    if (! Objects.equals(description, c.description)) return false;
    if (! Objects.equals(propertyLevel, c.propertyLevel)) return false;
    return Objects.equals(searchName, c.searchName);
  }

  protected static class Ed implements PieceEditor {
    protected StringConfigurer descConfig;
    protected FormattedExpressionConfigurer nameConfig;
    protected BooleanConfigurer numericConfig;
    protected JLabel minLabel;
    protected IntConfigurer minConfig;
    protected JLabel maxLabel;
    protected IntConfigurer maxConfig;
    protected JLabel wrapLabel;
    protected BooleanConfigurer wrapConfig;
    protected DynamicKeyCommandListConfigurer keyCommandListConfig;
    protected TranslatingStringEnumConfigurer levelConfig;
    protected FormattedExpressionConfigurer searchNameConfig;
    protected String mapText = Resources.getString("Editor.SetGlobalProperty.name_of_map");
    private final String mapHint = Resources.getString("Editor.GlobalKeyCommand.map_name_hint");
    protected String zoneText = Resources.getString("Editor.SetGlobalProperty.name_of_zone");
    private final String zoneHint = Resources.getString("Editor.GlobalKeyCommand.zone_name_hint");
    protected JLabel searchLabel;
    protected TraitConfigPanel controls;

    public Ed(final SetGlobalProperty m) {
      keyCommandListConfig = new DynamicKeyCommandListConfigurer(null, Resources.getString("Editor.DynamicProperty.commands"), m);
      keyCommandListConfig.setValue(new ArrayList<>(Arrays.asList(m.keyCommands)));

      controls = new TraitConfigPanel();

      descConfig = new StringConfigurer(m.description);
      descConfig.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descConfig);

      nameConfig = new FormattedExpressionConfigurer(m.getKey(), (EditablePiece) m);
      nameConfig.setHintKey("Editor.SetGlobalProperty.global_property_name_hint");
      controls.add("Editor.SetGlobalProperty.global_property_name", nameConfig);

      final String[] levelKeys = {
        "Editor.SetGlobalProperty.current",
        "Editor.SetGlobalProperty.named_zone",
        "Editor.SetGlobalProperty.named_map"
      };
      levelConfig = new TranslatingStringEnumConfigurer(
        new String[] { CURRENT_ZONE, NAMED_ZONE, NAMED_MAP },
        levelKeys,
        m.propertyLevel);
      levelConfig.addPropertyChangeListener(e -> updateVisibility());
      controls.add("Editor.SetGlobalProperty.locate_property", levelConfig);

      searchNameConfig = new FormattedExpressionConfigurer(m.searchName, (EditablePiece) m);
      searchLabel = new JLabel(mapText);
      controls.add(searchLabel, searchNameConfig);

      numericConfig = new BooleanConfigurer(m.isNumeric());
      controls.add("Editor.DynamicProperty.is_numeric", numericConfig);

      minLabel = new JLabel(Resources.getString("Editor.GlobalProperty.minimum_value"));
      minConfig = new IntConfigurer(m.getMinimumValue());
      controls.add(minLabel, minConfig);

      maxLabel = new JLabel(Resources.getString("Editor.GlobalProperty.maximum_value"));
      maxConfig = new IntConfigurer(m.getMaximumValue());
      controls.add(maxLabel, maxConfig);

      wrapLabel  = new JLabel(Resources.getString("Editor.DynamicProperty.wrap"));
      wrapConfig = new BooleanConfigurer(m.isWrap());
      controls.add(wrapLabel, wrapConfig);

      controls.add("Editor.DynamicProperty.key_commands", keyCommandListConfig);

      numericConfig.addPropertyChangeListener(evt -> {
        final boolean isNumeric = numericConfig.booleanValue();
        minConfig.getControls().setVisible(isNumeric);
        minLabel.setVisible(isNumeric);
        maxConfig.getControls().setVisible(isNumeric);
        maxLabel.setVisible(isNumeric);
        wrapConfig.getControls().setVisible(isNumeric);
        wrapLabel.setVisible(isNumeric);
        keyCommandListConfig.repack();
      });

      numericConfig.fireUpdate();
      updateVisibility();
    }

    protected void updateVisibility() {
      switch (levelConfig.getValueString()) {
      case NAMED_MAP :
        searchLabel.setText(mapText);
        searchNameConfig.updateHint(mapHint);
        break;
      case NAMED_ZONE :
        searchLabel.setText(zoneText);
        searchNameConfig.updateHint(zoneHint);
        break;
      }
      searchLabel.setVisible(!levelConfig.getValueString().equals(CURRENT_ZONE));
      searchNameConfig.getControls().setVisible(!levelConfig.getValueString().equals(CURRENT_ZONE));

      repack(controls);
    }

    @Override
    public Component getControls() {
      return controls;
    }

    protected String encodeConstraints() {
      return new SequenceEncoder(',')
        .append(numericConfig.getValueString())
        .append(minConfig.getValueString())
        .append(maxConfig.getValueString())
        .append(wrapConfig.getValueString())
        .getValue();
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameConfig.getValueString());
      se.append(encodeConstraints());
      se.append(keyCommandListConfig.getValueString());
      se.append(descConfig.getValueString());
      se.append(levelConfig.getValueString());
      se.append(searchNameConfig.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
