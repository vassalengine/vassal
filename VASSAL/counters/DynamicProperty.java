/*
 * $Id$
 *
 * Copyright (c) 2000-2006 by Rodney Kinney, Brent Easton
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
import java.awt.Point;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.swing.Box;
import javax.swing.KeyStroke;
import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertyChanger;
import VASSAL.build.module.properties.PropertyChangerConfigurer;
import VASSAL.build.module.properties.PropertyPrompt;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.HotKeyConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.ListConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.SequenceEncoder;

/**
 * Trait that contains a property accessible via getProperty() and updateable
 * dynamically via key commands
 * 
 * @author rkinney
 * 
 */
public class DynamicProperty extends Decorator implements TranslatablePiece, PropertyPrompt.DialogParent, PropertyChangerConfigurer.Constraints {

  public static final String ID = "PROP;";

  protected String value;

  protected String key;
  protected boolean numeric;
  protected int minValue;
  protected int maxValue;
  protected boolean wrap;
  protected FormattedString format = new FormattedString();

  protected DynamicKeyCommand[] keyCommands;
  protected DynamicKeyCommand[] menuCommands;

  protected ListConfigurer keyCommandListConfig;

  public DynamicProperty() {
    this(ID, null);
  }

  public DynamicProperty(String type, GamePiece p) {
    setInner(p);
    keyCommandListConfig = new ListConfigurer(null, "Commands") {
      protected Configurer buildChildConfigurer() {
        return new DynamicKeyCommandConfigurer(DynamicProperty.this);
      }
    };
    mySetType(type);
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
  }

  protected void decodeConstraints(String s) {
    SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s, ',');
    numeric = sd.nextBoolean(false);
    minValue = sd.nextInt(0);
    maxValue = sd.nextInt(100);
    wrap = sd.nextBoolean(false);
  }

  protected String encodeConstraints() {
    return new SequenceEncoder(',').append(numeric).append(minValue).append(maxValue).append(wrap).getValue();
  }

  public void draw(java.awt.Graphics g, int x, int y, java.awt.Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  public String getName() {
    return piece.getName();
  }

  public java.awt.Rectangle boundingBox() {
    return piece.boundingBox();
  }

  public Shape getShape() {
    return piece.getShape();
  }

  public Object getProperty(Object key) {
    if (key.equals(getKey())) {
      return getValue();
    }
    return super.getProperty(key);
  }

  public void setProperty(Object key, Object value) {
    if (key.equals(getKey())) {
      setValue((String) value);
    }
    else {
      super.setProperty(key, value);
    }
  }

  public String myGetState() {
    return getValue();
  }

  public Component getComponent() {
    return getMap() != null ? getMap().getView().getTopLevelAncestor() : GameModule.getGameModule().getFrame();
  }

  public void mySetState(String state) {
    setValue(state);
  }

  public String getKey() {
    return key;
  }

  public String getValue() {
    return value;
  }

  public void setValue(String value) {
    Stack parent = getParent();
    Map map = getMap();
    // If the property has changed the layer to which this piece belongs,
    // re-insert it into the map
    if (map != null) {
      GamePiece outer = Decorator.getOutermost(this);
      if (parent == null) {
        Point pos = getPosition();
        map.removePiece(outer);
        this.value = value;
        map.placeOrMerge(outer, pos);
      }
      else {
        GamePiece other = parent.getPieceBeneath(outer);
        if (other == null) {
          other = parent.getPieceAbove(outer);
        }
        if (other == null) {
          Point pos = parent.getPosition();
          map.removePiece(parent);
          this.value = value;
          map.placeOrMerge(parent,pos);
        }
        else {
          this.value = value;
          if (!map.getPieceCollection().canMerge(other, outer)) {
            map.placeOrMerge(outer, parent.getPosition());
          }
        }
      }
    }
    else {
      this.value = value;
    }
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(key);
    se.append(encodeConstraints());
    se.append(keyCommandListConfig.getValueString());
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    return menuCommands;
  }

  public Command myKeyEvent(KeyStroke stroke) {
    ChangeTracker tracker = new ChangeTracker(this);
    for (int i = 0; i < keyCommands.length; i++) {
      if (keyCommands[i].matches(stroke)) {
        String newValue = keyCommands[i].propChanger.getNewValue(value);
        format.setFormat(newValue);
        newValue = format.getText(Decorator.getOutermost(this));
        setValue(newValue);
      }
    }
    Command comm = tracker.getChangeCommand();
    return comm;
  }

  public String getDescription() {
    String s = "Dynamic Property";
    if (getKey() != null && getKey().length() > 0) {
      s += " - " + getKey();
    }
    return s;
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("DynamicProperty.htm");
  }

  public int getMaximumValue() {
    return maxValue;
  }

  public int getMinimumValue() {
    return minValue;
  }

  public boolean isNumeric() {
    return numeric;
  }

  public boolean isWrap() {
    return wrap;
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }
  
  public PieceI18nData getI18nData() {
    String[] commandNames = new String[menuCommands.length];
    String[] commandDescs = new String[menuCommands.length];
    for (int i=0; i < menuCommands.length; i++) {
      commandNames[i] = menuCommands[i].getName();
      commandDescs[i] = "Property " + key + ": Menu Command " + i;
    }
    return getI18nData(commandNames, commandDescs);
  }

  protected static class Ed implements PieceEditor {
    protected StringConfigurer nameConfig;
    protected StringConfigurer initialValueConfig;
    protected BooleanConfigurer numericConfig;
    protected IntConfigurer minConfig;
    protected IntConfigurer maxConfig;
    protected BooleanConfigurer wrapConfig;
    protected ListConfigurer keyCommandListConfig;
    protected Box controls;

    public Ed(final DynamicProperty m) {
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
      nameConfig = new StringConfigurer(null, "Name:  ", m.getKey());
      controls.add(nameConfig.getControls());
      initialValueConfig = new StringConfigurer(null, "Value:  ", m.getValue());
      controls.add(initialValueConfig.getControls());
      numericConfig = new BooleanConfigurer(null, "Is numeric:  ", m.isNumeric());
      controls.add(numericConfig.getControls());
      minConfig = new IntConfigurer(null, "Minimum value:  ", new Integer(m.getMinimumValue()));
      controls.add(minConfig.getControls());
      maxConfig = new IntConfigurer(null, "Maximum value:  ", new Integer(m.getMaximumValue()));
      controls.add(maxConfig.getControls());
      wrapConfig = new BooleanConfigurer(null, "Wrap?", m.isWrap());
      controls.add(wrapConfig.getControls());
      controls.add(keyCommandListConfig.getControls());

      numericConfig.addPropertyChangeListener(l);
      numericConfig.fireUpdate();
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
      return ID + se.getValue();
    }

    public String getState() {
      return initialValueConfig.getValueString();
    }

  }

  /**
   * DynamicKeyCommand A class that represents an action to be performed on a
   * Dynamic property
   */
  protected static class DynamicKeyCommand extends KeyCommand {
    private static final long serialVersionUID = 1L;

    protected PropertyChanger propChanger = null;

    public DynamicKeyCommand(String name, KeyStroke key, GamePiece target, PieceI18nData data, PropertyChanger propChanger) {
      super(name, key, target, data);
      this.propChanger = propChanger;
    }

  }

  /**
   * 
   * Configure a single Dynamic Key Command line
   */
  protected static class DynamicKeyCommandConfigurer extends Configurer {
    protected HotKeyConfigurer keyConfig;
    protected PropertyChangerConfigurer propChangeConfig;
    protected StringConfigurer commandConfig;

    protected Box controls = null;
    protected DynamicProperty target;

    public DynamicKeyCommandConfigurer(DynamicProperty target) {
      super(target.getKey(), target.getKey(), new DynamicKeyCommand("Change value", KeyStroke.getKeyStroke('V', InputEvent.CTRL_MASK), target, target.getI18nData(), 
          new PropertyPrompt(target, "Change value of " + target.getKey())));
      commandConfig = new StringConfigurer(null, " Menu Command:  ", "Change value");
      keyConfig = new HotKeyConfigurer(null, " Key Command:  ", KeyStroke.getKeyStroke('V', InputEvent.CTRL_MASK));
      propChangeConfig = new PropertyChangerConfigurer(null, "Action:  ", target);
      propChangeConfig.setValue(new PropertyPrompt(target, " Change value of " + target.getKey()));

      PropertyChangeListener pl = new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          updateValue();
        }
      };
      commandConfig.addPropertyChangeListener(pl);
      keyConfig.addPropertyChangeListener(pl);
      propChangeConfig.addPropertyChangeListener(pl);
      this.target = target;
    }

    public String getValueString() {
      SequenceEncoder se = new SequenceEncoder(':');
      se.append(commandConfig.getValueString()).append(keyConfig.getValueString()).append(propChangeConfig.getValueString());
      return se.getValue();
    }

    public void setValue(Object value) {
      if (!noUpdate && value instanceof DynamicKeyCommand && commandConfig != null) {
        DynamicKeyCommand dkc = (DynamicKeyCommand) value;
        commandConfig.setValue(dkc.getName());
        keyConfig.setValue(dkc.getKeyStroke());
        propChangeConfig.setValue(dkc.propChanger);
      }
      super.setValue(value);
    }

    public DynamicKeyCommand getKeyCommand() {
      return (DynamicKeyCommand) getValue();
    }

    public void setValue(String s) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s == null ? "" : s, ':');
      commandConfig.setValue(sd.nextToken(""));
      keyConfig.setValue(sd.nextToken(""));
      propChangeConfig.setValue(sd.nextToken(""));
      updateValue();
    }

    public Component getControls() {
      if (controls == null) {
        buildControls();
      }
      return controls;
    }

    protected void updateValue() {
      noUpdate = true;
      setValue(new DynamicKeyCommand(commandConfig.getValueString(), (KeyStroke) keyConfig.getValue(), target, target.getI18nData(), propChangeConfig.getPropertyChanger()));
      noUpdate = false;
    }

    protected void buildControls() {
      controls = Box.createHorizontalBox();
      controls.add(commandConfig.getControls());
      controls.add(keyConfig.getControls());
      controls.add(propChangeConfig.getControls());
    }

  }

}
