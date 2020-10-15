/*
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

import VASSAL.build.GameModule;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.EnumeratedPropertyPrompt;
import VASSAL.build.module.properties.IncrementProperty;
import VASSAL.build.module.properties.PropertyChanger;
import VASSAL.build.module.properties.PropertyChangerConfigurer;
import VASSAL.build.module.properties.PropertyPrompt;
import VASSAL.build.module.properties.PropertySetter;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.ListConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.script.expression.Expression;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Point;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.KeyStroke;

import net.miginfocom.swing.MigLayout;

import org.apache.commons.lang3.StringUtils;

/**
 * Trait that contains a property accessible via getProperty() and updateable
 * dynamically via key commands
 *
 * @author rkinney
 *
 */
public class DynamicProperty extends Decorator implements TranslatablePiece, PropertyPrompt.DialogParent, PropertyChangerConfigurer.Constraints {

  public static final String ID = "PROP;"; // NON-NLS

  protected String value = "";

  protected String key;
  protected boolean numeric;
  protected int minValue;
  protected int maxValue;
  protected boolean wrap;
  protected FormattedString format = new FormattedString();

  protected DynamicKeyCommand[] keyCommands;
  protected KeyCommand[] menuCommands;

  protected ListConfigurer keyCommandListConfig;

  public DynamicProperty() {
    this(ID, null);
  }

  public DynamicProperty(String type, GamePiece p) {
    setInner(p);
    keyCommandListConfig = new ListConfigurer(null, Resources.getString("Editor.DynamicProperty.commands")) {
      @Override
      protected Configurer buildChildConfigurer() {
        return new DynamicKeyCommandConfigurer(DynamicProperty.this);
      }
    };
    mySetType(type);
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

  @Override
  public void draw(java.awt.Graphics g, int x, int y, java.awt.Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  public java.awt.Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public Object getProperty(Object key) {
    if (key.equals(getKey())) {
      return getValue();
    }
    return super.getProperty(key);
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    if (key.equals(getKey())) {
      return getValue();
    }
    else {
      return super.getLocalizedProperty(key);
    }
  }

  @Override
  public void setProperty(Object key, Object value) {
    if (key.equals(getKey())) {
      setValue(null == value ? null : value.toString());
    }
    else {
      super.setProperty(key, value);
    }
  }

  @Override
  public String myGetState() {
    return getValue();
  }

  @Override
  public Component getComponent() {
    return getMap() != null ? getMap().getView().getTopLevelAncestor() : GameModule.getGameModule().getPlayerWindow();
  }

  @Override
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

    value = formatValue(value);

    // If the property has changed the layer to which this piece belongs,
    // re-insert it into the map.
    // No need to re-insert pieces in Decks, it causes problems if they are NO_STACK
    if (map != null && ! (getParent() instanceof Deck)) {

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
          map.placeOrMerge(parent, pos);
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

  private String formatValue(String value) {
    format.setFormat(value);
    return format.getText(Decorator.getOutermost(this));
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(key);
    se.append(encodeConstraints());
    se.append(keyCommandListConfig.getValueString());
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return menuCommands;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    final ChangeTracker tracker = new ChangeTracker(this);
    for (DynamicKeyCommand dkc : keyCommands) {
      if (dkc.matches(stroke)) {
        setValue(dkc.propChanger.getNewValue(value));
      }
    }

    return tracker.getChangeCommand();
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.DynamicProperty.trait_description", getKey());
  }


  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.stream(keyCommands).map(KeyCommand::getNamedKeyStroke).collect(Collectors.toList());
  }

  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    List<String> l = new ArrayList<>();
    l.add(value); // We'll treat the at-start value of the property as a quasi-expression

    for (DynamicKeyCommand dkc : keyCommands) {
      PropertyChanger propChanger = dkc.getPropChanger();
      if (propChanger == null) {
        continue;
      }

      if (propChanger instanceof IncrementProperty) {
        l.add(((IncrementProperty)propChanger).getIncrement());
      }
      else if (propChanger instanceof PropertySetter) {
        l.add(((PropertySetter)propChanger).getRawValue());
      }
      else if (propChanger instanceof PropertyPrompt) {
        PropertyPrompt pp = (PropertyPrompt)propChanger;
        l.add(pp.getPrompt());
        if (pp instanceof EnumeratedPropertyPrompt) {
          Expression[] ve = ((EnumeratedPropertyPrompt) pp).getValueExpressions();
          for (Expression e : ve) {
            if (e == null) {
              continue;
            }
            l.add(e.getExpression());
          }
        }
      }
    }
    return l;
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    List<String> l = new ArrayList<>();
    for (DynamicKeyCommand dkc : keyCommands) {
      if (StringUtils.isEmpty(dkc.getName())) {
        continue;
      }
      l.add(dkc.getName());
    }
    return l;
  }

  /**
   * @return a list of any Property Names referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getPropertyList() {
    return Arrays.asList(key);
  }


  @Override
  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("DynamicProperty.html"); // NON-NLS
  }

  @Override
  public int getMaximumValue() {
    return maxValue;
  }

  @Override
  public int getMinimumValue() {
    return minValue;
  }

  @Override
  public boolean isNumeric() {
    return numeric;
  }

  @Override
  public boolean isWrap() {
    return wrap;
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<>();
    l.add(key);
    return l;
  }

  @Override
  public PropertySource getPropertySource() {
    return Decorator.getOutermost(this);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    final String[] commandNames = new String[menuCommands.length];
    final String[] commandDescs = new String[menuCommands.length];

    for (int i = 0; i < menuCommands.length; i++) {
      commandNames[i] = menuCommands[i].getName();
      commandDescs[i] = Resources.getString("Editor.DynamicProperty.command_descrption", key, i);
    }

    return getI18nData(commandNames, commandDescs);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof DynamicProperty)) return false;
    DynamicProperty c = (DynamicProperty) o;
    if (! Objects.equals(encodeConstraints(), c.encodeConstraints())) return false;
    if (! Objects.equals(keyCommandListConfig.getValueString(), keyCommandListConfig.getValueString())) return false;
    if (! Objects.equals(key, c.key)) return false;
    return Objects.equals(value, c.value);
  }

  protected static class Ed implements PieceEditor {
    protected StringConfigurer nameConfig;
    protected StringConfigurer initialValueConfig;
    protected BooleanConfigurer numericConfig;
    protected IntConfigurer minConfig;
    protected IntConfigurer maxConfig;
    protected BooleanConfigurer wrapConfig;
    protected JLabel minLabel;
    protected JLabel maxLabel;
    protected JLabel wrapLabel;
    protected ListConfigurer keyCommandListConfig;
    protected TraitConfigPanel controls;

    public Ed(final DynamicProperty m) {
      keyCommandListConfig = new ListConfigurer(null, Resources.getString("Editor.DynamicProperty.key_commands")) {
        @Override
        protected Configurer buildChildConfigurer() {
          return new DynamicKeyCommandConfigurer(m);
        }
      };
      keyCommandListConfig.setValue(
        new ArrayList<>(Arrays.asList(m.keyCommands)));
      PropertyChangeListener l = evt -> {
        boolean isNumeric = numericConfig.booleanValue();
        minConfig.getControls().setVisible(isNumeric);
        minLabel.setVisible(isNumeric);
        maxConfig.getControls().setVisible(isNumeric);
        maxLabel.setVisible(isNumeric);
        wrapConfig.getControls().setVisible(isNumeric);
        wrapLabel.setVisible(isNumeric);
        repack(keyCommandListConfig);
      };
      controls = new TraitConfigPanel();

      nameConfig = new StringConfigurer(m.getKey());
      controls.add("Editor.DynamicProperty.property_name", nameConfig);

      initialValueConfig = new StringConfigurer(m.getValue());
      controls.add("Editor.DynamicProperty.initial_value", initialValueConfig);

      numericConfig = new BooleanConfigurer(m.isNumeric());
      controls.add("Editor.DynamicProperty.is_numeric", numericConfig);

      minLabel = new JLabel(Resources.getString("Editor.DynamicProperty.minimum_value"));
      minConfig = new IntConfigurer(m.getMinimumValue());
      controls.add(minLabel, minConfig);

      maxLabel = new JLabel(Resources.getString("Editor.DynamicProperty.maximum_value"));
      maxConfig = new IntConfigurer(m.getMaximumValue());
      controls.add(maxLabel, maxConfig);

      wrapLabel = new JLabel(Resources.getString("Editor.DynamicProperty.wrap"));
      wrapConfig = new BooleanConfigurer(m.isWrap());
      controls.add(wrapLabel, wrapConfig, "wrap"); // NON-NLS

      controls.add(keyCommandListConfig.getControls(), "span 2,growx"); // NON-NLS

      numericConfig.addPropertyChangeListener(l);
      numericConfig.fireUpdate();
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
        .append(wrapConfig.getValueString()).getValue();
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(nameConfig.getValueString());
      se.append(encodeConstraints());
      se.append(keyCommandListConfig.getValueString());
      return ID + se.getValue();
    }

    @Override
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

    protected PropertyChanger propChanger;

    public DynamicKeyCommand(String name, NamedKeyStroke key, GamePiece target, TranslatablePiece i18nPiece, PropertyChanger propChanger) {
      super(name, key, target, i18nPiece);
      this.propChanger = propChanger;
    }

    public PropertyChanger getPropChanger() {
      return propChanger;
    }
  }

  /**
   *
   * Configure a single Dynamic Key Command line
   */
  protected static class DynamicKeyCommandConfigurer extends Configurer {
    protected final NamedHotKeyConfigurer keyConfig;
    protected PropertyChangerConfigurer propChangeConfig;
    protected StringConfigurer commandConfig;
    protected JPanel controls;
    protected DynamicProperty target;

    public DynamicKeyCommandConfigurer(DynamicProperty target) {
      super(target.getKey(), target.getKey(), new DynamicKeyCommand(Resources.getString("Editor.DynamicProperty.change_value"), NamedKeyStroke.getNamedKeyStroke('V', InputEvent.CTRL_DOWN_MASK), Decorator.getOutermost(target), target,
          new PropertyPrompt(target, Resources.getString("Editor.DynamicProperty.change_value_of", target.getKey()))));


      commandConfig = new StringConfigurer(Resources.getString("Editor.DynamicProperty.change_value"));
      keyConfig = new NamedHotKeyConfigurer(NamedKeyStroke.getNamedKeyStroke('V', InputEvent.CTRL_DOWN_MASK));
      propChangeConfig = new PropertyChangerConfigurer(null, target.getKey(), target);
      propChangeConfig.setValue(new PropertyPrompt(target, Resources.getString("Editor.DynamicProperty.change_value_of", target.getKey())));

      PropertyChangeListener pl = e -> updateValue();
      commandConfig.addPropertyChangeListener(pl);
      keyConfig.addPropertyChangeListener(pl);
      propChangeConfig.addPropertyChangeListener(pl);
      this.target = target;
    }

    @Override
    public String getValueString() {
      SequenceEncoder se = new SequenceEncoder(':');
      se.append(commandConfig.getValueString()).append(keyConfig.getValueString()).append(propChangeConfig.getValueString());
      return se.getValue();
    }

    @Override
    public void setValue(Object value) {
      if (!noUpdate && value instanceof DynamicKeyCommand && commandConfig != null) {
        DynamicKeyCommand dkc = (DynamicKeyCommand) value;
        commandConfig.setValue(dkc.getName());
        keyConfig.setValue(dkc.getNamedKeyStroke());
        propChangeConfig.setValue(dkc.propChanger);
      }
      super.setValue(value);
    }

    public DynamicKeyCommand getKeyCommand() {
      return (DynamicKeyCommand) getValue();
    }

    @Override
    public void setValue(String s) {
      SequenceEncoder.Decoder sd = new SequenceEncoder.Decoder(s == null ? "" : s, ':');
      commandConfig.setValue(sd.nextToken(""));
      keyConfig.setValue(sd.nextNamedKeyStroke(null));
      propChangeConfig.setValue(sd.nextToken(""));
      updateValue();
    }

    @Override
    public Component getControls() {
      if (controls == null) {
        buildControls();
      }
      return controls;
    }

    protected void updateValue() {
      noUpdate = true;
      setValue(new DynamicKeyCommand(commandConfig.getValueString(), keyConfig.getValueNamedKeyStroke(), target, target, propChangeConfig.getPropertyChanger()));
      noUpdate = false;
      repack();
    }

    protected void buildControls() {
      controls = new JPanel(new MigLayout("ins panel,gapy 2,hidemode 3", "[]rel[][]rel[]")); // NON-NLS
      controls.setBorder(BorderFactory.createEtchedBorder());
      controls.add(new JLabel(Resources.getString("Editor.menu_command")));
      controls.add(commandConfig.getControls(), "grow"); // NON-NLS

      controls.add(new JLabel(Resources.getString("Editor.keyboard_command")));
      controls.add(keyConfig.getControls(), "grow,wrap"); // NON-NLS

      controls.add(propChangeConfig.getTypeLabel());
      controls.add(propChangeConfig.getTypeControls(), "grow"); // NON-NLS

      controls.add(propChangeConfig.getChangerLabel());
      controls.add(propChangeConfig.getChangerControls(), "grow,wrap"); // NON-NLS
      controls.add(propChangeConfig.getValuesControls(), "grow,span 4"); // NON-NLS
    }
  }
}
