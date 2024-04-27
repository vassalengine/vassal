/*
 *
 * Copyright (c) 2000-2023 by The Vassal Development Team, Brent Easton, Rodney Kinney, Brian Reynolds
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

import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.map.MassKeyCommand;
import VASSAL.build.module.properties.EnumeratedPropertyPrompt;
import VASSAL.build.module.properties.IncrementProperty;
import VASSAL.build.module.properties.PropertyChanger;
import VASSAL.build.module.properties.PropertyPrompt;
import VASSAL.build.module.properties.PropertySetter;
import VASSAL.build.module.properties.RemoteEnumeratedPropertyPrompt;
import VASSAL.build.module.properties.RemoteIncrementProperty;
import VASSAL.build.module.properties.RemotePropertyChanger;
import VASSAL.build.module.properties.RemotePropertyPrompt;
import VASSAL.build.module.properties.RemotePropertySetter;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.command.NullCommand;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.DynamicKeyCommandListConfigurer;
import VASSAL.configure.FormattedExpressionConfigurer;
import VASSAL.configure.GlobalCommandTargetConfigurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.PropertyExpression;
import VASSAL.configure.PropertyExpressionConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TranslatingStringEnumConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.script.expression.AuditTrail;
import VASSAL.tools.FormattedString;
import VASSAL.tools.RecursionLimiter;
import VASSAL.tools.SequenceEncoder;
import org.apache.commons.lang3.StringUtils;

import javax.swing.JLabel;
import javax.swing.KeyStroke;
import java.awt.Component;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 *
 * @author Brian Reynolds, Brent Easton
 *
 * A trait that allows counters to manipulate the value of the Dynamic Properties of OTHER pieces.
 * Combines the Property manipulation functionality of DynamicProperty with the searching function of Global Key Commands
 */
public class SetPieceProperty extends DynamicProperty implements RecursionLimiter.Loopable {
  protected PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);
  public static final String ID = "setpieceprop;"; // NON-NLS
  protected String description;

  protected GlobalCommandTarget target;
  protected GlobalSetter globalSetter;
  protected PropertyExpression propertiesFilter;
  protected boolean restrictRange;
  protected boolean fixedRange;
  protected int range;
  protected String rangeProperty = "";
  protected boolean overrideConstraints;

  protected Decorator dec;

  public SetPieceProperty() {
    this(ID, null);
  }

  public SetPieceProperty(String type, GamePiece p) {
    super(type, p);
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.SetPieceProperty.trait_description", key, description) + getCommandsList();
  }

  @Override
  public String getBaseDescription() {
    return Resources.getString("Editor.SetPieceProperty.trait_description");
  }

  @Override
  public String getDescriptionField() {
    return description;
  }

  @Override
  public void mySetType(String s) {

    // Because DynamicProperty's constructor calls mySetType, we have to put these initializations here rather than w/ the declarations
    if (target == null) {
      target = new GlobalCommandTarget(GlobalCommandTarget.GKCtype.COUNTER);
    }

    if (globalSetter == null) {
      globalSetter = new GlobalSetter(this);
    }

    if (propertiesFilter == null) {
      propertiesFilter = new PropertyExpression();
    }

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

    target.decode(sd.nextToken(""));
    target.setGKCtype(GlobalCommandTarget.GKCtype.COUNTER);
    target.setCurPiece(this);

    propertiesFilter.setExpression(sd.nextToken(""));
    restrictRange = sd.nextBoolean(false);
    range = sd.nextInt(1);
    fixedRange = sd.nextBoolean(true);
    rangeProperty = sd.nextToken("");
    globalSetter.setSelectFromDeckExpression(sd.nextToken("-1"));
    overrideConstraints = sd.nextBoolean(false);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(key);
    se.append(encodeConstraints());
    se.append(keyCommandListConfig.getValueString());
    se.append(description);
    se.append(target.encode());
    se.append(propertiesFilter);
    se.append(restrictRange);
    se.append(range);
    se.append(fixedRange);
    se.append(rangeProperty);
    se.append(globalSetter.getSelectFromDeckExpression());
    se.append(overrideConstraints);

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
  public List<String> getPropertyNames() {
    // This is not a real Property source, so don't return the property name
    return Collections.emptyList();
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("SetPieceProperty.html"); // NON-NLS
  }

  // These are used to provide makeSetTargetCommand with the context of what we're doing up in myKeyEvent
  private String propName;
  private RemotePropertyChanger changer = null;
  private String newValue = null;
  private boolean cancelled;
  private DynamicProperty currentDPTarget = null;

  /**
   * Our filter has found a matching piece. Check it for a matching Dynamic Property and if found apply our setter.
   * @param p Piece to check for matching Dynamic Properties
   * @return command to reproduce any work we do here
   */
  public Command makeSetTargetCommand(GamePiece p) {

    Command comm = new NullCommand();

    // User pressed Cancel?
    if (cancelled) return comm;

    while (p instanceof Decorator) {
      // Compare class directly, not via instanceof as we need to exclude all subclasses
      if (p.getClass() == DynamicProperty.class) {
        final DynamicProperty currentDP = (DynamicProperty)p;

        if (propName.equals(currentDP.getKey())) {
          if ((newValue == null) || !(changer instanceof PropertyPrompt)) {
            currentDPTarget = currentDP;
            final String userValue = changer.getNewValue(currentDP, this, getOutermost(this));

            // A changer only returns null if the user pressed cancel in a Prompt or List dialog.
            // This is callback, so just record that cancel has been pressed and get out of here.
            if (userValue == null) {
              cancelled = true;
              return comm;
            }
            newValue = userValue;
          }

          final ChangeTracker ct = new ChangeTracker(currentDP);

          currentDP.setValue(newValue);

          comm = comm.append(ct.getChangeCommand());
          break;  // No need to search further, any deeper DP's of the same name are shadowed by this one.
        }
      }

      p = ((Decorator) p).getInner();
    }

    return comm;
  }

  /*
   * Locate the correct property/properties to adjust and update value(s).
   * $xxxx$ names are allowed in any of the property name, attachment name, and attachment index fields
   * Blank fields for attachment name and/or index count as wild cards
   */
  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    final GamePiece outer = getOutermost(this);
    globalSetter.setPropertySource(outer); // Doing this here ensures trait is linked into GamePiece before finding source

    Command comm = new NullCommand();
    for (final DynamicKeyCommand keyCommand : keyCommands) {
      if (keyCommand.matches(stroke)) {
        // Evaluate the property name expression & initialize the context information for makeSetTargetCommand
        propName = (new FormattedString(key)).getText(getOutermost(this), this, "Editor.SetPieceProperty.property_name");
        changer  = createRemotePropertyChanger(keyCommand.propChanger);
        newValue = null;
        cancelled = false;

        // Make piece properties filter
        final AuditTrail audit = AuditTrail.create(this, propertiesFilter.getExpression(), Resources.getString("Editor.SetPieceProperty.matching_properties"));
        PieceFilter filter = propertiesFilter.getFilter(outer, this, audit);

        // Make a range filter if applicable
        if (restrictRange) {
          int r = range;
          if (!fixedRange) {
            final String rangeValue = (String) getOutermost(this).getProperty(rangeProperty);
            try {
              r = Integer.parseInt(rangeValue);
            }
            catch (NumberFormatException e) {
              reportDataError(this, Resources.getString("Error.non_number_error"), "range[" + rangeProperty + "]=" + rangeValue, e); // NON-NLS
            }
          }
          filter = new BooleanAndPieceFilter(filter, new RangeFilter(getMap(), getPosition(), r));
        }

        // Now apply our filter globally -- we will get callbacks to our makeSetTargetCommand method
        comm = comm.append(globalSetter.apply(Map.getMapList().toArray(new Map[0]), filter, target, audit));

        return comm;
      }
    }
    return comm;
  }

  /**
   * Create a Remote version of a PropertyChanger that will operate sensibly on a DP in a remote piece
   * @param changer Local propertyChanger
   * @return        Remote PropertyChanger
   */
  public RemotePropertyChanger createRemotePropertyChanger(PropertyChanger changer) {
    if (changer instanceof PropertySetter) {
      return new RemotePropertySetter((PropertySetter) changer);
    }
    else if (changer instanceof IncrementProperty) {
      return new RemoteIncrementProperty((IncrementProperty) changer);
    }
    else if (changer instanceof EnumeratedPropertyPrompt) {
      return new RemoteEnumeratedPropertyPrompt((EnumeratedPropertyPrompt) changer);
    }
    else if (changer instanceof PropertyPrompt) {
      return new RemotePropertyPrompt((PropertyPrompt) changer);
    }
    return null;
  }

  //
  // The following 4 overrides are called when the PropertyChanger is activated to determine
  // the numeric constraints to follow when applying the change to a specific remote DP.
  // If this trait is not overriding the numeric constraints, then use the constraints of the
  // target DP.
  // NOTE: Constraints.getPropertySource() is not overridden, it is handled separately by the
  //       Remote Property Setters.
  //
  @Override
  public int getMaximumValue() {
    return (overrideConstraints || currentDPTarget == null) ? super.getMaximumValue() : currentDPTarget.getMaximumValue();
  }

  @Override
  public int getMinimumValue() {
    return (overrideConstraints || currentDPTarget == null)  ? super.getMinimumValue() : currentDPTarget.getMinimumValue();
  }

  @Override
  public boolean isNumeric() {
    return (overrideConstraints || currentDPTarget == null)  ? super.isNumeric() : currentDPTarget.isNumeric();
  }

  @Override
  public boolean isWrap() {
    return (overrideConstraints || currentDPTarget == null)  ? super.isWrap() : currentDPTarget.isWrap();
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  @SuppressWarnings("PMD.SimplifyBooleanReturns")
  public boolean testEquals(Object o) {
    if (! (o instanceof SetPieceProperty)) return false;
    final SetPieceProperty c = (SetPieceProperty) o;

    if (! Objects.equals(key, c.key)) return false;
    if (! Objects.equals(encodeConstraints(), c.encodeConstraints())) return false;
    if (! Objects.equals(keyCommandListConfig.getValueString(), c.keyCommandListConfig.getValueString())) return false;
    if (! Objects.equals(description, c.description)) return false;
    if (! Objects.equals(target, c.target)) return false;
    if (! Objects.equals(propertiesFilter, c.propertiesFilter)) return false;
    if (! Objects.equals(restrictRange, c.restrictRange)) return false;
    if (! Objects.equals(range, c.range)) return false;
    if (! Objects.equals(fixedRange, c.fixedRange)) return false;
    if (! Objects.equals(rangeProperty, c.rangeProperty)) return false;
    if (! Objects.equals(overrideConstraints, c.overrideConstraints)) return false;
    return Objects.equals(globalSetter.getSelectFromDeckExpression(), c.globalSetter.getSelectFromDeckExpression());
  }

  protected static class Ed implements PieceEditor {
    protected StringConfigurer descConfig;
    protected FormattedExpressionConfigurer nameConfig;
    protected BooleanConfigurer overrideConfig;
    protected JLabel numericLabel;
    protected BooleanConfigurer numericConfig;
    protected JLabel minLabel;
    protected IntConfigurer minConfig;
    protected JLabel maxLabel;
    protected IntConfigurer maxConfig;
    protected JLabel wrapLabel;
    protected BooleanConfigurer wrapConfig;
    protected DynamicKeyCommandListConfigurer keyCommandListConfig;
    protected TranslatingStringEnumConfigurer levelConfig;
    protected FormattedExpressionConfigurer attachNameConfig;
    protected FormattedExpressionConfigurer attachIndexConfig;
    protected TraitConfigPanel controls;

    protected GlobalCommandTargetConfigurer targetConfig;
    protected PropertyExpressionConfigurer propertyMatch;
    protected PropertyExpressionConfigurer clearMatchingMatch;

    protected MassKeyCommand.DeckPolicyConfig deckPolicy;
    protected BooleanConfigurer restrictRange;
    protected BooleanConfigurer fixedRange;
    protected JLabel fixedRangeLabel;
    protected IntConfigurer range;
    protected JLabel rangeLabel;
    protected StringConfigurer rangeProperty;
    protected JLabel rangePropertyLabel;
    protected JLabel targetLabel;
    protected JLabel propertyLabel;
    protected JLabel deckLabel;
    protected JLabel restrictLabel;

    public Ed(final SetPieceProperty m) {
      keyCommandListConfig = new DynamicKeyCommandListConfigurer(null, Resources.getString("Editor.DynamicProperty.commands"), m);
      keyCommandListConfig.setValue(new ArrayList<>(Arrays.asList(m.keyCommands)));

      controls = new TraitConfigPanel();

      descConfig = new StringConfigurer(m.description);
      descConfig.setHintKey("Editor.description_hint");
      controls.add("Editor.description_label", descConfig);

      nameConfig = new FormattedExpressionConfigurer(m.getKey(), (EditablePiece) m);
      nameConfig.setHintKey("Editor.SetPieceProperty.property_name_hint");
      controls.add("Editor.SetPieceProperty.property_name", nameConfig);

      overrideConfig = new BooleanConfigurer(m.overrideConstraints);
      controls.add("Editor.SetPieceProperty.override_constraints", overrideConfig);

      numericLabel = new JLabel(Resources.getString("Editor.DynamicProperty.is_numeric"));
      numericConfig = new BooleanConfigurer(m.isNumeric());
      controls.add(numericLabel, numericConfig);

      minLabel = new JLabel(Resources.getString("Editor.GlobalProperty.minimum_value"));
      minConfig = new IntConfigurer(m.getMinimumValue());
      controls.add(minLabel, minConfig);

      maxLabel = new JLabel(Resources.getString("Editor.GlobalProperty.maximum_value"));
      maxConfig = new IntConfigurer(m.getMaximumValue());
      controls.add(maxLabel, maxConfig);

      wrapLabel  = new JLabel(Resources.getString("Editor.DynamicProperty.wrap"));
      wrapConfig = new BooleanConfigurer(m.isWrap());
      controls.add(wrapLabel, wrapConfig);

      targetConfig = new GlobalCommandTargetConfigurer(m.target, m);
      targetLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.pre_select"));
      controls.add(targetLabel, targetConfig);

      propertyMatch = new PropertyExpressionConfigurer(m.propertiesFilter, m);
      propertyLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.matching_properties"));
      controls.add(propertyLabel, propertyMatch);

      deckPolicy = new MassKeyCommand.DeckPolicyConfig(false, m);
      deckPolicy.setValue(m.globalSetter.getSelectFromDeckExpression());
      deckLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.deck_policy"));
      controls.add(deckLabel, deckPolicy);

      restrictRange = new BooleanConfigurer(m.restrictRange);
      restrictLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.restrict_range"));
      controls.add(restrictLabel, restrictRange);

      fixedRange = new BooleanConfigurer(m.fixedRange);
      fixedRangeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.fixed_range"));
      controls.add(fixedRangeLabel, fixedRange);

      range = new IntConfigurer(m.range);
      rangeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range"));
      controls.add(rangeLabel, range);

      rangeProperty = new StringConfigurer(m.rangeProperty);
      rangeProperty.setHintKey("Editor.GlobalKeyCommand.range_property_hint");
      rangePropertyLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.range_property"));
      controls.add(rangePropertyLabel, rangeProperty);

      controls.add("Editor.DynamicProperty.key_commands", keyCommandListConfig);

      final PropertyChangeListener nl = evt -> {
        final boolean isOverride = overrideConfig.booleanValue();
        final boolean isNumeric = numericConfig.booleanValue();
        numericLabel.setVisible(isOverride);
        numericConfig.getControls().setVisible(isOverride);
        minConfig.getControls().setVisible(isOverride && isNumeric);
        minLabel.setVisible(isOverride && isNumeric);
        maxConfig.getControls().setVisible(isOverride && isNumeric);
        maxLabel.setVisible(isOverride && isNumeric);
        wrapConfig.getControls().setVisible(isOverride && isNumeric);
        wrapLabel.setVisible(isOverride && isNumeric);
        keyCommandListConfig.repack();
      };
      overrideConfig.addPropertyChangeListener(nl);
      numericConfig.addPropertyChangeListener(nl);

      numericConfig.fireUpdate();

      final PropertyChangeListener pl = evt -> {

        final boolean isRange = Boolean.TRUE.equals(restrictRange.getValue());
        final boolean isFixed = Boolean.TRUE.equals(fixedRange.getValue());

        range.getControls().setVisible(isRange && isFixed);
        rangeLabel.setVisible(isRange && isFixed);
        fixedRange.getControls().setVisible(isRange);
        fixedRangeLabel.setVisible(isRange);
        rangeProperty.getControls().setVisible(isRange && !isFixed);
        rangePropertyLabel.setVisible(isRange && !isFixed);

        repack(range);
      };

      restrictRange.addPropertyChangeListener(pl);
      fixedRange.addPropertyChangeListener(pl);
      fixedRange.fireUpdate();

      pl.propertyChange(null);
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

      se.append(targetConfig.getValueString());
      se.append(propertyMatch.getValueString());
      se.append(restrictRange.getValueString());
      se.append(range.getValueString());
      se.append(fixedRange.getValueString());
      se.append(rangeProperty.getValueString());
      se.append(deckPolicy.getValueString());
      se.append(overrideConfig.getValueString());

      return ID + se.getValue();
    }

    @Override
    public String getState() {
      return "";
    }
  }
}
