/*
 *
 * Copyright (c) 2020 by Vassal Development Team
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
package VASSAL.configure;

import VASSAL.counters.GlobalCommandTarget;

import java.awt.Component;
import java.util.ArrayList;
import java.util.List;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;

import VASSAL.i18n.Resources;
import VASSAL.counters.TraitLayout;

import net.miginfocom.swing.MigLayout;

public class GlobalCommandTargetConfigurer extends Configurer {

  private JPanel controls;
  private BooleanConfigurer fastMatchLocationConfig;
  private BooleanConfigurer fastMatchPropertyConfig;
  private TranslatingStringEnumConfigurer targetTypeConfig;
  private JLabel targetTypeLabel;

  private FormattedExpressionConfigurer targetMapConfig;
  private JLabel targetMapLabel;
  private FormattedExpressionConfigurer targetBoardConfig;
  private JLabel targetBoardLabel;
  private FormattedExpressionConfigurer targetZoneConfig;
  private JLabel targetZoneLabel;
  private FormattedExpressionConfigurer targetLocationConfig;
  private JLabel targetLocationLabel;
  private FormattedExpressionConfigurer targetXConfig;
  private JLabel targetXLabel;
  private FormattedExpressionConfigurer targetYConfig;
  private JLabel targetYLabel;
  private FormattedExpressionConfigurer targetDeckConfig;
  private JLabel targetDeckLabel;

  private FormattedExpressionConfigurer targetPropertyConfig;
  private JLabel targetPropertyLabel;
  private CompareConfigurer targetCompareConfig;
  private JLabel targetCompareLabel;
  private FormattedExpressionConfigurer targetValueConfig;
  private JLabel targetValueLabel;

  // A local copy of the target used for configuring
  private final GlobalCommandTarget target;

  public GlobalCommandTargetConfigurer(String key, String name, GlobalCommandTarget target) {
    super(key, name, target);
    this.target = new GlobalCommandTarget(target);
  }

  public GlobalCommandTargetConfigurer(String key, String name) {
    this(key, name, null);
  }

  public GlobalCommandTargetConfigurer(GlobalCommandTarget target) {
    this(null, null, target);
  }

  public GlobalCommandTarget getTarget() {
    return (GlobalCommandTarget) getValue();
  }

  @Override
  public String getValueString() {
    return getTarget().encode();
  }

  @Override
  public void setValue(String s) {
    setValue(new GlobalCommandTarget(s));
  }

  @Override
  public void setValue(Object o) {
    if (!noUpdate && controls != null && o instanceof GlobalCommandTarget) {
      setFrozen(true);
      final GlobalCommandTarget t = (GlobalCommandTarget) o;
      fastMatchLocationConfig.setValue(t.isFastMatchLocation());
      targetTypeConfig.setValue(t.getTargetType());
      targetMapConfig.setValue(t.getTargetMap().getExpression());
      targetBoardConfig.setValue(t.getTargetBoard().getExpression());
      targetZoneConfig.setValue(t.getTargetZone().getExpression());
      targetLocationConfig.setValue(t.getTargetLocation().getExpression());
      targetXConfig.setValue(t.getTargetX().getExpression());
      targetYConfig.setValue(t.getTargetY().getExpression());
      targetDeckConfig.setValue(t.getTargetDeck().getExpression());
      fastMatchPropertyConfig.setValue(t.isFastMatchProperty());
      targetPropertyConfig.setValue(t.getTargetProperty().getExpression());
      targetCompareConfig.setValue(t.getTargetCompare());
      targetValueConfig.setValue(t.getTargetValue().getExpression());
      targetChanged();
      fastMatchPropertyChanged();
      setFrozen(false);
    }
    super.setValue(o);
  }

  /**
   * Freeze the Configurer from issuing PropertyChange Events.
   * Ensure the subsidiary Configurers are quiet also.
   *
   * @param val true to freeze
   */
  @Override
  public void setFrozen(boolean val) {
    super.setFrozen(val);
    fastMatchLocationConfig.setFrozen(val);
    targetTypeConfig.setFrozen(val);
    targetMapConfig.setFrozen(val);
    targetBoardConfig.setFrozen(val);
    targetZoneConfig.setFrozen(val);
    targetLocationConfig.setFrozen(val);
    targetXConfig.setFrozen(val);
    targetYConfig.setFrozen(val);
    targetDeckConfig.setFrozen(val);
    fastMatchPropertyConfig.setFrozen(val);
    targetPropertyConfig.setFrozen(val);
    targetCompareConfig.setFrozen(val);
    targetValueConfig.setFrozen(val);
  }

  @Override
  public Component getControls() {
    if (controls == null) {
      controls = new JPanel(new MigLayout("hidemode 3," + TraitLayout.STANDARD_GAPY, "[]rel[]rel[fill,grow]")); // NON-NLS

      fastMatchLocationConfig = new BooleanConfigurer(target.isFastMatchLocation());
      fastMatchLocationConfig.addPropertyChangeListener(evt -> update());
      controls.add(fastMatchLocationConfig.getControls());
      controls.add(new JLabel(Resources.getString("Editor.GlobalKeyCommand.by_location")), "wrap"); //NON-NLS

      final List<String> options = new ArrayList<>();
      final List<String> i18nKeys = new ArrayList<>();
      if (getTarget().getGKCtype() == GlobalCommandTarget.GKCtype.COUNTER) {
        options.add(GlobalCommandTarget.Target.CURSTACK.toString());
        options.add(GlobalCommandTarget.Target.CURMAP.toString());
        options.add(GlobalCommandTarget.Target.CURZONE.toString());
        options.add(GlobalCommandTarget.Target.CURLOC.toString());
        i18nKeys.add(GlobalCommandTarget.Target.CURSTACK.toTranslatedString());
        i18nKeys.add(GlobalCommandTarget.Target.CURMAP.toTranslatedString());
        i18nKeys.add(GlobalCommandTarget.Target.CURZONE.toTranslatedString());
        i18nKeys.add(GlobalCommandTarget.Target.CURLOC.toTranslatedString());
      }
      options.add(GlobalCommandTarget.Target.MAP.toString());
      options.add(GlobalCommandTarget.Target.ZONE.toString());
      options.add(GlobalCommandTarget.Target.LOCATION.toString());
      options.add(GlobalCommandTarget.Target.XY.toString());
      options.add(GlobalCommandTarget.Target.DECK.toString());
      i18nKeys.add(GlobalCommandTarget.Target.MAP.toTranslatedString());
      i18nKeys.add(GlobalCommandTarget.Target.ZONE.toTranslatedString());
      i18nKeys.add(GlobalCommandTarget.Target.LOCATION.toTranslatedString());
      i18nKeys.add(GlobalCommandTarget.Target.XY.toTranslatedString());
      i18nKeys.add(GlobalCommandTarget.Target.DECK.toTranslatedString());

      targetTypeConfig = new TranslatingStringEnumConfigurer(options, i18nKeys, target.getTargetType().toString());
      targetTypeConfig.addPropertyChangeListener(evt -> update());
      targetTypeLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.select_by"));
      controls.add(targetTypeLabel, "span 2"); // NON-NLS
      controls.add(targetTypeConfig.getControls(), "growx, wrap"); // NON-NLS

      targetMapConfig = new FormattedExpressionConfigurer(target.getTargetMap().getExpression());
      targetMapConfig.addPropertyChangeListener(evt -> update());
      targetMapConfig.setHintKey("Editor.GlobalKeyCommand.map_name_hint");
      targetMapLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.map_name"));
      controls.add(targetMapLabel, "span 2"); // NON-NLS
      controls.add(targetMapConfig.getControls(), "growx, wrap"); // NON-NLS

      targetBoardConfig = new FormattedExpressionConfigurer(target.getTargetBoard().getExpression());
      targetBoardConfig.addPropertyChangeListener(evt -> update());
      targetBoardConfig.setHintKey("Editor.GlobalKeyCommand.board_name_hint");
      targetBoardLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.board_name"));
      controls.add(targetBoardLabel, "span 2"); // NON-NLS
      controls.add(targetBoardConfig.getControls(), "growx, wrap"); // NON-NLS

      targetZoneConfig = new FormattedExpressionConfigurer(target.getTargetZone().getExpression());
      targetZoneConfig.addPropertyChangeListener(evt -> update());
      targetZoneConfig.setHintKey("Editor.GlobalKeyCommand.zone_name_hint");
      targetZoneLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.zone_name"));
      controls.add(targetZoneLabel, "span 2"); // NON-NLS
      controls.add(targetZoneConfig.getControls(), "growx, wrap"); // NON-NLS

      targetLocationConfig = new FormattedExpressionConfigurer(target.getTargetLocation().getExpression());
      targetLocationConfig.addPropertyChangeListener(evt -> update());
      targetLocationConfig.setHintKey("Editor.GlobalKeyCommand.location_name_hint");
      targetLocationLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.location_name"));
      controls.add(targetLocationLabel, "span 2"); // NON-NLS
      controls.add(targetLocationConfig.getControls(), "growx, wrap"); // NON-NLS

      targetXConfig = new FormattedExpressionConfigurer(target.getTargetX().getExpression());
      targetXConfig.addPropertyChangeListener(evt -> update());
      targetXLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.x_position"));
      controls.add(targetXLabel, "span 2"); // NON-NLS
      controls.add(targetXConfig.getControls(), "growx, wrap"); // NON-NLS

      targetYConfig = new FormattedExpressionConfigurer(target.getTargetY().getExpression());
      targetYConfig.addPropertyChangeListener(evt -> update());
      targetYLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.y_position"));
      controls.add(targetYLabel, "span 2"); // NON-NLS
      controls.add(targetYConfig.getControls(), "growY, wrap"); // NON-NLS

      targetDeckConfig = new FormattedExpressionConfigurer(target.getTargetDeck().getExpression());
      targetDeckConfig.addPropertyChangeListener(evt -> update());
      targetDeckConfig.setHintKey("Editor.GlobalKeyCommand.deck_name_hint");
      targetDeckLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.deck_name"));
      controls.add(targetDeckLabel, "span 2"); //NON-NLS
      controls.add(targetDeckConfig.getControls(), "growx, wrap"); //NON-NLS

      fastMatchPropertyConfig = new BooleanConfigurer(target.isFastMatchProperty());
      fastMatchPropertyConfig.addPropertyChangeListener(evt -> update());
      controls.add(fastMatchPropertyConfig.getControls());
      controls.add(new JLabel(Resources.getString("Editor.GlobalKeyCommand.by_property")), "wrap"); //NON-NLS

      targetPropertyConfig = new FormattedExpressionConfigurer(target.getTargetProperty().getExpression());
      targetPropertyConfig.addPropertyChangeListener(evt -> update());
      targetPropertyConfig.setHintKey("Editor.GlobalKeyCommand.property_name_hint");
      targetPropertyLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.property_name"));
      controls.add(targetPropertyLabel, "span 2"); // NON-NLS
      controls.add(targetPropertyConfig.getControls(), "wrap"); // NON-NLS

      targetCompareConfig = new CompareConfigurer();
      targetCompareConfig.setValue(getTarget().getTargetCompare().getSymbol());
      targetCompareConfig.addPropertyChangeListener(evt -> update());
      targetCompareLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.property_compare"));
      controls.add(targetCompareLabel, "span 2"); //NON-NLS
      controls.add(targetCompareConfig.getControls(), "wrap"); //NON-NLS

      targetValueConfig = new FormattedExpressionConfigurer(target.getTargetValue().getExpression());
      targetValueConfig.addPropertyChangeListener(evt -> update());
      targetValueConfig.setHintKey("Editor.GlobalKeyCommand.property_value_hint");
      targetValueLabel = new JLabel(Resources.getString("Editor.GlobalKeyCommand.property_value"));
      controls.add(targetValueLabel, "span 2"); // NON-NLS
      controls.add(targetValueConfig.getControls(), "wrap"); // NON-NLS

      targetChanged();
      fastMatchPropertyChanged();

      controls.setBorder(BorderFactory.createEtchedBorder());

      if (getName() != null && ! getName().isEmpty()) {
        final JPanel controls2 = controls;
        controls = new JPanel(new MigLayout("ins 0", "[]rel[fill,grow]")); // NON-NLS
        controls.add(new JLabel(getName()));
        controls.add(controls2, "growx"); // NON-NLS
      }
    }

    return controls;
  }

  private void targetChanged() {

    final boolean fastMatchLocation = fastMatchLocationConfig.getValueBoolean();
    target.setFastMatchLocation(fastMatchLocation);

    target.setTargetType(targetTypeConfig.getValueString());
    final GlobalCommandTarget.Target targetType = target.getTargetType();

    targetTypeConfig.getControls().setVisible(fastMatchLocation);
    targetTypeLabel.setVisible(fastMatchLocation);

    final boolean mapVisible = fastMatchLocation && (
      targetType.equals(GlobalCommandTarget.Target.MAP) ||
        targetType.equals(GlobalCommandTarget.Target.ZONE) ||
        targetType.equals(GlobalCommandTarget.Target.LOCATION) ||
        targetType.equals(GlobalCommandTarget.Target.XY));

    targetMapConfig.getControls().setVisible(fastMatchLocation && mapVisible);
    targetMapLabel.setVisible(fastMatchLocation && mapVisible);
    targetBoardConfig.getControls().setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.XY));
    targetBoardLabel.setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.XY));
    targetZoneConfig.getControls().setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.ZONE));
    targetZoneLabel.setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.ZONE));
    targetLocationConfig.getControls().setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.LOCATION));
    targetLocationLabel.setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.LOCATION));
    targetXConfig.getControls().setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.XY));
    targetXLabel.setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.XY));
    targetYConfig.getControls().setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.XY));
    targetYLabel.setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.XY));
    targetDeckLabel.setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.DECK));
    targetDeckConfig.getControls().setVisible(fastMatchLocation && targetType.equals(GlobalCommandTarget.Target.DECK));
    repack();
  }

  private void fastMatchPropertyChanged() {

    target.setFastMatchProperty(fastMatchPropertyConfig.getValueBoolean());
    targetPropertyConfig.getControls().setVisible(target.isFastMatchProperty());
    targetPropertyLabel.setVisible(target.isFastMatchProperty());
    targetValueConfig.getControls().setVisible(target.isFastMatchProperty());
    targetValueLabel.setVisible(target.isFastMatchProperty());
    targetCompareConfig.getControls().setVisible(target.isFastMatchProperty());
    targetCompareLabel.setVisible(target.isFastMatchProperty());
    repack();
  }

  private void update() {
    // Wait until the Configurer has been completely built before trying to update anything
    if (target == null) {
      return;
    }

    // Rebuild the local copy of the target, then set into the value to create a correct property change event
    targetChanged();
    fastMatchPropertyChanged();

    target.setTargetValue(targetValueConfig.getValueString());
    target.setTargetCompare(GlobalCommandTarget.CompareMode.whichSymbol(targetCompareConfig.getValueString()));
    target.setTargetProperty(targetPropertyConfig.getValueString());
    target.setTargetDeck(targetDeckConfig.getValueString());
    target.setTargetY(targetYConfig.getValueString());
    target.setTargetX(targetXConfig.getValueString());
    target.setTargetLocation(targetLocationConfig.getValueString());
    target.setTargetZone(targetZoneConfig.getValueString());
    target.setTargetBoard(targetBoardConfig.getValueString());
    target.setTargetMap(targetMapConfig.getValueString());

    setValue(target);
  }

  @Override
  public void repack() {
    repack(controls);
  }

  /**
   * Happy little Configurer class for the Compare Modes
   */
  private static class CompareConfigurer extends StringEnumConfigurer {
    CompareConfigurer() {
      super(null, null, GlobalCommandTarget.CompareMode.getSymbols());
    }
  }
}
