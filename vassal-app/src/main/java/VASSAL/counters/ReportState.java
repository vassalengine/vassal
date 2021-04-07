/*
 *
 * Copyright (c) 2000-2013 by Rodney Kinney, Brent Easton
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
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.BooleanConfigurer;
import VASSAL.configure.NamedKeyStrokeArrayConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.search.HTMLImageFinder;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ProblemDialog;
import VASSAL.tools.SequenceEncoder;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.InputEvent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.JLabel;
import javax.swing.KeyStroke;

import org.apache.commons.lang3.ArrayUtils;

/**
 * A GamePiece with this trait will echo the piece's current name when any of a given key commands are pressed
 * (and after they take effect)
 */
public class ReportState extends Decorator implements TranslatablePiece {
  public static final String ID = "report;"; // NON-NLS
  protected NamedKeyStroke[] keys;
  protected FormattedString format = new FormattedString();
  protected String reportFormat;
  protected String[] cycleReportFormat;
  protected NamedKeyStroke[] cycleDownKeys;
  protected int cycleIndex = -1;
  protected String description;
  protected boolean noSuppress;

  public ReportState() {
    this(ID, null);
  }

  public ReportState(String type, GamePiece inner) {
    mySetType(type);
    setInner(inner);
  }

  @Override
  public Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public void draw(Graphics g, int x, int y, Component obs, double zoom) {
    piece.draw(g, x, y, obs, zoom);
  }

  @Override
  public String getName() {
    return piece.getName();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    return KeyCommand.NONE;
  }

  @Override
  public String myGetState() {
    return Integer.toString(cycleIndex);
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(NamedKeyStrokeArrayConfigurer.encode(keys))
      .append(reportFormat)
      .append(NamedKeyStrokeArrayConfigurer.encode(cycleDownKeys))
      .append(StringArrayConfigurer.arrayToString(cycleReportFormat))
      .append(description)
      .append(noSuppress);
    return ID + se.getValue();
  }

  // We perform the inner commands first so that their effects will be reported
  @Override
  public Command keyEvent(KeyStroke stroke) {
    final Command c = piece.keyEvent(stroke);
    return c == null ? myKeyEvent(stroke) : c.append(myKeyEvent(stroke));
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    final GamePiece outer = getOutermost(this);

    // Retrieve the name, location and visibility of the unit prior to the
    // trait being executed if it is outside this one.

    format.setProperty(MAP_NAME, getMap() == null ? null : getMap().getConfigureName());
    format.setProperty(LOCATION_NAME, getMap() == null ? null : getMap().locationName(getPosition()));
    format.setProperty(OLD_MAP_NAME, (String) getProperty(BasicPiece.OLD_MAP));
    format.setProperty(OLD_LOCATION_NAME, (String) getProperty(BasicPiece.OLD_LOCATION_NAME));

    Command c = null;

    final java.util.Map<String, Object> oldPiece;
    final Object o = getProperty(Properties.SNAPSHOT);
    // If the SNAPSHOT is returned as a PropertyExporter instead of a Map, then it been set from custom code
    // that is still calling using PieceCloner.clonePiece. Extract the Property Map from the supplied GamePiece and annoy the user.
    if (o instanceof PropertyExporter) {
      oldPiece = ((PropertyExporter) o).getProperties();
      ProblemDialog.showOutdatedUsage(Resources.getString("Editor.ReportState.custom_trait_warning"));
    }
    else {
      // If this cast fails, then custom code developer has done something terribly wrong, so just let it crash and burn
      oldPiece = (java.util.Map<String, Object>) o;
    }

    final boolean wasVisible = oldPiece != null && !Boolean.TRUE.equals(oldPiece.get(Properties.INVISIBLE_TO_OTHERS));
    final boolean isVisible = !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS));

    PieceAccess.GlobalAccess.hideAll();
    final String oldUnitName = oldPiece == null ? null : (String) oldPiece.get(PropertyExporter.LOCALIZED_NAME);
    format.setProperty(OLD_UNIT_NAME, oldUnitName);
    final String newUnitName = outer.getLocalizedName();
    format.setProperty(NEW_UNIT_NAME, newUnitName);
    PieceAccess.GlobalAccess.revertAll();

    // Only make a report if:
    //  1. It's not part of a global command with Single Reporting on
    //  2. The piece is visible to all players either before or after the trait
    //     command was executed.

    if (isVisible || wasVisible) {
      final NamedKeyStroke[] allKeys = ArrayUtils.addAll(keys, cycleDownKeys);

      for (int i = 0; i < allKeys.length; ++i) {
        if (stroke != null && stroke.equals(allKeys[i].getKeyStroke())) {

          //
          // Find the Command Name
          //
          String commandName = "";
          final KeyCommand[] k = ((Decorator) outer).getKeyCommands();
          for (final KeyCommand keyCommand : k) {
            final KeyStroke commandKey = keyCommand.getKeyStroke();
            if (stroke.equals(commandKey)) {
              commandName = keyCommand.getName();
            }
          }

          final ChangeTracker tracker = new ChangeTracker(this);

          format.setProperty(COMMAND_NAME, commandName);

          String theFormat = reportFormat;
          if (cycleIndex >= 0 && cycleReportFormat.length > 0) {
            if (i < keys.length) {
              theFormat = cycleReportFormat[cycleIndex];
              cycleIndex = (cycleIndex + 1) % cycleReportFormat.length;
            }
            else {
              cycleIndex = (cycleIndex + cycleReportFormat.length - 1) % cycleReportFormat.length;
              theFormat = cycleReportFormat[(cycleIndex + cycleReportFormat.length - 1) % cycleReportFormat.length];
            }
          }
          format.setFormat(getTranslation(theFormat));

          final OldAndNewPieceProperties properties = new OldAndNewPieceProperties(oldPiece, outer);

          String reportText = format.getLocalizedText(properties);

          if (getMap() != null) {
            format.setFormat(getMap().getChangeFormat(noSuppress));
          }
          else if (!Map.isChangeReportingEnabled() && !noSuppress) {
            format.setFormat("");
          }
          else {
            format.setFormat("$" + Map.MESSAGE + "$");
          }
          format.setProperty(Map.MESSAGE, reportText);
          reportText = format.getLocalizedText(properties);

          if (reportText.length() > 0) {
            final Command display = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + reportText);
            display.execute();
            c = display;
          }
          c = tracker.getChangeCommand().append(c);
          break;
        }
      }
    }

    return c;
  }

  protected String getPieceName() {

    final String name;

    PieceAccess.GlobalAccess.hideAll();

    name = getOutermost(this).getName();

    PieceAccess.GlobalAccess.revertAll();

    return name;
  }

  @Override
  public void mySetState(String newState) {
    if (newState.length() > 0) {
      try {
        cycleIndex = Integer.parseInt(newState);
      }
      catch (NumberFormatException e) {
        cycleIndex = -1;
        reportDataError(this, Resources.getString("Error.non_number_error"), "Trying to init Message Index to " + newState); // NON-NLS
      }
    }
    else {
      cycleIndex = -1;
    }
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.ReportState.trait_description", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ReportChanges.html"); // NON-NLS
  }

  @Override
  public void mySetType(String type) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    final String encodedKeys = st.nextToken("");
    if (encodedKeys.indexOf(',') > 0) {
      keys = NamedKeyStrokeArrayConfigurer.decode(encodedKeys);
    }
    else {
      keys = new NamedKeyStroke[encodedKeys.length()];
      for (int i = 0; i < keys.length; i++) {
        keys[i] = NamedKeyStroke.of(encodedKeys.charAt(i), InputEvent.CTRL_DOWN_MASK);
      }
    }
    reportFormat = st.nextToken("$" + LOCATION_NAME + "$: $" + NEW_UNIT_NAME + "$ *");
    final String encodedCycleDownKeys = st.nextToken("");
    if (encodedCycleDownKeys.indexOf(',') > 0) {
      cycleDownKeys = NamedKeyStrokeArrayConfigurer.decode(encodedCycleDownKeys);
    }
    else {
      cycleDownKeys = new NamedKeyStroke[encodedCycleDownKeys.length()];
      for (int i = 0; i < cycleDownKeys.length; i++) {
        cycleDownKeys[i] = NamedKeyStroke.of(encodedCycleDownKeys.charAt(i), InputEvent.CTRL_DOWN_MASK);
      }
    }
    cycleReportFormat = StringArrayConfigurer.stringToArray(st.nextToken(""));
    description = st.nextToken("");
    noSuppress = st.nextBoolean(false);
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    final int c = cycleReportFormat == null ? 0 : cycleReportFormat.length;
    final String[] formats = new String[c + 1];
    final String[] descriptions = new String[c + 1];
    formats[0] = reportFormat;
    descriptions[0] = getCommandDescription(description, Resources.getString("Editor.ReportState.report_format"));
    int j = 1;
    for (int i = 0; i < c; i++) {
      formats[j] = cycleReportFormat[i];
      descriptions[j] = getCommandDescription(description, Resources.getString("Editor.ReportState.report_format_2"));
      j++;
    }
    return getI18nData(formats, descriptions);
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof ReportState)) return false;
    final ReportState c = (ReportState) o;
    if (!Arrays.equals(keys, c.keys)) return false;
    if (! Objects.equals(reportFormat, c.reportFormat)) return false;
    if (!Arrays.equals(cycleDownKeys, c.cycleDownKeys)) return false;
    if (!Arrays.equals(cycleReportFormat, c.cycleReportFormat)) return false;
    if (!Objects.equals(noSuppress, c.noSuppress)) return false;
    return Objects.equals(description, c.description);
  }

  public static final String OLD_UNIT_NAME = "oldPieceName"; // NON-NLS
  public static final String NEW_UNIT_NAME = "newPieceName"; // NON-NLS
  public static final String MAP_NAME = "mapName"; // NON-NLS
  public static final String OLD_MAP_NAME = "oldMapName"; // NON-NLS
  public static final String LOCATION_NAME = "location"; // NON-NLS
  public static final String OLD_LOCATION_NAME = "oldLocation"; // NON-NLS
  public static final String COMMAND_NAME = "menuCommand"; // NON-NLS

  public static class Ed implements PieceEditor {

    private final NamedKeyStrokeArrayConfigurer keys;
    private final StringConfigurer format;
    private final JLabel formatLabel;
    private final BooleanConfigurer cycle;
    private final JLabel cycleLabel;
    private final StringArrayConfigurer cycleFormat;
    private final JLabel cycleDownLabel;
    private final NamedKeyStrokeArrayConfigurer cycleDownKeys;
    protected StringConfigurer descInput;
    private final TraitConfigPanel box;
    private final BooleanConfigurer noSuppressConfig;

    public Ed(ReportState piece) {

      box = new TraitConfigPanel();

      descInput = new StringConfigurer(piece.description);
      descInput.setHintKey("Editor.description_hint");
      box.add("Editor.description_label", descInput);

      keys = new NamedKeyStrokeArrayConfigurer(piece.keys);
      box.add("Editor.ReportState.report_on_these_keystrokes", keys);

      cycle = new BooleanConfigurer(piece.cycleReportFormat.length > 0);
      cycle.addPropertyChangeListener(e -> adjustVisibilty());
      box.add("Editor.ReportState.cycle_through_different_messages", cycle);

      formatLabel = new JLabel(Resources.getString("Editor.ReportState.report_format_3"));
      format = new PlayerIdFormattedStringConfigurer(
        new String[]{
          COMMAND_NAME,
          OLD_UNIT_NAME,
          NEW_UNIT_NAME,
          MAP_NAME,
          OLD_MAP_NAME,
          LOCATION_NAME,
          OLD_LOCATION_NAME},
        piece.reportFormat);
      box.add(formatLabel, format);

      cycleLabel = new JLabel(Resources.getString("Editor.ReportState.message_formats"));
      cycleFormat = new StringArrayConfigurer(piece.cycleReportFormat);
      box.add(cycleLabel, cycleFormat);

      cycleDownLabel = new JLabel(Resources.getString("Editor.ReportState.report_previous"));
      cycleDownKeys = new NamedKeyStrokeArrayConfigurer(piece.cycleDownKeys);
      box.add(cycleDownLabel, cycleDownKeys);

      noSuppressConfig = new BooleanConfigurer(piece.noSuppress);
      box.add("Editor.ReportState.no_suppress", noSuppressConfig);

      adjustVisibilty();
    }

    private void adjustVisibilty() {
      format.getControls().setVisible(!cycle.getValueBoolean());
      formatLabel.setVisible(!cycle.getValueBoolean());
      cycleFormat.getControls().setVisible(cycle.getValueBoolean());
      cycleLabel.setVisible(cycle.getValueBoolean());
      cycleDownKeys.getControls().setVisible(cycle.getValueBoolean());
      cycleDownLabel.setVisible(cycle.getValueBoolean());
      repack(box);
    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getState() {
      return cycle.getValueBoolean() ? "0" : "-1";
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(keys.getValueString())
        .append(format.getValueString())
        .append(cycleDownKeys.getValueString())
        .append(cycleFormat.getValueString())
        .append(descInput.getValueString())
        .append(noSuppressConfig.getValueString());
      return ID + se.getValue();
    }
  }

  /**
   * Looks in both the new and old piece for property values.
   * Any properties with names of the format "oldXyz" are changed
   * to "xyz" and applied to the old piece.
   * @author rkinney
   *
   */
  private static class OldAndNewPieceProperties implements PropertySource {
    private final java.util.Map<String, Object> oldPiece;
    private final GamePiece newPiece;
    public OldAndNewPieceProperties(java.util.Map<String, Object> oldPiece, GamePiece newPiece) {
      super();
      this.oldPiece = oldPiece;
      this.newPiece = newPiece;
    }
    @Override
    public Object getProperty(Object key) {
      Object value = null;
      if (key != null) {
        String name = key.toString();
        if (name.startsWith("old") && name.length() >= 4) { // NON-NLS
          name = name.substring(3);
          value = oldPiece.get(name);
        }
        else {
          value = newPiece.getProperty(key);
        }
      }
      return value;
    }

    @Override
    public Object getLocalizedProperty(Object key) {
      return getProperty(key);
    }

  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Arrays.asList(keys);
  }

  /**
   * @return a list of any Message Format strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getFormattedStringList() {
    final List<String> l = new ArrayList<>();

    if (cycleIndex >= 0 && cycleReportFormat.length > 0) {
      Collections.addAll(l, cycleReportFormat);
    }
    else {
      l.add(reportFormat);
    }
    return l;
  }

  /**
   * In case reports use HTML and  refer to any image files
   * @param s Collection to add image names to
   */
  @Override
  public void addLocalImageNames(Collection<String> s) {
    HTMLImageFinder h;
    if (cycleIndex >= 0 && cycleReportFormat.length > 0) {
      for (final String r : cycleReportFormat) {
        h = new HTMLImageFinder(r);
        h.addImageNames(s);
      }
    }
    else {
      h = new HTMLImageFinder(reportFormat);
      h.addImageNames(s);
    }
  }
}
