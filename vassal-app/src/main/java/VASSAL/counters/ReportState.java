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
/*
 * Created by IntelliJ IDEA.
 * User: rkinney
 * Date: Oct 2, 2002
 * Time: 6:30:35 AM
 * To change template for new class use
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package VASSAL.counters;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.event.InputEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import org.apache.commons.lang3.ArrayUtils;

import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.Map;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.build.module.properties.PropertySource;
import VASSAL.command.ChangeTracker;
import VASSAL.command.Command;
import VASSAL.configure.NamedKeyStrokeArrayConfigurer;
import VASSAL.configure.PlayerIdFormattedStringConfigurer;
import VASSAL.configure.StringArrayConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.FormattedString;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.SequenceEncoder;

/**
 * A GamePiece with this trait will echo the piece's current name when any of a given key commands are pressed
 * (and after they take effect)
 */
public class ReportState extends Decorator implements TranslatablePiece {
  public static final String ID = "report;";
  protected NamedKeyStroke[] keys;
  protected FormattedString format = new FormattedString();
  protected String reportFormat;
  protected String[] cycleReportFormat;
  protected NamedKeyStroke[] cycleDownKeys;
  protected int cycleIndex = -1;
  protected String description;

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
    return new KeyCommand[0];
  }

  @Override
  public String myGetState() {
    return cycleIndex + "";
  }

  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(NamedKeyStrokeArrayConfigurer.encode(keys)).append(reportFormat).append(NamedKeyStrokeArrayConfigurer.encode(cycleDownKeys)).append(StringArrayConfigurer.arrayToString(cycleReportFormat))
    .append(description);
    return ID + se.getValue();
  }

  // We perform the inner commands first so that their effects will be reported
  @Override
  public Command keyEvent(KeyStroke stroke) {
    Command c = piece.keyEvent(stroke);
    return c == null ? myKeyEvent(stroke) : c.append(myKeyEvent(stroke));
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    GamePiece outer = getOutermost(this);

    // Retrieve the name, location and visibilty of the unit prior to the
    // trait being executed if it is outside this one.

    format.setProperty(MAP_NAME, getMap() == null ? null : getMap().getConfigureName());
    format.setProperty(LOCATION_NAME, getMap() == null ? null : getMap().locationName(getPosition()));
    format.setProperty(OLD_MAP_NAME, (String) getProperty(BasicPiece.OLD_MAP));
    format.setProperty(OLD_LOCATION_NAME, (String) getProperty(BasicPiece.OLD_LOCATION_NAME));

    Command c = null;

    GamePiece oldPiece = (GamePiece) getProperty(Properties.SNAPSHOT);

    boolean wasVisible = oldPiece != null && !Boolean.TRUE.equals(oldPiece.getProperty(Properties.INVISIBLE_TO_OTHERS));
    boolean isVisible = !Boolean.TRUE.equals(outer.getProperty(Properties.INVISIBLE_TO_OTHERS));

    PieceAccess.GlobalAccess.hideAll();
    String oldUnitName = oldPiece == null ? null : oldPiece.getLocalizedName();
    format.setProperty(OLD_UNIT_NAME, oldUnitName);
    String newUnitName = outer.getLocalizedName();
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
          KeyCommand[] k = ((Decorator) outer).getKeyCommands();
          for (KeyCommand keyCommand : k) {
            KeyStroke commandKey = keyCommand.getKeyStroke();
            if (stroke.equals(commandKey)) {
              commandName = keyCommand.getName();
            }
          }

          ChangeTracker tracker = new ChangeTracker(this);

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

          OldAndNewPieceProperties properties = new OldAndNewPieceProperties(oldPiece,outer);

          String reportText = format.getLocalizedText(properties);

          if (getMap() != null) {
            format.setFormat(getMap().getChangeFormat());
          }
          else if (!Map.isChangeReportingEnabled()) {
            format.setFormat("");
          }
          else {
            format.setFormat("$"+Map.MESSAGE+"$");
          }
          format.setProperty(Map.MESSAGE, reportText);
          reportText = format.getLocalizedText(properties);

          if (reportText.length() > 0) {
            Command display = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), "* " + reportText);
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

    String name = "";

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
        reportDataError(this, Resources.getString("Error.non_number_error"), "Trying to init Message Index to "+newState);
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
    String d =  "Report Action";
    if (description.length() > 0) {
      d += " - " + description;
    }
    return d;
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("ReportChanges.htm");
  }

  @Override
  public void mySetType(String type) {
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(type, ';');
    st.nextToken();
    String encodedKeys = st.nextToken("");
    if (encodedKeys.indexOf(',') > 0) {
      keys = NamedKeyStrokeArrayConfigurer.decode(encodedKeys);
    }
    else {
      keys = new NamedKeyStroke[encodedKeys.length()];
      for (int i = 0; i < keys.length; i++) {
        keys[i] = NamedKeyStroke.getNamedKeyStroke(encodedKeys.charAt(i),InputEvent.CTRL_DOWN_MASK);
      }
    }
    reportFormat = st.nextToken("$" + LOCATION_NAME + "$: $" + NEW_UNIT_NAME + "$ *");
    String encodedCycleDownKeys = st.nextToken("");
    if (encodedCycleDownKeys.indexOf(',') > 0) {
      cycleDownKeys = NamedKeyStrokeArrayConfigurer.decode(encodedCycleDownKeys);
    }
    else {
      cycleDownKeys = new NamedKeyStroke[encodedCycleDownKeys.length()];
      for (int i = 0; i < cycleDownKeys.length; i++) {
        cycleDownKeys[i] = NamedKeyStroke.getNamedKeyStroke(encodedCycleDownKeys.charAt(i),InputEvent.CTRL_DOWN_MASK);
      }
    }
    cycleReportFormat = StringArrayConfigurer.stringToArray(st.nextToken(""));
    description = st.nextToken("");
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    int c = cycleReportFormat == null ? 0 : cycleReportFormat.length;
    String[] formats = new String[c+1];
    String[] descriptions = new String[c+1];
    formats[0] = reportFormat;
    descriptions[0] = getCommandDescription(description, "Report Format");
    int j = 1;
    for (int i=0; i < c; i++) {
      formats[j] = cycleReportFormat[i];
      descriptions[j] = getCommandDescription(description, "Report Format " + j);
      j++;
    }
    return getI18nData(formats, descriptions);
  }

  public static final String OLD_UNIT_NAME = "oldPieceName";
  public static final String NEW_UNIT_NAME = "newPieceName";
  public static final String MAP_NAME = "mapName";
  public static final String OLD_MAP_NAME = "oldMapName";
  public static final String LOCATION_NAME = "location";
  public static final String OLD_LOCATION_NAME = "oldLocation";
  public static final String COMMAND_NAME = "menuCommand";

  public static class Ed implements PieceEditor {

    private NamedKeyStrokeArrayConfigurer keys;
    private StringConfigurer format;
    private JCheckBox cycle;
    private StringArrayConfigurer cycleFormat;
    private NamedKeyStrokeArrayConfigurer cycleDownKeys;
    protected StringConfigurer descInput;
    private JPanel box;

    public Ed(ReportState piece) {

      box = new JPanel();
      box.setLayout(new BoxLayout(box, BoxLayout.Y_AXIS));
      descInput = new StringConfigurer(null, "Description:  ", piece.description);
      box.add(descInput.getControls());
      keys = new NamedKeyStrokeArrayConfigurer(null, "Report on these keystrokes:  ", piece.keys);
      box.add(keys.getControls());
      cycle = new JCheckBox("Cycle through different messages?");
      box.add(cycle);
      format = new PlayerIdFormattedStringConfigurer(
        null,
        "Report format:  ",
        new String[]{
          COMMAND_NAME,
          OLD_UNIT_NAME,
          NEW_UNIT_NAME,
          MAP_NAME,
          OLD_MAP_NAME,
          LOCATION_NAME,
          OLD_LOCATION_NAME});

      format.setValue(piece.reportFormat);
      box.add(format.getControls());
      cycleFormat = new StringArrayConfigurer(null, "Message formats", piece.cycleReportFormat);
      box.add(cycleFormat.getControls());
      cycleDownKeys = new NamedKeyStrokeArrayConfigurer(null, "Report previous message on these keystrokes:  ", piece.cycleDownKeys);
      box.add(cycleDownKeys.getControls());
      ItemListener l = new ItemListener() {
        @Override
        public void itemStateChanged(ItemEvent e) {
          format.getControls().setVisible(!cycle.isSelected());
          cycleFormat.getControls().setVisible(cycle.isSelected());
          cycleDownKeys.getControls().setVisible(cycle.isSelected());
          Window w = SwingUtilities.getWindowAncestor(box);
          if (w != null) {
            w.pack();
          }
        }
      };
      l.itemStateChanged(null);
      cycle.addItemListener(l);
      cycle.setSelected(piece.cycleReportFormat.length > 0);
    }

    @Override
    public Component getControls() {
      return box;
    }

    @Override
    public String getState() {
      return cycle.isSelected() ? "0" : "-1";
    }

    @Override
    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      if (cycle.isSelected() && cycleFormat.getStringArray().length > 0) {
        se.append(keys.getValueString()).append("").append(cycleDownKeys.getValueString()).append(cycleFormat.getValueString());
      }
      else {
        se.append(keys.getValueString()).append(format.getValueString()).append("").append("");
      }
      se.append(descInput.getValueString());
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
    private GamePiece oldPiece;
    private GamePiece newPiece;
    public OldAndNewPieceProperties(GamePiece oldPiece, GamePiece newPiece) {
      super();
      this.oldPiece = oldPiece;
      this.newPiece = newPiece;
    }
    @Override
    public Object getProperty(Object key) {
      Object value = null;
      if (key != null) {
        String name = key.toString();
        if (name.startsWith("old") && name.length() >= 4) {
          name = name.substring(3);
          value = oldPiece.getProperty(name);
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
}
