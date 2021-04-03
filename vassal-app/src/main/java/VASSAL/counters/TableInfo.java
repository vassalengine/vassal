/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney
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
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.Resources;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;

import java.awt.Point;
import java.awt.Shape;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.KeyStroke;

/**
 * A Decorator class that endows a GamePiece with an editable
 * spreadsheet (i.e. JTable) */
public class TableInfo extends Decorator implements TranslatablePiece {
  public static final String ID = "table;"; // NON-NLS

  protected String values;
  protected String oldState;
  protected int nRows, nCols;
  protected String command;
  protected NamedKeyStroke launchKey;
  protected KeyCommand launch;
  protected JTable table;
  protected JDialog frame;
  protected String description = "";

  public TableInfo() {
    this(ID + "2;2;" + Resources.getString("Editor.TableInfo.default_command") + ";S", null); // NON-NLS
  }

  public TableInfo(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }

  public int getRowCount() {
    return nRows;
  }

  public int getColumnCount() {
    return nCols;
  }

  @Override
  public void mySetType(String s) {
    s = s.substring(ID.length());
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    nRows = st.nextInt(2);
    nCols = st.nextInt(2);
    command = st.nextToken(Resources.getString("Editor.TableInfo.default_command"));
    launchKey = st.nextNamedKeyStroke(null);
    description = st.nextToken("");
    frame = null;
    table = null;
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
  public String myGetState() {
    if (table == null) {
      return values;
    }
    else {
      final SequenceEncoder se = new SequenceEncoder(',');
      for (int row = 0; row < nRows; ++row) {
        for (int col = 0; col < nCols; ++col) {
          final String s = (String) table.getValueAt(row, col);
          se.append(s == null ? "" : s);
        }
      }
      return se.getValue();
    }
  }

  @Override
  public void mySetState(String state) {
    if (table == null) {
      values = state;
    }
    else {
      setValues(state);
    }
  }

  /**
   * @param val a comma-separated list of table values
   */
  private void setValues(String val) {
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(val, ',');

    for (int row = 0; row < nRows; ++row) {
      for (int col = 0; col < nCols; ++col) {
        table.setValueAt(st.nextToken(), row, col);
      }
    }
  }

  @Override
  public String myGetType() {
    final SequenceEncoder se = new SequenceEncoder(';');
    se.append(nRows).append(nCols).append(command).append(launchKey).append(description);
    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    if (launch == null) {
      launch = new KeyCommand(command, launchKey, Decorator.getOutermost(this), this);
    }
    return new KeyCommand[]{launch};
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();

    if (!launch.matches(stroke)) {
      return null;
    }

    if (frame == null) {
      frame = new JDialog(GameModule.getGameModule().getPlayerWindow(), false);
      table = new JTable(nRows, nCols);
      setValues(values);
      table.setTableHeader(null);
      final JScrollPane scroll = new ScrollPane(table);
      scroll.getViewport().setPreferredSize(table.getPreferredSize());
      frame.add(scroll);

      Point p = GameModule.getGameModule().getPlayerWindow().getLocation();
      final Map map = getMap();
      if (map != null) {
        final JComponent view = map.getView();
        if (view.isShowing()) {
          p = view.getLocationOnScreen();
          final Point p2 = map.mapToComponent(getPosition());
          p.translate(p2.x, p2.y);
        }
      }
      frame.setLocation(p.x, p.y);

      frame.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(WindowEvent evt) {
          table.editingStopped(null);
          final GamePiece outer = Decorator.getOutermost(TableInfo.this);
          if (outer.getId() != null) {
            GameModule.getGameModule().sendAndLog(new ChangePiece(outer.getId(), oldState, outer.getState()));
          }
        }
      });
      frame.pack();
    }
    frame.setTitle(getName());
    oldState = Decorator.getOutermost(this).getState();
    frame.setVisible(true);

    return null;
  }

  @Override
  public String getDescription() {
    return buildDescription("Editor.TableInfo.trait_description", description);
  }

  @Override
  public HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Spreadsheet.html"); // NON-NLS
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  @Override
  public PieceI18nData getI18nData() {
    return getI18nData(command, Resources.getString("Editor.TableInfo.table_info_command"));
  }

  @Override
  public boolean testEquals(Object o) {
    if (! (o instanceof TableInfo)) return false;
    final TableInfo c = (TableInfo) o;

    if (! Objects.equals(nRows, c.nRows)) return false;
    if (! Objects.equals(nCols, c.nCols)) return false;
    if (! Objects.equals(command, c.command)) return false;
    if (! Objects.equals(launchKey, c.launchKey)) return false;

    return Objects.equals(values, c.values);
  }

  private static class Ed implements PieceEditor {
    private final IntConfigurer rowConfig;
    private final IntConfigurer colConfig;
    private final StringConfigurer commandConfig;
    private final NamedHotKeyConfigurer keyConfig;
    private final TraitConfigPanel panel;
    private final StringConfigurer descInput;

    public Ed(TableInfo p) {

      panel = new TraitConfigPanel();

      descInput = new StringConfigurer(p.description);
      descInput.setHintKey("Editor.description_hint");
      panel.add("Editor.description_label", descInput);

      commandConfig = new StringConfigurer(p.command);
      commandConfig.setHintKey("Editor.menu_command_hint");
      panel.add("Editor.menu_command", commandConfig);

      keyConfig = new NamedHotKeyConfigurer(p.launchKey);
      panel.add("Editor.keyboard_command", keyConfig);

      rowConfig = new IntConfigurer(p.nRows);
      panel.add("Editor.TableInfo.number_of_rows", rowConfig);

      colConfig = new IntConfigurer(p.nCols);
      panel.add("Editor.TableInfo.number_of_columns", colConfig);
    }

    @Override
    public java.awt.Component getControls() {
      return panel;
    }

    @Override
    public String getType() {
      final SequenceEncoder se = new SequenceEncoder(';');
      se.append(rowConfig.getValueString())
        .append(colConfig.getValueString())
        .append(commandConfig.getValueString())
        .append(keyConfig.getValueString())
        .append(descInput.getValueString());
      return ID + se.getValue();
    }

    @Override
    public String getState() {
      final StringBuilder buf = new StringBuilder();
      int n = (Integer) rowConfig.getValue() * (Integer) colConfig.getValue();
      while (--n > 0) {
        buf.append(',');
      }
      return buf.toString();
    }
  }



  /**
   * @return a list of the Decorator's string/expression fields if any (for search)
   */
  @Override
  public List<String> getExpressionList() {
    final List<String> l = new ArrayList<>();
    for (int row = 0; row < nRows; ++row) {
      for (int col = 0; col < nCols; ++col) {
        l.add((String) table.getValueAt(row, col));
      }
    }
    return l;
  }

  /**
   * @return a list of any Named KeyStrokes referenced in the Decorator, if any (for search)
   */
  @Override
  public List<NamedKeyStroke> getNamedKeyStrokeList() {
    return Collections.singletonList(launchKey);
  }

  /**
   * @return a list of any Menu Text strings referenced in the Decorator, if any (for search)
   */
  @Override
  public List<String> getMenuTextList() {
    return List.of(command);
  }
}
