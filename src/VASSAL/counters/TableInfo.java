/*
 * $Id$
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

import java.awt.Point;
import java.awt.Shape;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.KeyStroke;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;

/**
 * A Decorator class that endows a GamePiece with an editable
 * spreadsheet (i.e. JTable) */
public class TableInfo extends Decorator implements TranslatablePiece {
  public static final String ID = "table;";

  protected String values;
  protected String oldState;
  protected int nRows, nCols;
  protected String command;
  protected NamedKeyStroke launchKey;
  protected KeyCommand launch;
  protected JTable table;
  protected JDialog frame;

  public TableInfo() {
    this(ID + "2;2;Show Data;S", null);
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

  public void mySetType(String s) {
    s = s.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, ';');
    nRows = st.nextInt(2);
    nCols = st.nextInt(2);
    command = st.nextToken();
    launchKey = st.nextNamedKeyStroke(null);
    frame = null;
    table = null;
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

  public String myGetState() {
    if (table == null) {
      return values;
    }
    else {
      SequenceEncoder se = new SequenceEncoder(',');
      for (int row = 0; row < nRows; ++row) {
        for (int col = 0; col < nCols; ++col) {
          String s = (String) table.getValueAt(row, col);
          se.append(s == null ? "" : s);
        }
      }
      return se.getValue();
    }
  }

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
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(val, ',');

    for (int row = 0; row < nRows; ++row) {
      for (int col = 0; col < nCols; ++col) {
        table.setValueAt(st.nextToken(), row, col);
      }
    }
  }

  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(';');
    se.append(nRows).append(nCols).append(command).append(launchKey);
    return ID + se.getValue();
  }

  protected KeyCommand[] myGetKeyCommands() {
    if (launch == null) {
      launch = new KeyCommand(command, launchKey, Decorator.getOutermost(this), this);
    }
    return new KeyCommand[]{launch};
  }

  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();
    if (launch.matches(stroke)) {
      if (frame == null) {
        frame = new JDialog((java.awt.Frame) null, false);
        table = new JTable(nRows, nCols);
        setValues(values);
        table.setTableHeader(null);
        JScrollPane scroll = new ScrollPane(table);
        scroll.getViewport().setPreferredSize(table.getPreferredSize());
        frame.add(scroll);
        Point p = GameModule.getGameModule().getFrame().getLocation();
        if (getMap() != null) {
          p = getMap().getView().getLocationOnScreen();
          Point p2 = getMap().componentCoordinates(getPosition());
          p.translate(p2.x, p2.y);
        }
        frame.setLocation(p.x, p.y);
        frame.addWindowListener(new WindowAdapter() {
          public void windowClosing(WindowEvent evt) {
            table.editingStopped(null);
            GamePiece outer = Decorator.getOutermost(TableInfo.this);
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
    else {
      return null;
    }
  }

  public String getDescription() {
    return "Spreadsheet";
  }

  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("Spreadsheet.htm");
  }

  public PieceEditor getEditor() {
    return new Ed(this);
  }

  public PieceI18nData getI18nData() {
    return getI18nData(command, "Table Info command");
  }

  private static class Ed implements PieceEditor {
    private IntConfigurer rowConfig = new IntConfigurer(null, "Number of rows:  ");
    private IntConfigurer colConfig = new IntConfigurer(null, "Number of columns:  ");
    private StringConfigurer commandConfig = new StringConfigurer(null, "Menu Command:  ");
    private NamedHotKeyConfigurer keyConfig;
    private JPanel panel;

    public Ed(TableInfo p) {
      rowConfig.setValue(p.nRows);
      colConfig.setValue(p.nCols);
      commandConfig.setValue(p.command);
      keyConfig = new NamedHotKeyConfigurer(null,"Keyboard Command:  ",p.launchKey);

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      panel.add(commandConfig.getControls());
      panel.add(keyConfig.getControls());
      panel.add(rowConfig.getControls());
      panel.add(colConfig.getControls());
    }

    public java.awt.Component getControls() {
      return panel;
    }

    public String getType() {
      SequenceEncoder se = new SequenceEncoder(';');
      se.append(rowConfig.getValueString())
        .append(colConfig.getValueString())
        .append(commandConfig.getValueString())
        .append(keyConfig.getValueString());
      return ID + se.getValue();
    }

    public String getState() {
      final StringBuilder buf = new StringBuilder();
      int n = ((Integer) rowConfig.getValue()).intValue()
        * ((Integer) colConfig.getValue()).intValue();
      while (--n > 0) {
        buf.append(',');
      }
      return buf.toString();
    }
  }
}
