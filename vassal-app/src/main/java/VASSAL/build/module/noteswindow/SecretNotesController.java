/*
 *
 * Copyright (c) 2004 by Rodney Kinney
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
package VASSAL.build.module.noteswindow;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.beans.PropertyChangeListener;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.border.TitledBorder;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableColumn;

import VASSAL.build.BadDataReport;
import VASSAL.build.GameModule;
import VASSAL.build.module.Chatter;
import VASSAL.build.module.GameComponent;
import VASSAL.build.module.GlobalOptions;
import VASSAL.command.Command;
import VASSAL.command.CommandEncoder;
import VASSAL.configure.StringConfigurer;
import VASSAL.configure.TextConfigurer;
import VASSAL.i18n.Resources;
import VASSAL.tools.ErrorDialog;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.WarningDialog;

public class SecretNotesController implements GameComponent, CommandEncoder, AddSecretNoteCommand.Interface {
  public static final String COMMAND_PREFIX = "SNOTE\t"; //$NON-NLS-1$

  private Controls controls;
  private JPanel panel;
  private final List<SecretNote> notes;
  private List<SecretNote> lastSavedNotes;

  // Secret Note display table columns
  public static final int COL_HANDLE = 0;
  public static final int COL_DTM = 1;
  public static final int COL_NAME = 2;
  public static final int COL_REVEALED = 3;

  private static final String INTERNAL_DATETIME_FORMAT = "MM/dd/yyyy h:mm a"; //NON-NLS

  /**
   * Date formatter to save and restore date/times in the save file.
   *
   * Not thread-safe!
   */
  public static final DateFormat INTERNAL_DATE_FORMATTER =
    new SimpleDateFormat(INTERNAL_DATETIME_FORMAT);

  /**
   * Date formatter to display date/time to the player.
   *
   * Not thread-safe!
   */
  public static final DateFormat LOCAL_DATE_FORMATTER =
    DateFormat.getDateTimeInstance(DateFormat.SHORT, DateFormat.SHORT, Locale.getDefault());

  public SecretNotesController() {
    notes = new ArrayList<>();
    controls = new Controls();
  }

  @Override
  public Command getRestoreCommand() {
    Command comm = null;
    for (final SecretNote note : notes) {
      final Command c = new AddSecretNoteCommand(this, note);
      if (comm == null) {
        comm = c;
      }
      else {
        comm.append(c);
      }
    }
    return comm;
  }

  @Override
  public void setup(boolean gameStarting) {
    if (!gameStarting) {
      notes.clear();
      rebuildControls();
    }
  }

  private void rebuildControls() {
    if (panel != null) {
      panel.remove(controls);
    }
    controls = new Controls();
    if (panel != null) {
      panel.add(controls);
    }
  }

  @Override
  public Command decode(String command) {
    if (!command.startsWith(COMMAND_PREFIX)) {
      return null;
    }
    final SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(command.substring(COMMAND_PREFIX.length()), '\t');
    final String name = st.nextToken();
    final String owner = st.nextToken();
    final boolean hidden = "true".equals(st.nextToken()); //$NON-NLS-1$
    final String text = TextConfigurer.restoreNewlines(st.nextToken());
    String handle = ""; //$NON-NLS-1$
    Date date = null;

    if (st.hasMoreTokens()) {
      final String formattedDate = st.nextToken();
      try {
        date = new SimpleDateFormat(INTERNAL_DATETIME_FORMAT).parse(formattedDate);
      }
      catch (final ParseException e) {
        ErrorDialog.dataWarning(new BadDataReport("Illegal date format", formattedDate, e));  //NON-NLS
      }
    }

    if (st.hasMoreTokens()) {
      handle = st.nextToken();
    }

    final SecretNote note = new SecretNote(name, owner, text, hidden, date, handle);
    return new AddSecretNoteCommand(this, note);
  }

  @Override
  public String encode(Command c) {
    if (!(c instanceof AddSecretNoteCommand)) {
      return null;
    }
    final SecretNote note = ((AddSecretNoteCommand) c).getNote();
    final SequenceEncoder se = new SequenceEncoder('\t');
    final String date =
      note.getDate() == null ? "" : new SimpleDateFormat(INTERNAL_DATETIME_FORMAT).format(note.getDate()); //$NON-NLS-1$

    return COMMAND_PREFIX +
      se
        .append(note.getName())
        .append(note.getOwner())
        .append(note.isHidden())
        .append(TextConfigurer.escapeNewlines(note.getText()))
        .append(date)
        .append(note.getHandle()).getValue();
  }

  @Override
  public void addSecretNote(SecretNote note) {
    final int index = notes.indexOf(note);
    if (index >= 0) {
      notes.set(index, note);
    }
    else {
      notes.add(0, note);
    }
    rebuildControls();
  }


  public JComponent getControls() {
    if (panel == null) {
      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
      final JLabel l = new JLabel(Resources.getString("Notes.visible_once_revealed")); //$NON-NLS-1$
      l.setAlignmentX(0.0F);
      panel.add(l);
      panel.add(controls);
    }
    return panel;
  }

  public Command save() {
    Command comm = null;
    for (final SecretNote secretNote : notes) {
      final int index = lastSavedNotes.indexOf(secretNote);
      if (index < 0 ||
          lastSavedNotes.get(index).isHidden() != secretNote.isHidden()) {
        Command c = new AddSecretNoteCommand(this, secretNote);
        if (comm == null) {
          comm = c;
        }
        else {
          comm.append(c);
        }
        final String msg;
        if (index < 0) {
          msg = "* " + Resources.getString("Notes.has_created", GlobalOptions.getInstance().getPlayerId(), secretNote.getName()) + " *"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        else {
          msg = "* " + Resources.getString("Notes.has_revealed", GlobalOptions.getInstance().getPlayerId(), secretNote.getName()) + " *"; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        c = new Chatter.DisplayText(GameModule.getGameModule().getChatter(), msg);
        c.execute();
        comm.append(c);
      }
    }
    return comm;
  }

  public void captureState() {
    lastSavedNotes = new ArrayList<>(notes);
  }

  public void restoreState() {
    notes.clear();
    notes.addAll(lastSavedNotes);
    rebuildControls();
  }

  private class Controls extends JPanel implements ItemListener {
    private static final long serialVersionUID = 1L;

    private final JTextArea text;
    private final JTable table;
    private final JButton revealButton;

    private final String[] columnNames = {
      Resources.getString("Notes.player"),    //$NON-NLS-1$
      Resources.getString("Notes.date_time"), //$NON-NLS-1$
      Resources.getString("Notes.note_name"), //$NON-NLS-1$
      Resources.getString("Notes.revealed")   //$NON-NLS-1$
    };

    public Controls() {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

      table = new JTable(new MyTableModel());
      initColumns(table);

      table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      final ListSelectionModel rowSM = table.getSelectionModel();
      rowSM.addListSelectionListener(e -> {
        //Ignore extra messages.
        if (e.getValueIsAdjusting())
          return;

        final ListSelectionModel lsm = (ListSelectionModel) e.getSource();
        if (!lsm.isSelectionEmpty()) {
          displaySelected();
        }
      });

      final JScrollPane secretScroll = new ScrollPane(table);
      table.setPreferredScrollableViewportSize(new Dimension(500, 100));

      add(secretScroll);

      final Box b = Box.createHorizontalBox();
      b.setAlignmentX(0.0F);

      final JButton newButton = new JButton(Resources.getString(Resources.NEW));
      newButton.addActionListener(e -> createNewNote());
      b.add(newButton);

      revealButton = new JButton(Resources.getString("Notes.reveal")); //$NON-NLS-1$
      revealButton.addActionListener(e -> revealSelectedNote());
      revealButton.setEnabled(false);
      b.add(revealButton);
      add(b);


      text = new JTextArea(6, 20);
      text.setEditable(false);
      final JScrollPane scroll = new ScrollPane(text);
      scroll.setBorder(new TitledBorder(Resources.getString("Notes.text"))); //$NON-NLS-1$
      add(scroll);
    }

    private void initColumns(JTable t) {
      TableColumn column;
      for (int i = 0; i < columnNames.length; i++) {
        column = t.getColumnModel().getColumn(i);
        final int width;
        switch (i) {
        case COL_HANDLE:
          width = 60;
          break;
        case COL_DTM:
          width = 100;
          break;
        case COL_NAME:
          width = 280;
          break;
        case COL_REVEALED:
          width = 60;
          break;
        default:
          width = 100;
          break;
        }
        column.setPreferredWidth(width);
      }
    }

    public class MyTableModel extends AbstractTableModel {
      private static final long serialVersionUID = 1L;

      @Override
      public String getColumnName(int col) {
        return columnNames[col];
      }

      @Override
      public int getRowCount() {
        return notes.size();
      }

      @Override
      public int getColumnCount() {
        return columnNames.length;
      }

      @Override
      public Object getValueAt(int row, int col) {
        final SecretNote note = notes.get(row);
        switch (col) {
        case COL_HANDLE:
          return note.getHandle();
        case COL_DTM:
          return note.getDate() == null ? "" : LOCAL_DATE_FORMATTER.format(note.getDate()); //$NON-NLS-1$
        case COL_NAME:
          return note.getName();
        case COL_REVEALED:
          return !note.isHidden();
        default:
          return null;
        }
      }

      @Override
      public Class<?> getColumnClass(int c) {
        return getValueAt(0, c).getClass();
      }

      @Override
      public boolean isCellEditable(int row, int col) {
        return false;
      }

      @Override
      public void setValueAt(Object value, int row, int col) {
      }
    }

    public void refresh() {
      table.setModel(new MyTableModel());
      initColumns(table);
      displaySelected();
    }

    private void revealSelectedNote() {
      final int selectedRow = table.getSelectedRow();
      if (selectedRow < 0) {
        return;
      }
      final String selectedName = (String) table.getValueAt(selectedRow, COL_NAME);
      SecretNote note = getNoteForName(selectedName);

      if (note.getOwner().equals(GameModule.getActiveUserId())) {
        note = new SecretNote(note.getName(), note.getOwner(), note.getText(), false, note.getDate(), note.getHandle());
        if (note != null) {
          final int i = notes.indexOf(note);
          notes.set(i, note);
          refresh();
        }
      }
    }

    public void createNewNote() {
      final Dialog parent =
        (Dialog) SwingUtilities.getAncestorOfClass(Dialog.class, this);

      final JDialog d;
      if (parent != null) {
        d = new JDialog(parent, true);
      }
      else {
        d = new JDialog(
          (Frame) SwingUtilities.getAncestorOfClass(Frame.class, this),
          true
        );
      }

      d.setTitle(Resources.getString("Notes.delayed_note")); //$NON-NLS-1$

      final StringConfigurer name = new StringConfigurer(null,
        Resources.getString("Notes.name")); //$NON-NLS-1$
      final TextConfigurer text = new TextConfigurer(null,
        Resources.getString("Notes.text")); //$NON-NLS-1$

      d.setLayout(new BoxLayout(d.getContentPane(), BoxLayout.Y_AXIS));
      d.add(name.getControls());
      d.add(text.getControls());

      final Box buttonPanel = Box.createHorizontalBox();
      final JButton okButton = new JButton(Resources.getString(Resources.OK));
      okButton.addActionListener(e -> {
        final SecretNote note = new SecretNote(
          name.getValueString(),
          GameModule.getActiveUserId(),
          (String) text.getValue(),
          true
        );

        if (notes.contains(note)) {
          WarningDialog.show(this, "Notes.note_exists");  //NON-NLS
        }
        else {
          notes.add(0, note);
          refresh();
          d.dispose();
        }
      });

      final PropertyChangeListener l = evt -> okButton.setEnabled(name.getValueString() != null
                          && name.getValueString().length() > 0
                          && text.getValueString() != null
                          && text.getValueString().length() > 0);
      name.addPropertyChangeListener(l);
      text.addPropertyChangeListener(l);

      okButton.setEnabled(false);
      buttonPanel.add(okButton);
      final JButton cancelButton =
        new JButton(Resources.getString(Resources.CANCEL));
      cancelButton.addActionListener(e -> d.dispose());
      d.add(buttonPanel);

      d.pack();
      d.setLocationRelativeTo(d.getOwner());
      d.setVisible(true);
    }

    @Override
    public void itemStateChanged(ItemEvent e) {
      displaySelected();
    }

    private void displaySelected() {
      revealButton.setEnabled(false);
      text.setText(""); //$NON-NLS-1$

      final int selectedRow = table.getSelectedRow();
      if (selectedRow < 0) {
        return;
      }
      final String selectedName = (String) table.getValueAt(selectedRow, COL_NAME);
      final SecretNote note = getNoteForName(selectedName);

      if (note != null) {
        if (note.getOwner().equals(GameModule.getActiveUserId())) {
          text.setText(note.getText());
          revealButton.setEnabled(note.isHidden());
        }
        else {
          text.setText(note.isHidden() ? Resources.getString("Notes.message_not_revealed") : note.getText()); //$NON-NLS-1$
        }
      }
    }
  }

  public SecretNote getNoteForName(String s) {
    for (final SecretNote n : notes) {
      if (n.getName().equals(s)) {
        return n;
      }
    }
    return null;
  }
}
