/*
 *
 * Copyright (c) 2000-2003 by Rodney Kinney, Jim Urbas
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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.DefaultCellEditor;
import javax.swing.JButton;
import javax.swing.JColorChooser;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSpinner;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.KeyStroke;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableModel;
import javax.swing.text.JTextComponent;

import VASSAL.build.GameModule;
import VASSAL.build.module.documentation.HelpFile;
import VASSAL.command.ChangePiece;
import VASSAL.command.Command;
import VASSAL.configure.NamedHotKeyConfigurer;
import VASSAL.i18n.PieceI18nData;
import VASSAL.i18n.TranslatablePiece;
import VASSAL.tools.NamedKeyStroke;
import VASSAL.tools.ScrollPane;
import VASSAL.tools.SequenceEncoder;
import VASSAL.tools.swing.SwingUtils;

/**
 * A Decorator class that endows a GamePiece with a dialog.
 */
public class PropertySheet extends Decorator implements TranslatablePiece {
  public static final String ID = "propertysheet;";

  protected String oldState;

  // properties
  protected String menuName;
  protected NamedKeyStroke launchKeyStroke;
  protected KeyCommand launch;
  protected Color backgroundColor;
  protected String m_definition;

  protected PropertySheetDialog frame;
  protected JButton applyButton;

  // Commit type definitions
  final static String[] COMMIT_VALUES = {"Every Keystroke", "Apply Button or Enter Key", "Close Window or Enter Key"};
  static final int COMMIT_IMMEDIATELY = 0;
  static final int COMMIT_ON_APPLY = 1;
  static final int COMMIT_ON_CLOSE = 2;
  static final int COMMIT_DEFAULT = COMMIT_IMMEDIATELY;

  // Field type definitions
  static final String[] TYPE_VALUES = {"Text", "Multi-line text", "Label Only", "Tick Marks", "Tick Marks with Max Field", "Tick Marks with Value Field", "Tick Marks with Value & Max", "Spinner"};
  static final int TEXT_FIELD = 0;
  static final int TEXT_AREA = 1;
  static final int LABEL_ONLY = 2;
  static final int TICKS = 3;
  static final int TICKS_MAX = 4;
  static final int TICKS_VAL = 5;
  static final int TICKS_VALMAX = 6;
  static final int SPINNER = 7;

  protected int commitStyle = COMMIT_DEFAULT;

  protected boolean isUpdating;

  protected String state;
  protected Map<String,Object> properties = new HashMap<>();
  protected List<JComponent> m_fields;


  static final char TYPE_DELIMITOR = ';';
  static final char DEF_DELIMITOR = '~';
  static final char STATE_DELIMITOR = '~';
  static final char LINE_DELIMINATOR = '|';
  static final char VALUE_DELIMINATOR = '/';

  class PropertySheetDialog extends JDialog implements ActionListener {
    private static final long serialVersionUID = 1L;

    public PropertySheetDialog(Frame owner) {
      super(owner, false);
    }

    @Override
    public void actionPerformed(ActionEvent event) {
      if (applyButton != null) {
        applyButton.setEnabled(false);
      }
      updateStateFromFields();
    }
  }


  public PropertySheet() {
    // format is propertysheet;menu-name;keystroke;commitStyle;backgroundRed;backgroundGreen;backgroundBlue
    this(ID + ";Properties;P;;;;", null);
  }


  public PropertySheet(String type, GamePiece p) {
    mySetType(type);
    setInner(p);
  }


  /** Changes the "type" definition this decoration, which discards all value data and structures.
   *  Format: definition; name; keystroke
   */
  @Override
  public void mySetType(String s) {

    s = s.substring(ID.length());
    SequenceEncoder.Decoder st = new SequenceEncoder.Decoder(s, TYPE_DELIMITOR);

    m_definition = st.nextToken();
    menuName = st.nextToken();
    final String launchKeyToken = st.nextToken("");
    commitStyle = st.nextInt(COMMIT_DEFAULT);
    String red = st.hasMoreTokens() ? st.nextToken() : "";
    String green = st.hasMoreTokens() ? st.nextToken() : "";
    String blue = st.hasMoreTokens() ? st.nextToken() : "";
    final String launchKeyStrokeToken = st.nextToken("");

    backgroundColor = red.equals("") ? null : new Color(atoi(red), atoi(green), atoi(blue));
    frame = null;

    // Handle conversion from old character only key
    if (launchKeyStrokeToken.length() > 0) {
      launchKeyStroke = NamedHotKeyConfigurer.decode(launchKeyStrokeToken);
    }
    else if (launchKeyToken.length() > 0) {
      launchKeyStroke = new NamedKeyStroke(launchKeyToken.charAt(0), InputEvent.CTRL_DOWN_MASK);
    }
    else {
      launchKeyStroke = new NamedKeyStroke('P', InputEvent.CTRL_DOWN_MASK);
    }
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
  public java.awt.Rectangle boundingBox() {
    return piece.boundingBox();
  }

  @Override
  public Shape getShape() {
    return piece.getShape();
  }

  @Override
  public String myGetState() {
    return state;
  }

  @Override
  public void mySetState(String state) {
    this.state = state;
    updateFieldsFromState();
  }

  /** returns string defining the field types */
  @Override
  public String myGetType() {
    SequenceEncoder se = new SequenceEncoder(TYPE_DELIMITOR);

    String red = backgroundColor == null ? "" : Integer.toString(backgroundColor.getRed());
    String green = backgroundColor == null ? "" : Integer.toString(backgroundColor.getGreen());
    String blue = backgroundColor == null ? "" : Integer.toString(backgroundColor.getBlue());
    String commit = Integer.toString(commitStyle);

    se.append(m_definition).append(menuName).append("").append(commit).
        append(red).append(green).append(blue).append(launchKeyStroke);


    return ID + se.getValue();
  }

  @Override
  protected KeyCommand[] myGetKeyCommands() {
    launch = new KeyCommand(menuName, launchKeyStroke, Decorator.getOutermost(this), this);
    return new KeyCommand[]{launch};
  }

  /** parses leading integer from string */
  int atoi(String s) {
    int value = 0;
    if (s != null) {
      for (int i = 0; i < s.length() && Character.isDigit(s.charAt(i)); ++i) {
        value = value * 10 + s.charAt(i) - '0';
      }
    }
    return value;
  }

  /** parses trailing integer from string */
  int atoiRight(String s) {
    int value = 0;
    if (s != null) {
      int base = 1;
      for (int i = s.length() - 1; i >= 0 && Character.isDigit(s.charAt(i)); --i, base *= 10) {
        value = value + base * (s.charAt(i) - '0');
      }
    }
    return value;
  }

  // stores field values
  private void updateStateFromFields() {
    SequenceEncoder encoder = new SequenceEncoder(STATE_DELIMITOR);

    for (Object field : m_fields) {
      if (field instanceof JTextComponent) {
        encoder.append(((JTextComponent) field).getText()
               .replace('\n', LINE_DELIMINATOR));
      }
      else if (field instanceof TickPanel) {
        encoder.append(((TickPanel) field).getValue());
      }
      else {
        encoder.append("Unknown");
      }
    }

    if (encoder.getValue() != null && !encoder.getValue().equals(state)) {
      mySetState(encoder.getValue());

      GamePiece outer = Decorator.getOutermost(PropertySheet.this);
      if (outer.getId() != null) {
        GameModule.getGameModule().sendAndLog(
          new ChangePiece(outer.getId(), oldState, outer.getState()));
      }
    }
  }

  private void updateFieldsFromState() {
    isUpdating = true;
    properties.clear();

    final SequenceEncoder.Decoder defDecoder =
      new SequenceEncoder.Decoder(m_definition, DEF_DELIMITOR);
    final SequenceEncoder.Decoder stateDecoder =
      new SequenceEncoder.Decoder(state, STATE_DELIMITOR);

    for (int iField = 0; defDecoder.hasMoreTokens(); ++iField) {
      String name = defDecoder.nextToken();
      if (name.length() == 0) {
        continue;
      }

      final int type = name.charAt(0) - '0';
      name = name.substring(1);
      String value = stateDecoder.nextToken("");
      switch (type) {
      case TICKS:
      case TICKS_VAL:
      case TICKS_MAX:
      case TICKS_VALMAX:
        final int index = value.indexOf('/');
        properties.put(name, index > 0 ? value.substring(0, index) : value);
        break;
      default:
        properties.put(name, value);
      }

      value = value.replace(LINE_DELIMINATOR, '\n');

      if (frame != null) {
        final Object field = m_fields.get(iField);
        if (field instanceof JTextComponent) {
          final JTextComponent tf = (JTextComponent) field;
          final int pos = Math.min(tf.getCaretPosition(), value.length());
          tf.setText(value);
          tf.setCaretPosition(pos);
        }
        else if (field instanceof TickPanel) {
          ((TickPanel) field).updateValue(value);
        }
      }
    }

    if (applyButton != null) {
      applyButton.setEnabled(false);
    }

    isUpdating = false;
  }

  @Override
  public Command myKeyEvent(KeyStroke stroke) {
    myGetKeyCommands();

    if (launch.matches(stroke)) {
      if (frame == null) {
        m_fields = new ArrayList<>();
        VASSAL.build.module.Map map = piece.getMap();
        Frame parent = null;
        if (map != null && map.getView() != null) {
          Container topWin = map.getView().getTopLevelAncestor();
          if (topWin instanceof JFrame) {
            parent = (Frame) topWin;
          }
        }
        else {
          parent = GameModule.getGameModule().getFrame();
        }

        frame = new PropertySheetDialog(parent);

        JPanel pane = new JPanel();
        JScrollPane scroll =
          new JScrollPane(pane, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        frame.add(scroll);

        // set up Apply button
        if (commitStyle == COMMIT_ON_APPLY) {
          applyButton = new JButton("Apply");

          applyButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent event) {
              if (applyButton != null) {
                applyButton.setEnabled(false);
              }
              updateStateFromFields();
            }
          });

          applyButton.setMnemonic(java.awt.event.KeyEvent.VK_A); // respond to Alt+A
          applyButton.setEnabled(false);
        }

        // ... enable APPLY button when field changes
        DocumentListener changeListener = new DocumentListener() {
          @Override
          public void insertUpdate(DocumentEvent e) {
            update(e);
          }

          @Override
          public void removeUpdate(DocumentEvent e) {
            update(e);
          }

          @Override
          public void changedUpdate(DocumentEvent e) {
            update(e);
          }

          public void update(DocumentEvent e) {
            if (!isUpdating) {
              switch (commitStyle) {
              case COMMIT_IMMEDIATELY:
                // queue commit operation because it could do something
                // unsafe in a an event update
                SwingUtilities.invokeLater(new Runnable() {
                  @Override
                  public void run() {
                    updateStateFromFields();
                  }
                });
                break;
              case COMMIT_ON_APPLY:
                applyButton.setEnabled(true);
                break;
              case COMMIT_ON_CLOSE:
                break;
              default:
                throw new IllegalStateException();
              }
            }
          }
        };

        pane.setLayout(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.insets = new Insets(1, 3, 1, 3);
        c.gridx = 0;
        c.gridy = 0;
        c.fill = GridBagConstraints.BOTH;
        SequenceEncoder.Decoder defDecoder =
          new SequenceEncoder.Decoder(m_definition, DEF_DELIMITOR);
        SequenceEncoder.Decoder stateDecoder =
          new SequenceEncoder.Decoder(state, STATE_DELIMITOR);

        while (defDecoder.hasMoreTokens()) {
          String code = defDecoder.nextToken();
          if (code.length() == 0) {
            break;
          }
          int type = code.charAt(0) - '0';
          String name = code.substring(1);
          JComponent field;
          switch (type) {
          case TEXT_FIELD:
            field = new JTextField(stateDecoder.nextToken(""));
            ((JTextComponent) field).getDocument()
                                    .addDocumentListener(changeListener);
            ((JTextField) field).addActionListener(frame);
            m_fields.add(field);
            break;
          case TEXT_AREA:
            field = new JTextArea(
              stateDecoder.nextToken("").replace(LINE_DELIMINATOR, '\n'));
            ((JTextComponent) field).getDocument()
                                    .addDocumentListener(changeListener);
            m_fields.add(field);
            field = new ScrollPane(field);
            break;
          case TICKS:
          case TICKS_VAL:
          case TICKS_MAX:
          case TICKS_VALMAX:
            field = new TickPanel(stateDecoder.nextToken(""), type);
            ((TickPanel) field).addDocumentListener(changeListener);
            ((TickPanel) field).addActionListener(frame);
            if (backgroundColor != null)
              field.setBackground(backgroundColor);
            m_fields.add(field);
            break;
          case SPINNER:
            JSpinner spinner = new JSpinner();
            JTextField textField =
              ((JSpinner.DefaultEditor) spinner.getEditor()).getTextField();
            textField.setText(stateDecoder.nextToken(""));
            textField.getDocument().addDocumentListener(changeListener);
            m_fields.add(textField);
            field = spinner;
            break;
          case LABEL_ONLY:
          default :
            stateDecoder.nextToken("");
            field = null;
            m_fields.add(field);
            break;
          }
          c.gridwidth = type == TEXT_AREA || type == LABEL_ONLY ? 2 : 1;

          if (name != null && !name.equals("")) {
            c.gridx = 0;
            c.weighty = 0.0;
            c.weightx = c.gridwidth == 2 ? 1.0 : 0.0;
            pane.add(new JLabel(getTranslation(name)), c);
            if (c.gridwidth == 2) {
              ++c.gridy;
            }
          }

          if (field != null) {
            c.weightx = 1.0;
            c.weighty = type == TEXT_AREA ? 1.0 : 0.0;
            c.gridx = type == TEXT_AREA ? 0 : 1;
            pane.add(field, c);
            ++c.gridy;
          }
        }

        if (backgroundColor != null) {
          pane.setBackground(backgroundColor);
        }

        if (commitStyle == COMMIT_ON_APPLY) {
          // setup Close button
          JButton closeButton = new JButton("Close");
          closeButton.setMnemonic(java.awt.event.KeyEvent.VK_C); // respond to Alt+C // key event cannot be resolved

          closeButton.addActionListener(new java.awt.event.ActionListener() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent e) {
              updateStateFromFields();
              frame.setVisible(false);
            }
          });

          c.gridwidth = 1;
          c.weighty = 0.0;
          c.anchor = GridBagConstraints.SOUTHEAST;
          c.gridwidth = 2; // use the whole row
          c.gridx = 0;
          c.weightx = 0.0;

          JPanel buttonRow = new JPanel();
          buttonRow.add(applyButton);
          buttonRow.add(closeButton);
          if (backgroundColor != null) {
            applyButton.setBackground(backgroundColor);
            closeButton.setBackground(backgroundColor);
            buttonRow.setBackground(backgroundColor);
          }
          pane.add(buttonRow, c);
        }

        // move window
        Point p = GameModule.getGameModule().getFrame().getLocation();
        if (getMap() != null) {
          p = getMap().getView().getLocationOnScreen();
          Point p2 = getMap().mapToComponent(getPosition());
          p.translate(p2.x, p2.y);
        }
        frame.setLocation(p.x, p.y);

        // watch for window closing - save state
        frame.addWindowListener(new WindowAdapter() {
          @Override
          public void windowClosing(WindowEvent evt) {
            updateStateFromFields();
          }
        });
        frame.pack();
      }

      // Name window and make it visible
      frame.setTitle(getLocalizedName());
      oldState = Decorator.getOutermost(this).getState();
      frame.setVisible(true);
      return null;
    }
    else {
      return null;
    }
  }

  @Override
  public Object getLocalizedProperty(Object key) {
    final Object value = properties.get(key);
    return value == null ? super.getLocalizedProperty(key) : value;
  }

  @Override
  public Object getProperty(Object key) {
    final Object value = properties.get(key);
    return value == null ? super.getProperty(key) : value;
  }

  @Override
  public String getDescription() {
    return "Property Sheet";
  }

  @Override
  public VASSAL.build.module.documentation.HelpFile getHelpFile() {
    return HelpFile.getReferenceManualPage("PropertySheet.htm");
  }

  @Override
  public PieceEditor getEditor() {
    return new Ed(this);
  }

  /**
   * A generic panel for editing unit traits.
   * Not directly related to "PropertySheets".
   */
  static class PropertyPanel extends JPanel implements FocusListener {
    private static final long serialVersionUID = 1L;

    GridBagConstraints c = new GridBagConstraints();

    public PropertyPanel() {
      super(new GridBagLayout());
      c.insets = new Insets(0, 4, 0, 4);
      //c.ipadx = 5;
      c.anchor = GridBagConstraints.WEST;
    }

    public NamedHotKeyConfigurer addKeyStrokeConfig(NamedKeyStroke value) {
      ++c.gridy;
      c.gridx = 0;
      c.weightx = 0.0;
      c.fill = GridBagConstraints.NONE;
      add(new JLabel("Keystroke:"), c);

      ++c.gridx;
      final NamedHotKeyConfigurer config = new NamedHotKeyConfigurer(null, "", value);
      final Component field = config.getControls();
      add(field, c);

      return config;
    }

    public JTextField addStringCtrl(String name, String value) {
      ++c.gridy;
      c.gridx = 0;
      c.weightx = 0.0;
      c.fill = GridBagConstraints.HORIZONTAL;
      add(new JLabel(name), c);

      c.weightx = 1.0;
      JTextField field = new JTextField(value);
      ++c.gridx;
      add(field, c);
      return field;
    }

    public JButton addColorCtrl(String name, Color value) {
      ++c.gridy;
      c.gridx = 0;
      c.weightx = 0.0;
      c.fill = GridBagConstraints.NONE;
      add(new JLabel(name), c);

      c.weightx = 0.0;
      JButton button = new JButton("Default");

      if (value != null) {
        button.setBackground(value);
        button.setText("sample");
      }
      button.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent event) {
          JButton button = (JButton) event.getSource();
          Color value = button.getBackground();
          Color newColor = JColorChooser.showDialog(PropertyPanel.this, "Choose background color or CANCEL to use default color scheme", value);
          if (newColor != null) {
            button.setBackground(newColor);
            button.setText("sample");
          }
          else {
            button.setBackground(PropertyPanel.this.getBackground());
            button.setText("Default");
          }
        }
      });
      ++c.gridx;
      add(button, c);
      return button;
    }

    public JComboBox addComboBox(String name, String[] values, int initialRow) {
      JComboBox<String> comboBox = new JComboBox<>(values);
      comboBox.setEditable(false);
      comboBox.setSelectedIndex(initialRow);

      addCtrl(name, comboBox);

      return comboBox;
    }


    public void addCtrl(String name, JComponent ctrl) {
      ++c.gridy;
      c.gridx = 0;
      c.weightx = 0.0;
      c.fill = GridBagConstraints.NONE;
      add(new JLabel(name), c);

      c.weightx = 0.0;

      ++c.gridx;
      add(ctrl, c);
    }

    DefaultTableModel tableModel;
    String[] defaultValues;

    public static class SmartTable extends JTable {
      private static final long serialVersionUID = 1L;

      SmartTable(TableModel m) {
        super(m);
        setSurrendersFocusOnKeystroke(true);
      }

      //Prepares the editor by querying the data model for the value and selection state of the cell at row, column.    }
      @Override
      public Component prepareEditor(TableCellEditor editor, int row, int column) {
        if (row == getRowCount() - 1) {
          ((DefaultTableModel) getModel()).addRow(Ed.DEFAULT_ROW);
        }
        Component component = super.prepareEditor(editor, row, column);

        if (component instanceof JTextComponent) {
          ((JTextComponent) component).grabFocus();
          ((JTextComponent) component).selectAll();
        }
        return component;
      }
    }

    public JTable addTableCtrl(String name, DefaultTableModel theTableModel, String[] theDefaultValues) {
      tableModel = theTableModel;
      this.defaultValues = theDefaultValues;

      ++c.gridy;
      c.gridx = 0;
      c.weighty = 0.0;
      c.weightx = 1.0;
      c.gridwidth = 2;
      c.fill = GridBagConstraints.HORIZONTAL;
      add(new JLabel(name), c);

      ++c.gridy;
      c.weighty = 1.0;
      c.fill = GridBagConstraints.BOTH;
      final SmartTable table = new SmartTable(tableModel);
      add(new ScrollPane(table), c);

      c.gridwidth = 1;

      ++c.gridy;
      c.fill = GridBagConstraints.NONE;
      c.anchor = GridBagConstraints.EAST;
      c.gridwidth = 2;
      c.weighty = 0.0;
      c.weightx = 1.0;

      JPanel buttonPanel = new JPanel();

      // add button
      JButton addButton = new JButton("Insert Row");
      buttonPanel.add(addButton);
      addButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {

          if (table.isEditing()) {
            table.getCellEditor().stopCellEditing();
          }

          ListSelectionModel selection = table.getSelectionModel();
          int iSelection;
          if (selection.isSelectionEmpty()) {
            tableModel.addRow(defaultValues);
            iSelection = tableModel.getRowCount() - 1;
          }
          else {
            iSelection = selection.getMaxSelectionIndex();
            tableModel.insertRow(iSelection, defaultValues);
          }
          tableModel.fireTableDataChanged(); // BING BING BING
          selection.setSelectionInterval(iSelection, iSelection);
          table.grabFocus();
          table.editCellAt(iSelection, 0);
          Component comp = table.getCellEditor().getTableCellEditorComponent(table, null, true, iSelection, 0);
          if (comp instanceof JComponent) {
            ((JComponent) comp).grabFocus();
          }
        }
      });

      // delete button
      final JButton deleteButton = new JButton("Delete Row");
      deleteButton.setEnabled(false);
      buttonPanel.add(deleteButton);
      deleteButton.addActionListener(new ActionListener() {
        @Override
        public void actionPerformed(ActionEvent e) {

          if (table.isEditing()) {
            table.getCellEditor().stopCellEditing();
          }

          ListSelectionModel selection = table.getSelectionModel();
          for (int i = selection.getMaxSelectionIndex(); i >= selection.getMinSelectionIndex(); --i) {
            if (selection.isSelectedIndex(i)) {
              tableModel.removeRow(i);
            }
          }
          tableModel.fireTableDataChanged(); // BING BING BING
        }
      });

      // Ask to be notified of selection changes.
      table.getSelectionModel().addListSelectionListener(new ListSelectionListener() {
        @Override
        public void valueChanged(ListSelectionEvent event) {
          //Ignore extra messages.
          if (!event.getValueIsAdjusting()) {
            ListSelectionModel lsm = (ListSelectionModel) event.getSource();
            deleteButton.setEnabled(!lsm.isSelectionEmpty());
          }
        }
      });

      add(buttonPanel, c);

      c.anchor = GridBagConstraints.WEST;
      c.weightx = 0.0;
      return table;
    }

    @Override
    public void focusGained(FocusEvent event) {
    }

    // make sure we save user's changes
    @Override
    public void focusLost(FocusEvent event) {
      if (event.getComponent() instanceof JTable) {
        JTable table = (JTable) event.getComponent();
        if (table.isEditing()) {
          table.getCellEditor().stopCellEditing();
        }
      }
    }
  }

  @Override
  public PieceI18nData getI18nData() {
    final ArrayList<String> items = new ArrayList<>();
    final SequenceEncoder.Decoder defDecoder =
      new SequenceEncoder.Decoder(m_definition, DEF_DELIMITOR);
    while (defDecoder.hasMoreTokens()) {
      final String item = defDecoder.nextToken();
      items.add(item.length() == 0 ? "" : item.substring(1));
    }

    final String[] menuNames = new String[items.size()+1];
    final String[] descriptions = new String[items.size()+1];
    menuNames[0]  = menuName;
    descriptions[0] = "Property Sheet command";
    int j = 1;
    for (String s : items) {
      menuNames[j] = s;
      descriptions[j] = "Property Sheet item " + j;
      j++;
    }
    return getI18nData(menuNames, descriptions);
  }

  /**
   * Return Property names exposed by this trait
   */
  @Override
  public List<String> getPropertyNames() {
    ArrayList<String> l = new ArrayList<>();
    for (String prop : properties.keySet()) {
      l.add(prop);
    }
    return l;
  }

  private static class Ed implements PieceEditor {

    private PropertyPanel m_panel;
    private JTextField menuNameCtrl;
    private NamedHotKeyConfigurer keyStrokeConfig;
    private JButton colorCtrl;
    private JTable propertyTable;
    private JComboBox commitCtrl;

    static final String[] COLUMN_NAMES = {"Name", "Type"};
    static final String[] DEFAULT_ROW = {"*new property*", "Text"};

    public Ed(PropertySheet propertySheet) {
      m_panel = new PropertyPanel();
      menuNameCtrl = m_panel.addStringCtrl("Menu Text:", propertySheet.menuName);
      keyStrokeConfig = m_panel.addKeyStrokeConfig(propertySheet.launchKeyStroke);
      commitCtrl = m_panel.addComboBox("Commit changes on:", COMMIT_VALUES, propertySheet.commitStyle);
      colorCtrl = m_panel.addColorCtrl("Background Color:", propertySheet.backgroundColor);
      DefaultTableModel dataModel = new DefaultTableModel(getTableData(propertySheet.m_definition), COLUMN_NAMES);
      AddCreateRow(dataModel);
      propertyTable = m_panel.addTableCtrl("Properties:", dataModel, DEFAULT_ROW);

      DefaultCellEditor typePicklist = new DefaultCellEditor(new JComboBox(TYPE_VALUES));
      propertyTable.getColumnModel().getColumn(1).setCellEditor(typePicklist);
    }

    protected void AddCreateRow(DefaultTableModel data) {
      data.addRow(DEFAULT_ROW);
    }

    protected String[][] getTableData(String definition) {
      SequenceEncoder.Decoder decoder = new SequenceEncoder.Decoder(definition, DEF_DELIMITOR);

      int numRows = !definition.equals("") && decoder.hasMoreTokens() ? 1 : 0;
      for (int iDef = -1; (iDef = definition.indexOf(DEF_DELIMITOR, iDef + 1)) >= 0;) {
        ++numRows;
      }

      String[][] rows = new String[numRows][2];

      for (int iRow = 0; decoder.hasMoreTokens() && iRow < numRows; ++iRow) {
        String token = decoder.nextToken();
        rows[iRow][0] = token.substring(1);
        rows[iRow][1] = TYPE_VALUES[token.charAt(0) - '0'];
      }

      return rows;
    }

    @Override
    public Component getControls() {
      return m_panel;
    }

    /** returns the type-definition in the format:
     definition, name, keystroke, commit, red, green, blue
     */
    @Override
    public String getType() {

      if (propertyTable.isEditing()) {
        propertyTable.getCellEditor().stopCellEditing();
      }

      SequenceEncoder defEncoder = new SequenceEncoder(DEF_DELIMITOR);
//      StringBuilder definition = new StringBuilder();
//      int numRows = propertyTable.getRowCount();
      for (int iRow = 0; iRow < propertyTable.getRowCount(); ++iRow) {
        String typeString = (String) propertyTable.getValueAt(iRow, 1);
        for (int iType = 0; iType < TYPE_VALUES.length; ++iType) {
          if (typeString.matches(TYPE_VALUES[iType]) &&
              !DEFAULT_ROW[0].equals(propertyTable.getValueAt(iRow, 0))) {
            defEncoder.append("" + iType + propertyTable.getValueAt(iRow, 0));
            break;
          }
        }
      }

      SequenceEncoder typeEncoder = new SequenceEncoder(TYPE_DELIMITOR);

      // calc color strings
      String red, green, blue;
      if (colorCtrl.getText().equals("Default")) {
        red = "";
        green = "";
        blue = "";
      }
      else {
        red = Integer.toString(colorCtrl.getBackground().getRed());
        green = Integer.toString(colorCtrl.getBackground().getGreen());
        blue = Integer.toString(colorCtrl.getBackground().getBlue());
      }

      String definitionString = defEncoder.getValue();
      typeEncoder.append(definitionString == null ? "" : definitionString).
          append(menuNameCtrl.getText()).
          append("").
          append(Integer.toString(commitCtrl.getSelectedIndex())).
          append(red).append(green).append(blue).append(keyStrokeConfig.getValueString());

      return ID + typeEncoder.getValue();
    }

    /** returns a default value-string for the given definition */
    @Override
    public String getState() {
      final StringBuilder buf = new StringBuilder();
      for (int i = 0; i < propertyTable.getRowCount(); ++i) {
        buf.append(STATE_DELIMITOR);
      }

      return buf.toString();
    }
  }

  class TickPanel extends JPanel implements ActionListener, FocusListener, DocumentListener {
    private static final long serialVersionUID = 1L;

    private int numTicks;
    private int maxTicks;
    private int panelType;
    private JTextField valField;
    private JTextField maxField;
    private TickLabel ticks;
    private List<ActionListener> actionListeners =
      new ArrayList<>();
    private List<DocumentListener> documentListeners =
      new ArrayList<>();

    public TickPanel(String value, int type) {
      super(new GridBagLayout());
      set(value);

      panelType = type;

      GridBagConstraints c = new GridBagConstraints();
      c.fill = GridBagConstraints.BOTH;
      c.ipadx = 1;
      c.weightx = 0.0;
      c.gridx = 0;
      c.gridy = 0;

      Dimension minSize;

      if (panelType == TICKS_VAL || panelType == TICKS_VALMAX) {
        valField = new JTextField(Integer.toString(numTicks));
        minSize = valField.getMinimumSize();
        minSize.width = 24;
        valField.setMinimumSize(minSize);
        valField.setPreferredSize(minSize);
        valField.addActionListener(this);
        valField.addFocusListener(this);
        add(valField, c);
        ++c.gridx;
      }
      if (panelType == TICKS_MAX || panelType == TICKS_VALMAX) {
        maxField = new JTextField(Integer.toString(maxTicks));
        minSize = maxField.getMinimumSize();
        minSize.width = 24;
        maxField.setMinimumSize(minSize);
        maxField.setPreferredSize(minSize);
        maxField.addActionListener(this);
        maxField.addFocusListener(this);
        if (panelType == TICKS_VALMAX) {
          add(new JLabel("/"), c);
          ++c.gridx;
        }
        add(maxField, c);
        ++c.gridx;
      }

      ticks = new TickLabel(numTicks, maxTicks, panelType);
      ticks.addActionListener(this);
      c.weightx = 1.0;
      add(ticks, c);
      doLayout();
    }

    public void updateValue(String value) {
      set(value);
      updateFields();
    }

    private void set(String value) {
      set(atoi(value), atoiRight(value));
    }

    private boolean set(int num, int max) {

      boolean changed = false;

      if (numTicks == 0 && maxTicks == 0 && num == 0 && max > 0) {
//        num = max;  // This causes a bug in which ticks set to zero go to max after save/reload of a game
        changed = true;
      }
      if (numTicks == 0 && maxTicks == 0 && max == 0 && num > 0) {
        max = num;
        changed = true;
      }
      numTicks = Math.min(max, num);
      maxTicks = max;

      return changed;
    }

    public String getValue() {
      commitTextFields();
      return Integer.toString(numTicks) + VALUE_DELIMINATOR + maxTicks;
    }

    private void commitTextFields() {
      if (valField != null || maxField != null) {
        if (set(valField != null ? atoi(valField.getText()) : numTicks,
                maxField != null ? atoi(maxField.getText()) : maxTicks)) {
          updateFields();
        }
      }
    }

    private void updateFields() {
      if (valField != null) {
        valField.setText(Integer.toString(numTicks));
      }
      if (maxField != null) {
        maxField.setText(Integer.toString(maxTicks));
      }
      ticks.set(numTicks, maxTicks);
    }

    private boolean areFieldValuesValid() {
      int max = maxField == null ? maxTicks : atoi(maxField.getText());
      int val = valField == null ? numTicks : atoi(valField.getText());
      return val < max && val >= 0;
    }

    // field changed
    @Override
    public void actionPerformed(ActionEvent event) {

      if (event.getSource() == maxField || event.getSource() == valField) {
        commitTextFields();
        ticks.set(numTicks, maxTicks);
        fireActionEvent();
      }
      else if (event.getSource() == ticks) {
        commitTextFields();
        numTicks = ticks.getNumTicks();
        if (maxField == null) {
          maxTicks = ticks.getMaxTicks();
        }
        updateFields();
        fireDocumentEvent();
      }
    }

    public void addActionListener(ActionListener listener) {
      actionListeners.add(listener);
    }

    public void fireActionEvent() {
      for (ActionListener l : actionListeners) {
        l.actionPerformed(new ActionEvent(this, 0, null));
      }
    }

    void addDocumentListener(DocumentListener listener) {
      if (valField != null)
        valField.getDocument().addDocumentListener(this);
      if (maxField != null)
        maxField.getDocument().addDocumentListener(this);
      documentListeners.add(listener);
    }

    public void fireDocumentEvent() {
      for (DocumentListener l : documentListeners) {
        l.changedUpdate(null);
      }
    }

    // FocusListener Interface
    @Override
    public void focusLost(FocusEvent event) {
      commitTextFields();
      ticks.set(numTicks, maxTicks);
    }

    @Override
    public void focusGained(FocusEvent event) {
    }

    @Override
    public void changedUpdate(DocumentEvent event) {
      // do not propagate events unless min/max is valid.  We don't want to trigger both
      // a state update. A state update might trigger a fix-min/max operation, which would
      // cause the text fields to update which will throw an IllegalStateException
      if (areFieldValuesValid()) {
        fireDocumentEvent();
      }
    }

    @Override
    public void insertUpdate(DocumentEvent event) {
      changedUpdate(event);
    }

    @Override
    public void removeUpdate(DocumentEvent event) {
      changedUpdate(event);
    }
  }

  class TickLabel extends JLabel implements MouseListener {
    private static final long serialVersionUID = 1L;

    private int numTicks = 0;
    private int maxTicks = 0;
    protected int panelType;
    private List<ActionListener> actionListeners =
      new ArrayList<>();

    public int getNumTicks() {
      return numTicks;
    }

    public int getMaxTicks() {
      return maxTicks;
    }

    public void addActionListener(ActionListener listener) {
      actionListeners.add(listener);
    }

    public TickLabel(int numTicks, int maxTicks, int panelType) {
      super(" ");
      //Debug.trace("TickLabel( " + maxTicks + ", " + numTicks + " )");
      set(numTicks, maxTicks);
      this.panelType = panelType;
      addMouseListener(this);
    }

    protected int topMargin = 2;
    protected int leftMargin = 2;
    protected int numRows;
    protected int numCols;
    protected int dx = 1;

    @Override
    public void paint(Graphics g) {
      //Debug.trace("TickLabcmdel.paint(" + numTicks + "/" + maxTicks + ")");
      //Debug.trace("  width=" + getWidth() + " height=" + getHeight());
      if (maxTicks <= 0) {
        return;
      }

      // prefered width is 10
      // min width before resize is 6
      int displayWidth = getWidth() - 2;

      dx = Math.min(displayWidth / maxTicks, 10);
      // if dx < 2, we have a problem

      numRows = dx < 3 ? 3 : dx < 6 ? 2 : 1;

      dx = Math.min(displayWidth * numRows / maxTicks, Math.min(getHeight() / numRows, 10));
      if (dx < 1) dx = 1;

      numCols = (maxTicks + numRows - 1) / numRows;

      int dy = dx;

      topMargin = (getHeight() - dy * numRows + 2) / 2;

      int tick = 0;
      int row, col;
      if (dx > 4) {
        for (; tick < maxTicks; ++tick) {
          row = tick / numCols;
          col = tick % numCols;
          g.setColor(Color.BLACK);
          g.drawRect(leftMargin + col * dx, topMargin + row * dy, dx - 3, dy - 3);
          g.setColor(tick < numTicks ? Color.BLACK : Color.WHITE);
          g.fillRect(leftMargin + 1 + col * dx, topMargin + 1 + row * dy, dx - 4, dy - 4);
        }
      }
      else {
        g.setColor(Color.GRAY);
        g.fillRect(0, topMargin - 2, numCols * dx + leftMargin * 2, numRows * dy + 4);
        for (; tick < maxTicks; ++tick) {
          row = tick / numCols;
          col = tick % numCols;
          g.setColor(tick < numTicks ? Color.BLACK : Color.WHITE);
          g.fillRect(leftMargin + col * dx, topMargin + row * dy, dx - 1, dy - 1);
        }
      }
    }

    @Override
    public void mouseClicked(MouseEvent e) {
      if (panelType != TICKS_VALMAX &&
          ((SwingUtils.isLeftMouseButton(e) && e.isShiftDown()) ||
            SwingUtils.isRightMouseButton(e))) {
        new EditTickLabelValueDialog(this);
      }
      else if (SwingUtils.isLeftMouseButton(e)) {
        int col = Math.min((e.getX() - leftMargin + 1) / dx, numCols - 1);
        int row = Math.min((e.getY() - topMargin + 1) / dx, numRows - 1);
        int num = row * numCols + col + 1;

        // Checkbox behavior; toggle the box clicked on
        // numTicks = num > numTicks ? num : num - 1;

        // Slider behavior; set the box clicked on and all to left and clear all boxes to right,
        // UNLESS user clicked on last set box (which would do nothing), in this case, toggle the
        // box clicked on.  This is the only way the user can clear the first box.

        numTicks = (num == numTicks) ? num - 1 : num;
        fireActionEvent();

        repaint();
      }
    }

    public void fireActionEvent() {
      for (ActionListener l : actionListeners) {
        l.actionPerformed(new ActionEvent(this, 0, null));
      }
    }

    public void set(int newNumTicks, int newMaxTicks) {
      //Debug.trace("TickLabel.set( " + newNumTicks + "," + newMaxTicks + " ) was " + numTicks + "/" + maxTicks);
      numTicks = newNumTicks;
      maxTicks = newMaxTicks;

      String tip = numTicks + "/" + maxTicks;

      if (panelType != TICKS_VALMAX) {
        tip += " (right-click to edit)";
      }

      setToolTipText(tip);

      repaint();
    }

    @Override
    public void mouseEntered(MouseEvent event) {
    }

    @Override
    public void mouseExited(MouseEvent event) {
    }

    @Override
    public void mousePressed(MouseEvent event) {
    }

    @Override
    public void mouseReleased(MouseEvent event) {
    }

    public class EditTickLabelValueDialog extends JPanel implements ActionListener, DocumentListener, FocusListener {
      private static final long serialVersionUID = 1L;

      TickLabel theTickLabel;
      JLayeredPane editorParent;
      JTextField valueField;

      public EditTickLabelValueDialog(TickLabel owner) {
        super(new BorderLayout());

        theTickLabel = owner;

        // Find containing dialog
        Container theDialog = theTickLabel.getParent();
        while (!(theDialog instanceof JDialog) && theDialog != null) {
          theDialog = theDialog.getParent();
        }

        if (theDialog != null) {
          editorParent = ((JDialog) theDialog).getLayeredPane();
          Rectangle newBounds = SwingUtilities.convertRectangle(theTickLabel.getParent(), theTickLabel.getBounds(), editorParent);
          setBounds(newBounds);

          JButton okButton = new JButton("Ok");

          switch (panelType) {
          case TICKS_VAL:
            valueField = new JTextField(Integer.toString(owner.getMaxTicks()));
            valueField.setToolTipText("max value");
            break;
          case TICKS_MAX:
            valueField = new JTextField(Integer.toString(owner.getNumTicks()));
            valueField.setToolTipText("current value");
            break;
          case TICKS:
          default:
            valueField = new JTextField(owner.numTicks + "/" + owner.maxTicks);
            valueField.setToolTipText("current value / max value");
            break;
          }

          valueField.addActionListener(this);
          valueField.addFocusListener(this);
          valueField.getDocument().addDocumentListener(this);
          add(valueField, BorderLayout.CENTER);

          okButton.addActionListener(this);
          add(okButton, BorderLayout.EAST);

          editorParent.add(this, JLayeredPane.MODAL_LAYER);

          setVisible(true);
          valueField.grabFocus();
        }
      }

      public void storeValues() {
        String value = valueField.getText();
        switch (panelType) {
        case TICKS_VAL:
          theTickLabel.set(theTickLabel.getNumTicks(), atoi(value));
          break;
        case TICKS_MAX:
          theTickLabel.set(atoi(value), theTickLabel.getMaxTicks());
          break;
        case TICKS:
        default:
          theTickLabel.set(atoi(value), atoiRight(value));
          break;
        }
        theTickLabel.fireActionEvent();
      }

      @Override
      public void actionPerformed(ActionEvent event) {
        storeValues();
        editorParent.remove(this);
      }

      @Override
      public void changedUpdate(DocumentEvent event) {
        theTickLabel.fireActionEvent();
      }

      @Override
      public void insertUpdate(DocumentEvent event) {
        theTickLabel.fireActionEvent();
      }

      @Override
      public void removeUpdate(DocumentEvent event) {
        theTickLabel.fireActionEvent();
      }

      @Override
      public void focusGained(FocusEvent event) {
      }

      @Override
      public void focusLost(FocusEvent event) {
        storeValues();
      }
    }
  }
}
