/*
 * $Id$
 *
 * Copyright (c) 2005 by Rodney Kinney, Brent Easton
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Library General Public License (LGPL) as published by
 * the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
 * details.
 *
 * You should have received a copy of the GNU Library General Public License
 * along with this library; if not, copies are available at
 * http://www.opensource.org.
 */

package VASSAL.build.module.gamepieceimage;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;

import VASSAL.configure.Configurer;
import VASSAL.configure.IntConfigurer;
import VASSAL.configure.StringConfigurer;
import VASSAL.tools.ScrollPane;

public class LayoutConfigurer extends Configurer {

  protected static final String ADD_SYMBOL = "Symbol";
  protected static final String ADD_IMAGE = "Image";
  protected static final String ADD_TEXT = "Label";
  protected static final String ADD_TEXTBOX = "Text Box";
  protected static final String ADD_SHAPE = "Shape";
  protected static final String REMOVE = "Remove";
  protected static final String UP = "Up";
  protected static final String DOWN = "Down";
  protected static final int NO_CURRENT_ITEM = -1;

  protected JPanel panel;
  protected ItemPanel itemPanel;
  protected JPanel itemConfigPanel;
  protected Component currentItemControls;
  protected int currentItem = NO_CURRENT_ITEM;
  protected Box visBox;
  protected Visualizer visualizer = new Visualizer();
  protected JLabel visLabel;
  protected Box filler;
  protected GamePieceLayout layout;

  protected StringConfigurer defName;
  protected NewIntConfigurer height, width;

  protected LayoutConfigurer() {
    super(null, null);
  }

  protected LayoutConfigurer(String key, String name, GamePieceLayout def) {
    super(key, name);
    layout = def;
  }

  public Object getValue() {
    if (layout != null) {

      layout.setConfigureName(defName.getValueString());
      layout.setHeight(((Integer) height.getValue()).intValue());
      layout.setWidth(((Integer) width.getValue()).intValue());
    }
    return layout;
  }

  public void setValue(String s) {
    if (itemPanel != null) {
      itemPanel.reshow();
    }
  }

  public Component getControls() {
    if (panel == null) {

      panel = new JPanel();
      panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      visBox = Box.createHorizontalBox();
      visBox.setAlignmentX(Box.CENTER_ALIGNMENT);
      visualizer = new Visualizer(layout);
      visBox.add(new ScrollPane(visualizer));
      panel.add(visBox);

      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      itemPanel = new ItemPanel();
      panel.add(itemPanel);

      filler = Box.createHorizontalBox();
      filler.setPreferredSize(new Dimension(50, 10));
      panel.add(filler);

      Window w = SwingUtilities.getWindowAncestor(itemPanel);
      if (w != null) {
        w.pack();
      }
    }

    return panel;
  }

  public String getValueString() {

    return null;
  }

  protected void repack() {

    Window w = SwingUtilities.getWindowAncestor(panel);
    if (w != null) {
      w.pack();
    }
    if (visualizer != null) {
      visualizer.rebuild();
    }
  }

  protected class ItemPanel extends JPanel implements ActionListener {
    private static final long serialVersionUID = 1L;

    protected JTable table;
    protected AbstractTableModel model;
    protected JScrollPane scrollPane;
    protected JButton addSymbolBtn, addTextBtn, addTextBoxBtn, addImageBtn, addShapeBtn, remBtn, upBtn, dnBtn;
    protected JPanel mainPanel;

    public ItemPanel() {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));

      mainPanel = new JPanel();
      mainPanel.setBorder(BorderFactory.createLineBorder(Color.black));
      mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

      Box box = Box.createHorizontalBox();
      box.add(new JLabel("Items"));
      mainPanel.add(box);

      model = new MyTableModel();
      table = new JTable(model);
      table.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
      if (layout.getItemCount() > 0) {
        table.getSelectionModel().setSelectionInterval(0, 0);
      }
      ListSelectionModel rowSM = table.getSelectionModel();
      rowSM.addListSelectionListener(new ListSelectionListener() {
        public void valueChanged(ListSelectionEvent e) {
          if (e.getValueIsAdjusting()) return;

          ListSelectionModel lsm = (ListSelectionModel) e.getSource();
          if (lsm.isSelectionEmpty()) {
            showItem(NO_CURRENT_ITEM);
          }
          else {
            int selectedRow = lsm.getMinSelectionIndex();
            showItem(selectedRow);
          }
        }
      });

      scrollPane = new ScrollPane(table);
      table.setPreferredScrollableViewportSize(new Dimension(300, 100));
      mainPanel.add(scrollPane);

      box = Box.createHorizontalBox();
      addSymbolBtn = new JButton(ADD_SYMBOL);
      addSymbolBtn.setToolTipText("Add a Symbol to the Layout");
      addSymbolBtn.addActionListener(this);
      box.add(addSymbolBtn);
      addTextBtn = new JButton(ADD_TEXT);
      addTextBtn.setToolTipText("Add Text to the Layout");
      addTextBtn.addActionListener(this);
      box.add(addTextBtn);
      addTextBoxBtn = new JButton(ADD_TEXTBOX);
      addTextBoxBtn.setToolTipText("Add Text Box to the Layout");
      addTextBoxBtn.addActionListener(this);
      box.add(addTextBoxBtn);
      addImageBtn = new JButton(ADD_IMAGE);
      addImageBtn.setToolTipText("Add an Image to the Layout");
      addImageBtn.addActionListener(this);
      box.add(addImageBtn);
      addShapeBtn = new JButton(ADD_SHAPE);
      addShapeBtn.setToolTipText("Add a Colored Shape to the Layout");
      addShapeBtn.addActionListener(this);
      box.add(addShapeBtn);
      mainPanel.add(box);

      box = Box.createHorizontalBox();
      remBtn = new JButton(REMOVE);
      remBtn.setToolTipText("Remove the selected Item");
      remBtn.addActionListener(this);
      box.add(remBtn);
      upBtn = new JButton(UP);
      upBtn.setToolTipText("Move the selected Item up the list (draw earlier)");
      upBtn.addActionListener(this);
      box.add(upBtn);
      dnBtn = new JButton(DOWN);
      dnBtn.setToolTipText("Move the selected Item down the list (draw later)");
      dnBtn.addActionListener(this);
      box.add(dnBtn);
      mainPanel.add(box);
      add(mainPanel);

      box = Box.createHorizontalBox();
      box.setPreferredSize(new Dimension(50, 10));
      add(box);

      itemConfigPanel = new JPanel();
      itemConfigPanel.setBorder(BorderFactory.createLineBorder(Color.black));
      add(new ScrollPane(itemConfigPanel));

      showItem(0);

    }

    public void actionPerformed(ActionEvent e) {
      String action = e.getActionCommand();
      int pos = layout.getItemCount();
      int sel = table.getSelectedRow();

      if (action.equals(ADD_SYMBOL)) {
        addItem(new SymbolItem(layout, "Symbol"+pos)); //$NON-NLS-1$
      }
      else if (action.equals(ADD_TEXT)) {
        TextItem item = new TextItem(layout, "Text"+pos); //$NON-NLS-1$
        addItem(item);
      }
      else if (action.equals(ADD_TEXTBOX)) {
        TextBoxItem item = new TextBoxItem(layout, "TextBox"+pos); //$NON-NLS-1$
        addItem(item);
      }
      else if (action.equals(ADD_IMAGE)) {
        addItem(new ImageItem(layout, "Image"+pos)); //$NON-NLS-1$
      }
      else if (action.equals(ADD_SHAPE)) {
        addItem(new ShapeItem(layout, "Shape"+pos)); //$NON-NLS-1$
      }
      else if (action.equals(REMOVE)) {
        if (sel >= 0) {
          layout.removeItem(sel);
          model.fireTableRowsDeleted(sel, sel);
        }
        if (layout.getItemCount() > 1) {
          if (sel >= layout.getItemCount()) {
            table.getSelectionModel().setSelectionInterval(layout.getItemCount() - 1, layout.getItemCount() - 1);
          }
          else {
            table.getSelectionModel().setSelectionInterval(sel, sel);
          }
        }
      }
      else if (action.equals(UP)) {
        if (sel > 0) {
          moveItem(sel, sel-1);
        }
      }
      else if (action.equals(DOWN)) {
        if (sel < pos-1) {
          moveItem(sel, sel+1);
        }

      }

      rebuildViz();
    }

    protected void addItem(Item item) {
      layout.addItem(item);
      int pos = layout.getItemCount() - 1;
      model.fireTableRowsInserted(pos, pos);
      table.getSelectionModel().setSelectionInterval(pos, pos);
    }

    protected void moveItem(int from, int to) {
      layout.moveItem(from, to);
      model.fireTableRowsUpdated(from, to);
      table.getSelectionModel().setSelectionInterval(to, to);
      rebuildViz();
    }

    protected void rebuildViz() {
      layout.setImageDefn(new GamePieceImage(layout));
      visualizer.rebuild();
    }

    protected void showItem(int itemNo) {

      if (currentItemControls != null) {
        itemConfigPanel.remove(currentItemControls);
        currentItemControls = null;
        currentItem = NO_CURRENT_ITEM;
      }

      if (itemNo != NO_CURRENT_ITEM && layout.getItemCount() > 0 && itemNo < layout.getItemCount()) {
        Item item = layout.getItem(itemNo);
        Configurer c = item.getConfigurer();
        currentItemControls = c.getControls();
        itemConfigPanel.add(currentItemControls);
        currentItem = itemNo;
      }

      reshow();
    }

    public void reshow() {

      repack();
      rebuildViz();
      itemConfigPanel.repaint();

    }

    class MyTableModel extends AbstractTableModel {
      private static final long serialVersionUID = 1L;

      private String[] columnNames = new String[] { "Name", "Type", "Position" };

      public int getColumnCount() {
        return columnNames.length;
      }

      public int getRowCount() {
        return layout.getItemCount();
      }

      public String getColumnName(int col) {
        return columnNames[col];
      }

      public Object getValueAt(int row, int col) {
        if (col == 0) {
          return (layout.getItem(row)).getConfigureName();
        }
        else if (col == 1) {
          return (layout.getItem(row)).getDisplayName();
        }
        else if (col == 2) {
          return (layout.getItem(row)).getLocation();
        }
        else
          return null;
      }

      public Class<String> getColumnClass(int c) {
        return String.class;
      }
    }
  }

  protected class NewIntConfigurer extends IntConfigurer {

    NewIntConfigurer(String name, String key, Integer i) {
      super(name, key, i);
    }

    public void setColumns(int cols) {
      nameField.setColumns(cols);
    }

    public int getIntValue() {
      return ((Integer) getValue()).intValue();
    }

  }
}
